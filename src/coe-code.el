;;; coe-code.el --- Edit COE code.org files         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zainab Ali

;; Author: Zainab Ali <zainab@kebab-ca.se>
;; Keywords: tools, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; COE is the abbreviation for "Craft of Emacs", the working name for the Elisp
;; tutorial book. The tutorial code snippets are stored in a code.org file in
;; each tutorial directory (one tutorial has one directory). These are not
;; general purpose org files - they only contain headings and code snippets
;; which are referenced in the book (.pm) files.
;;
;; This minor mode is intended for editing such files. It eases snippet writing
;; by creating snippets off previous ones. It has an overlay feature for
;; highlighting diff regions. These are stored in a seperate diff.el file that
;; can be exported to the (.pm) when the snippets are complete.

;; Local Variables:
;; nameless-current-name: "coe-code"
;; column-enforce-column: 80
;; End:

;; TODO - the overlays are currently stored within the buffer, such that the
;; buffer represents a single source of truth. Unfortunately, the addition of
;; the surround type means that there isn't a one to one correspondence between
;; a diff and an overlay. surround diffs can be nested on top of each other and
;; still be consistent, but nested surround overlays appear invisible. We should
;; instead have two overlays per surround - each on the start and end
;; parentheses. We should have a buffer-local map of diff to overlay such that
;; a surround overlay can still be deleted. We should get rid of overlay
;; priority once done.

;;; Code:

;; Construction

;; Editing

(defun coe-code-step (headline)
  "Insert a new headline and src block at the end of the file.

This validates the existing blocks and copies the last block in the
file below the heading.

This means the user can add their edits working from the previous
block."
  (interactive "sHeadline: ")
  (save-excursion)
  (indent-region (point-min) (point-max))
  (coe-code--column-length-check)
  (goto-char (point-max))
  (unless (string-empty-p (string-trim (thing-at-point 'line)))
    (newline))
  (insert (format "* %s\n" headline))
  (coe-code--src-copy)
  (indent-region (point-min) (point-max))
  (goto-char (point-max)))

(defun coe-code--column-length-check ()
  "Enforces blocks to be 70 columns long.

A warning is printed when a line exceeds this length.
Note that since src blocks are indented by 4, we check 74
columns instead."
  (org-element-map
      (org-element-parse-buffer)
      'src-block
    (lambda (b)
      (let ((src-block-column 74)
            (begin (org-element-property :begin b))
            (end (org-element-property :end b)))
        (letrec ((check-function
                  (lambda ()
                    (forward-line)
                    (unless (>= (point) end)
                      (let ((line (s-trim-right
                                   (thing-at-point 'line))))
                        (when (> (length line) src-block-column)
                          (warn
                           "Code block exceeds 70 lines. Line: %s \n%s"
                           (line-number-at-pos)
                           line)))
                      (funcall check-function)))))
          (goto-char begin)
          (funcall check-function))))))

(defun coe-code--src-copy ()
  "Copies the last src block in an org file.

Inserts the block at the end of the org file."
  (let ((last-block nil))
    (org-element-map
        (org-element-parse-buffer)
        'src-block
      (lambda (b) (setq last-block
                   (cons (org-element-property :begin b)
                         (org-element-property :end b)))))
    (when last-block
      (goto-char (point-max))
      (insert (buffer-substring-no-properties
               (car last-block)
               (cdr last-block))))))


;; Core

(defun coe-code--overlays-between (begin end)
  "Overlays between two points."
  (--filter (overlay-get it 'coe-type)
            (overlays-in begin end)))

(defun coe-code--overlays ()
  "Get all the overlays in the file."
  (coe-code--overlays-between (point-min) (point-max)))

(defun coe-code--overlay-insert (begin end type &optional priority)
  "Highlights a region.

PRIORITY indicates if the overlay should be on top of the rest.
The added code overlay may live on top of the surround overlay to indicate that
a an atom was surrounded by parentheses and s-expressions were subsequently
added within those parentheses. For example, an atom could be modified to be
in the body of a let block:

  x => (let (x 1) x)

The let and (x 1) expressions have been added and the entire block is
surrounded."
  (let ((face (pcase type
                ('add 'coe-code-face-diff-add)
                ('remove 'coe-code-face-diff-remove)
                ('replace 'coe-code-face-diff-replace)
                ('surround 'coe-code-face-diff-surround)
                ('omit 'coe-code-face-omit)
                ;; TODO : Should this be centralized?
                ('region 'coe-code-face-scratch-region)
                ('point 'coe-code-face-scratch-point)
                (_ (error "Unrecognized type: %s" type))
                ))
        (o (make-overlay begin end)))
    (overlay-put o 'coe-type type)
    (overlay-put o 'face face)
    (when priority (overlay-put o 'priority 1))))

(defun coe-code--delete-overlays ()
  "Delete the overlays."
  (--each (coe-code--overlays) (delete-overlay it)))

(defun coe-code-delete ()
  "Delete the overlay at point."
  (interactive)
  (--each
      (--filter (overlay-get it 'coe-type) (overlays-at (point)))
    (delete-overlay it)))

;; Persistence

(defun coe-code--save (filename)
  "Save the regions to a file."
  (interactive)
  (let* ((os (--filter (not (= (overlay-start it) (overlay-end it)))
                       (coe-code--overlays)))
         (vals (-distinct
                (--sort
                (--sort

                 (< (caar it) (caar other))
                 (--map
                  (cons (cons (overlay-start it) (overlay-end it))
                        (overlay-get it 'coe-type))
                  os)))))
    (unless (seq-empty-p vals)
      (with-temp-buffer
        (insert "(\n")
        (--each vals (insert (format "%s\n" it)))
        (insert ")")
      (write-region nil nil filename)))))

(defun coe-code--load (filename)
  "Load the regions from a file."
  (when (file-exists-p filename)
    (let ((os (with-temp-buffer
                (insert-file-contents filename)
                (read (buffer-string)))))
      (--each os
        (pcase-let ((`((,start . ,end) . ,type) it))
          (coe-code--overlay-insert start end type))))))

;; Diff faces

(defun coe-code--diff-filename (buf)
  "Computes the name of the diff.el file"
  (concat (file-name-directory (buffer-file-name buf))
          "diff.el"))

(defun coe-code-save-diff ()
  "Save the diff regions to a file."
  (interactive)
  (coe-code--save (coe-code--diff-filename (current-buffer))))

(defun coe-code--load-diff ()
  "Load the diff regions from a file."
  (coe-code--load (coe-code--diff-filename (current-buffer))))

(defface coe-code-face-diff-add '((t :background "#507549"))
  "The face for the added code overlay.")

(defface coe-code-face-diff-remove '((t :background "#754a49"))
  "The face for the deleted code overlay.")

(defface coe-code-face-diff-replace '((t :background "#756649"))
  "The face for the replaced code overlay.")

(defface coe-code-face-diff-surround '((t :background "#614875"))
  "The face for the surrounded code overlay.")

;; Diff insertion

(defun coe-code-diff-add (begin end)
  "Highlights a region as added code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'add t)
  (deactivate-mark))

(defun coe-code-diff-remove (begin end)
  "Highlights a region as removeed code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'remove)
  (deactivate-mark))

(defun coe-code-diff-replace (begin end)
  "Highlights a region as replaceed code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'replace)
  (deactivate-mark))

(defun coe-code-diff-surround (begin end)
  "Highlights an s-expression as being raised into a list."
  (interactive "r")
  (coe-code--overlay-insert begin end 'surround)
  (deactivate-mark))

;; Omission

;; Omission face

(defface coe-code-face-omit '((t :background "#797282"))
  "The face for the surrounded code overlay.")

(defun coe-code-omit (begin end)
  "Highlights an s-expression as being omitted."
  (interactive "r")
  (coe-code--overlay-insert begin end 'omit)
  (deactivate-mark))

;; Scratch

(defface coe-code-face-scratch-region '((t :background "#797282"))
  "The face for the region overlay.")

(defface coe-code-face-scratch-point '((t :background "#614875"))
  "The face for the point overlay.")

(defun coe-code-scratch-region (begin end)
  "Highlights a region as added code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'region)
  (deactivate-mark))

(defun coe-code-scratch-point (point)
  "Highlights a region as added code."
  (interactive "d")
  (coe-code--overlay-insert point (+ 1 point) 'point t))

(defun coe-code--scratch-filename (buf)
  "Computes the name of the scratch.el file"
  (concat (file-name-directory (buffer-file-name buf))
          "scratch.el"))

(defun coe-code-save-scratch ()
  "Save the scratch regions to a file."
  (interactive)
  (coe-code--save (coe-code--scratch-filename (current-buffer))))

(defun coe-code--load-scratch ()
  "Load the scratch regions from a file."
  (coe-code--load (coe-code--scratch-filename (current-buffer))))

;; Export

(defun coe-code--export-text (export-function)
  "Return the marked up src block text at point."
  (let ((el (org-element-at-point)))
    (pcase el
      (`(src-block ,ps)
       (let* ((begin (plist-get ps :begin))
              (end (plist-get ps :end))
              (os (coe-code--overlays-between begin end))
              (src (plist-get ps :value))
              (offset (save-excursion
                           (goto-char begin)
                           (forward-line)
                           (- (line-beginning-position) 1))))

         (with-temp-buffer
           (erase-buffer)
           (insert src)
           (--each os
             (coe-code--overlay-insert (- (overlay-start it) offset)
                               (- (overlay-end it) offset)
                               (overlay-get it 'coe-type))
             (overlay-start it))
           (emacs-lisp-mode)
           (indent-region (point-min) (point-max))
           (--each (coe-code--overlays)
             (funcall export-function it))
           (coe-code--delete-overlays)
           (buffer-string))))
      (_ (message "No src block at point. Found type: %s" (car el))))))

(defun coe-code--export-code-overlay (o)
  "Write an ovrelay to the current export buffer.

The buffer is for export purposes only, so only contains code from a single
src-block. The overlaid text is surrounded by symbols depending on its type."
  (pcase (overlay-get o 'coe-type)
    ('add
     (goto-char (overlay-start o))
     (insert ?≪)
     (goto-char (overlay-end o))
     (insert ?≫))
    ('omit
     (delete-region (overlay-start o) (overlay-end o))
     (goto-char (overlay-start o))
     (insert ?…))
    (_ )))

(defun coe-code--export-scratch-overlay (o)
  "Write an ovrelay to the current export buffer.

The buffer is for export purposes only, so only contains code from a single
src-block. The overlaid text is surrounded by symbols depending on its type."
  (pcase (overlay-get o 'coe-type)
    ('region
     (goto-char (overlay-start o))
     (insert ?⋘)
     (goto-char (overlay-end o))
     (insert ?⋙))
    ('point
     (goto-char (+ 1 (overlay-start o)))
     (insert ?|))
    (_ )))

(defun coe-code--export-headlines ()
  "Return a list of all headlines with accompanying source positions."
  (org-element-map (org-element-parse-buffer)
      'headline
    (lambda (el)
      (-when-let* ((text (org-element-property :raw-value el))
                   (section (--find (equal (org-element-type it) 'section)
                                    (org-element-contents el)))
                   (src-block (--find (equal (org-element-type it) 'src-block)
                                      (org-element-contents section))))
        (cons text (org-element-property :begin src-block))))))

(defun coe-code--export (dirname export-function)
  "Export all code snippets to a directory."
  (--each-indexed (coe-code--export-headlines)
    (pcase it
      (`(,name . ,point)
       (goto-char point)
       (let ((text (coe-code--export-text export-function)))
         (with-temp-buffer
           (insert text)
           (write-file (concat default-directory
                               (format "/%s/" dirname)
                               (number-to-string it-index)
                               "-" name))))))))

(defun coe-code-export-code ()
  "Export all snippets to a 'code' directory."
  (interactive)
  (coe-code--export "code" #'coe-code--export-code-overlay))

(defun coe-code-export-scratch ()
  "Export all snippets to a 'scratch' directory."
  (interactive)
  (coe-code--export "scratch" #'coe-code--export-scratch-overlay))

;; Minor mode

(define-minor-mode
  coe-code-mode
  "Mode for code.org files in the craft of emacs book.
  \\{coe-code-mode-map}"
  nil
  nil
  `((,(kbd "C-c c") . ,#'coe-code-step)
    (,(kbd "C-c a") . ,#'coe-code-diff-add)
    (,(kbd "C-c d") . ,#'coe-code-diff-remove)
    (,(kbd "C-c r") . ,#'coe-code-diff-replace)
    (,(kbd "C-c s") . ,#'coe-code-diff-surround)
    (,(kbd "C-c 0") . ,#'coe-code-omit)
    (,(kbd "C-c e") . ,#'coe-code-export-code)
    (,(kbd "C-c <backspace>") . ,#'coe-code-delete))
  (if coe-code-mode
      ;; Enable coe
      (progn (add-hook 'before-save-hook #'coe-code-save-diff nil t)
             (coe-code--load-diff))
    ;; Disable coe
    (progn (coe-code-save-diff)
           (coe-code--delete-overlays)
           (remove-hook 'before-save-hook #'coe-code-save-diff t))))

(define-minor-mode
  coe-code-scratch-mode
  "Mode for scratch.org files in the craft of emacs book.
  \\{coe-code-scratch-mode-map}"
  nil
  nil
  `((,(kbd "C-c c") . ,#'coe-code-step)
    (,(kbd "C-c r") . ,#'coe-code-scratch-region)
    (,(kbd "C-c p") . ,#'coe-code-scratch-point)
    (,(kbd "C-c e") . ,#'coe-code-export-scratch)
    (,(kbd "C-c <backspace>") . ,#'coe-code-delete))
  (message "Mode %s" coe-code-scratch-mode)
  (if coe-code-scratch-mode
      ;; Enable coe
      (progn (add-hook 'before-save-hook #'coe-code-save-scratch 0 t)
             (coe-code--load-scratch))
    ;; Disable coe
    (progn (coe-code-save-scratch)
           (coe-code--delete-overlays)
           (remove-hook 'before-save-hook #'coe-code-save-scratch t))))

;; Book

(defun coe-code-steps ()
  "A list of all the files under the code directory.

This is used by yas in pollen-markup-mode to insert a src code snippet."
  (interactive)
  (directory-files (concat default-directory "code")))

(defun coe-code-scratches ()
  "A list of all the files under the scratch directory.

This is used by yas in pollen-markup-mode to insert a src code snippet."
  (interactive)
  (directory-files (concat default-directory "scratch")))

(provide 'coe-code)
;;; coe-code.el ends here
