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
  "Copies the last src block in an code.org file.

Inserts the block at the end of the code.org file."
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


;; Diff core

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
                ('add 'coe-code-face-add)
                ('remove 'coe-code-face-remove)
                ('replace 'coe-code-face-replace)
                ('surround 'coe-code-face-surround)
                ('omit 'coe-code-face-omit)))
        (o (make-overlay begin end)))
    (overlay-put o 'coe-type type)
    (overlay-put o 'face face)
    (when priority (overlay-put o 'priority 1))))


;; Diff persistence

(defun coe-code--diff-filename (buf)
  "Computes the name of the diff.el file"
  (concat (file-name-directory (buffer-file-name buf))
          "diff.el"))

(defun coe-code-save-diff ()
  "Save the diff regions to a file."
  (interactive)
  (let* ((os (--filter (not (= (overlay-start it) (overlay-end it)))
                       (coe-code--overlays)))
         (vals (--sort
                (< (caar it) (caar other))
                (--map
               (cons (cons (overlay-start it) (overlay-end it))
                     (overlay-get it 'coe-type))
               os)))
        (filename (coe-code--diff-filename (current-buffer))))
    (unless (seq-empty-p vals)
      (with-temp-buffer
        (insert "(\n")
        (--each vals (insert (format "%s\n" it)))
        (insert ")")
      (write-region nil nil filename)))))

(defun coe-code--load-diff ()
  "Load the diff regions from a file."
  (let ((diff-filename (coe-code--diff-filename (current-buffer))))
    (when (file-exists-p diff-filename)
      (let ((os (with-temp-buffer
                  (insert-file-contents diff-filename)
                  (read (buffer-string)))))
        (--each os
          (pcase-let ((`((,start . ,end) . ,type) it))
            (coe-code--overlay-insert start end type)))))))

(defun coe-code--delete-overlays ()
  "Delete the overlays."
  (--each (coe-code--overlays) (delete-overlay it)))


;; Diff faces

(defface coe-code-face-add '((t :background "#507549"))
  "The face for the added code overlay.")

(defface coe-code-face-remove '((t :background "#754a49"))
  "The face for the deleted code overlay.")

(defface coe-code-face-replace '((t :background "#756649"))
  "The face for the replaced code overlay.")

(defface coe-code-face-surround '((t :background "#614875"))
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

(defun coe-code-diff-delete ()
  "Delete the overlay at point."
  (interactive)
  (--each
      (--filter (overlay-get it 'coe-type) (overlays-at (point)))
    (delete-overlay it)))

;; Omission

;; Omission face

(defface coe-code-face-omit '((t :background "#797282"))
  "The face for the surrounded code overlay.")

(defun coe-code-omit (begin end)
  "Highlights an s-expression as being omitted."
  (interactive "r")
  (coe-code--overlay-insert begin end 'omit)
  (deactivate-mark))

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
    (,(kbd "C-c <backspace>") . ,#'coe-code-diff-delete))
  (if coe-code-mode
      ;; Enable coe
      (progn (add-hook 'before-save-hook #'coe-code-save-diff nil t)
             (coe-code--load-diff))
    ;; Disable coe
    (progn (coe-code--save-diff)
           (coe-code--delete-overlays)
           (remove-hook 'before-save-hook #'coe-code-save-diff t))))

;; Export

(defun coe-code--export-text ()
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
             (coe-code--export-overlay-to-text it))
           (coe-code--delete-overlays)
           (buffer-string))))
      (_ (message "No src block at point. Found type: %s" (car el))))))

(defun coe-code--export-overlay-to-text (o)
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

(defun coe-code--code-filename (buf)
  "Computes the name of the code.org file"
  (concat (file-name-directory (buffer-file-name buf))
          "code.org"))

(defun coe-code-export ()
  "Insert a marked up block from the code.org file into the current buffer.

The current buffer is assumed to be a book .pm file with the source tag
representing code snippets."
  (interactive)
  (let* ((buf (current-buffer))
         (code-buf (find-file-noselect (coe-code--code-filename (current-buffer))))
         (text (with-current-buffer code-buf
                 (let* ((blocks (coe-code--export-headlines))
                        (choice (completing-read "Step: " blocks nil t))
                        (src-point (cdr (assoc-string choice blocks))))
                   (goto-char src-point)
                   (coe-code--export-text)))))
    (insert (format "\n◊source{\n%s}"text))))

(provide 'coe-code)
;;; coe-code.el ends here
