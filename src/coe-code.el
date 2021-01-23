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

;;; Code:

;; Construction

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

(defun coe-code--overlays ()
  "Get all the overlays in the file."
  (interactive)
  (--filter (overlay-get it 'coe-type)
            (overlays-in (point-min) (point-max))))


;; Diff persistence

(defun coe-code--diff-filename (buf)
  "Computes the name of the diff.el file"
  (concat (file-name-directory (buffer-file-name buf))
          "diff.el"))

(defun coe-code--save-diff ()
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
  (interactive)
  (let ((diff-filename (coe-code--diff-filename (current-buffer))))
    (when (file-exists-p diff-filename)
      (let ((os (with-temp-buffer
                  (insert-file-contents diff-filename)
                  (read (buffer-string)))))
        (--each os
          (pcase-let ((`((,start . ,end) . ,type) it))
            (coe-code--overlay-insert start end type)))))))

(defun coe--delete-regions ()
  "Delete the overlays."
  (--each (coe-code--overlays) (delete-overlay it)))


;; Diff faces

(defface coe-code-face-add '((t :background "#507549"))
  "The face for the added code overlay.")

(defface coe-code-face-remove '((t :background "#754a49"))
  "The face for the added code overlay.")

(defface coe-code-face-modify '((t :background "#756649"))
  "The face for the added code overlay.")

;; Diff insertion

(defun coe-code-diff-add (begin end)
  "Highlights a region as added code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'add)
  (deactivate-mark))

(defun coe-code-diff-remove (begin end)
  "Highlights a region as removeed code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'remove)
  (deactivate-mark))

(defun coe-code-diff-modify (begin end)
  "Highlights a region as modifyed code."
  (interactive "r")
  (coe-code--overlay-insert begin end 'modify)
  (deactivate-mark))

(defun coe-code-diff-delete ()
  "Delete the overlay at point."
  (interactive)
  (--each
      (--filter (overlay-get it 'coe-type) (overlays-at (point)))
    (delete-overlay it)))

(defun coe-code--overlay-insert (begin end type)
  "Highlights a region."
  (let ((face (pcase type
                ('add 'coe-code-face-add)
                ('remove 'coe-code-face-remove)
                ('modify 'coe-code-face-modify)))
        (o (make-overlay begin end)))
    (overlay-put o 'coe-type type)
    (overlay-put o 'face face)))

(define-minor-mode
  coe-code-mode
  "Mode for code.org files in the craft of emacs book.
  \\{coe-code-mode-map}"
  nil
  nil
  `((,(kbd "C-c c") . ,#'coe-code-step)
    (,(kbd "C-c a") . ,#'coe-code-diff-add)
    (,(kbd "C-c r") . ,#'coe-code-diff-remove)
    (,(kbd "C-c m") . ,#'coe-code-diff-modify)
    (,(kbd "C-c <backspace>") . ,#'coe-code-diff-delete))
  (if coe-code-mode
      ;; Enable coe
      (progn (add-hook 'before-save-hook #'coe-code--save-diff nil t)
             (coe-code--load-diff))
    ;; Disable coe
    (progn (coe-code--save-diff)
           (coe--delete-regions)
           (remove-hook 'before-save-hook #'coe-code--save-diff t))))

(provide 'coe-code)
;;; coe-code.el ends here
