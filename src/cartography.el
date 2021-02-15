;;; cartography.el --- Concept map maker             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zainab Ali

;; Author: Zainab Ali <zainab@kebab-ca.se>
;; Keywords: tools

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

;; 

;;; Code:
(cl-defstruct (cartography--node (:constructor cartography--node-make)
                      (:copier nil))
  (label nil :documentation "The unique name of the concept")
  (type nil :documentation "The terrain type of the background.")
  (pos nil :documentation "The zero-indexed x and y position"))

(cl-defstruct (cartography--link (:constructor cartography--link-make)
                      (:copier nil))
  (start nil :documentation "The name of the first concept")
  (end nil :documentation "The name of the second concept")
  (pos nil :documentation
       "The control point of the quadratic bezier curve.")
  (label nil :documentation "The meaning of the link."))

(defvar cartography--nodes nil
  "The nodes on the map")

(defvar cartography--links nil
  "The links on the map")

(defconst cartography--rkt-length 100.0
  "The size of the cartography.rkt grid.
The discrete grid made by the elisp mode is converted into this.")

(defconst cartography--grid-length 10
  "The number of cells spanning the grid.
This is multiplied by the `cartography--scale-factor' to get the continuous cartography.rkt grid..")

(defun cartography--scale-factor ()
  "doc."
  (/ cartography--rkt-length cartography--grid-length))

(defconst cartography--types
  '(hill
    marsh
    forest
    scree)
  "The types of terrain"
  )

(defun cartography--empty-grid ()
  "The text string representing an empty grid.."
  (concat (s-repeat (+ cartography--grid-length 1) "x")
          (s-repeat cartography--grid-length (concat "\nx" (s-repeat cartography--grid-length " ")))))

(defun cartography--buffer-to-grid-pos (buf-pos)
  "Convert a buffer position BUF-POS such as (point) to a grid position."
  (let ((x (- (mod buf-pos (+ 2 cartography--grid-length)) 2))
        (y (- (/ buf-pos (+ 2 cartography--grid-length)) 1)))
    (cons x y)))

(defun cartography--grid-to-buffer-pos (grid-pos)
  "Convert a buffer position BUF-POS such as (point) to a grid position."
  (let ((x (car grid-pos))
        (y (cdr grid-pos)))
    (+ (* (+ y 1) (+ 2 cartography--grid-length)) x 2)))

(ert-deftest cartography--buffer-to-grid-pos ()
  "Test that the buffer pos can be converted to a grid pos."
  ;; Consider a 2 column by 3 line grid
  (let* ((poss '( ((0 . 0) . 14)
                  ((1 . 0) . 15)
                  ((3 . 0) . 17)
                  ((4 . 0) . 18)
                  ((0 . 1) . 26)
                  ((9 . 3) . 59))))
    (--each
        poss
      (progn 
        (should (equal (cartography--buffer-to-grid-pos (cdr it)) (car it)))
        (should (equal (cartography--grid-to-buffer-pos (car it)) (cdr it)))))))

(defun cartography-add-node (point label type)
  "Adds a node to the concept map."
  (interactive
   (list
    (point)
    (read-string "Label: ")
    (completing-read "Type: " cartography--types nil t)))
  (let ((grid-pos (cartography--buffer-to-grid-pos point)))
    (setq-local cartography--nodes
                (cons (cartography--node-make :label label :pos grid-pos :type type) cartography--nodes))
    (delete-char 1)
    (insert (format "%s" (- (length cartography--nodes) 1)))))

(defun cartography--delete-node (point)
  "Deletes a node from the concept map. Links are not deleted."
  (-let* ((grid-pos (cartography--buffer-to-grid-pos point))
          (nodes (--remove (equal (cartography--node-pos it) grid-pos) cartography--nodes)))
    (setq-local cartography--nodes nodes)
    (delete-char 1)
    (insert " ")))

(defun cartography--delete-link (point)
  "Deletes a link. The nodes are not deleted."
  (-let* ((grid-pos (cartography--buffer-to-grid-pos point))
          (links (--remove (equal (cartography--node-pos it) grid-pos) cartography--links)))
    (setq-local cartography--links links)
    (delete-char 1)
    (insert " ")))

(defun cartography-delete-node-or-link (point)
  "Deletes the thing at point from the concept map."
  (interactive "d")
  (if (equal (char-after) "x")
      (cartography--delete-link point)
    (cartography--delete-node point)))

(defun cartography-add-link (point start end label)
  "Adds a link at the point."
  (interactive
   (list
    (point)
    (completing-read "Start: " (-map 'cartography--node-label cartography--nodes) nil t)
    (completing-read "End: " (-map 'cartography--node-label cartography--nodes) nil t)
    (read-string "Label: ")))
  (let ((grid-pos (cartography--buffer-to-grid-pos point)))
    (setq-local cartography--links
                (cons (cartography--link-make :label label
                                   :pos grid-pos
                                   :start start
                                   :end end) cartography--links))
    (delete-char 1)
    (insert "*")))

(defun cartography-describe (point)
  "Describe the thing at point."
  (interactive "d")
  (let* ((pos (cartography--buffer-to-grid-pos point))
        (msg (pcase (char-to-string (char-after))
               ("*" (let ((link (--find (equal (cartography--link-pos it) pos) cartography--links)))
                      (concat (cartography--link-start link) " " (cartography--link-label link) " " (cartography--link-end link))
                      ))
               (" " "Nothing at point")
               (o (cartography--node-label (--find (equal (cartography--node-pos it) pos) cartography--nodes))))))
    (message msg)))


(defun cartography-save ()
  "Save cartography datastructure to a cart.el file."
  (interactive)
  (let ((filename (concat default-directory "/cart.el"))
        (nodes cartography--nodes)
        (links cartography--links))
    (with-temp-buffer
      (insert "(\n")
      (insert "(\n")
      (--each nodes (insert (format "%S\n" it)))
      (insert ")\n")
      (insert ".\n")
      (insert "(\n")
      (--each links (insert (format "%S\n" it)))
      (insert ")\n")
      (insert ")")
      (write-region nil nil filename))))

(defun cartography--load (filename)
  "Load the nodes and links from a file."
  (when (file-exists-p filename)
    (let* ((expr (with-temp-buffer
                   (insert-file-contents filename)
                   (read (buffer-string))))
           (nodes (car expr))
           (links (cdr expr)))
      (setq-local cartography--nodes nodes)
      (--each-indexed cartography--nodes
        (goto-char (cartography--grid-to-buffer-pos (cartography--node-pos it)))
        (delete-char 1)
        (insert (format "%s" it-index)))
      (setq-local cartography--links links)
      (--each cartography--links
        (goto-char (cartography--grid-to-buffer-pos (cartography--link-pos it)))
        (delete-char 1)
        (insert "*")))))

(defun cartography--export-pos (grid-pos)
  "doc."
  (cons (* (cartography--scale-factor) (+ 0.5 (car grid-pos)))
        (* (cartography--scale-factor) (+ 0.5 (cdr grid-pos)))))

(defun cartography-export ()
  "Export to pollen."
  (interactive)
  (let ((nodes cartography--nodes)
        (links cartography--links))
    (with-current-buffer (get-buffer-create "*pollen-map*")
      (switch-to-buffer "*pollen-map*")
      (erase-buffer)
      (insert
       (format "◊(cartography\n`%s\n '%s)"
               (--map
                (format ",(node %S '%s '%s)\n"
                        (cartography--node-label it)
                        (cartography--export-pos (cartography--node-pos it))
                        (cartography--node-type it))
                nodes)
               (--map
                (format "%S\n"
                        (list (cartography--link-label it)
                              (cons (cartography--link-start it)
                                    (cartography--link-end it))
                              (cartography--export-pos (cartography--link-pos it))))
                links))))))

(defun cartography ()
  "doc."
  (interactive)
  (with-current-buffer (get-buffer-create "*cartography*")
    (switch-to-buffer "*cartography*")
    (cartography-mode)
    (erase-buffer)
    (insert (cartography--empty-grid))
    (setq-local cartography--nodes nil)
    (setq-local cartography--links nil)
    (cartography--load (concat default-directory "/cart.el"))))


(defvar cartography-mode-map
  (let ((map (make-sparse-keymap 'cartography-mode-map)))
    (define-key map (kbd "C-c a") #'cartography-add-node)
     (define-key map (kbd "C-c l") #'cartography-add-link)
     (define-key map (kbd "C-c ?") #'cartography-describe)
     (define-key map (kbd "C-c <backspace>") #'cartography-delete-node-or-link)
     (define-key map (kbd "C-c q") #'cartography-save)
     map))

(define-derived-mode
  cartography-mode
  nil
  "Cartography"
  "Mode for generating maps for the craft of emacs book.
  \\{cartography-mode-map}"
  (column-enforce-mode)
  (setq-local column-enforce-column (+ 1 cartography--grid-length)))

(provide 'cartography)
;;; cartography.el ends here
