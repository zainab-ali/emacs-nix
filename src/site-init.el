;;; init.el --- Entry point
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; packages enabled by default
(require 'evil)
(require 'projectile)

;;; Helper functions
(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position' and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

;;; Customize appearance
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)
(display-battery-mode)


(define-minor-mode code-mode
  "Common bindings for coding.
\\{code-mode-map}"
  :keymap (make-sparse-keymap)
  (display-line-numbers-mode))

(evil-define-key 'normal code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-line)

(evil-define-key 'visual code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-region)

;;; Customize Lisp-like languages
(defconst
  lisp-mode-pretty-alist-1
  '(("lambda" . ?λ)
    ("and" . ?∧)
    ("or" . ?∨)
    ("not" . ?¬)
    ("eq" . ?≡))
  "Prettify rules for Lisp modes.")

;;; Customize emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (setq prettify-symbols-alist lisp-mode-pretty-alist-1)
				  (prettify-symbols-mode)))
(add-hook 'emacs-lisp-mode-hook 'code-mode)

(provide 'site-init)
