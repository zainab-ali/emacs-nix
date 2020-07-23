;;; init.el --- Entry point
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; packages enabled by default
(require 'evil)
(require 'evil-collection)
(require 'magit)
(require 'ivy)
(require 'counsel)
(require 'swiper)
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
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (setq prettify-symbols-alist lisp-mode-pretty-alist-1)
				  (prettify-symbols-mode)))

(provide 'site-init)
