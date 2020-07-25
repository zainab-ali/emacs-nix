;;; melpa-mirror-packages.el --- Entry point
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package evil
  :commands evil-define-minor-key evil-insert evil-delay evil-define-key
  :init (setq
	 evil-want-C-u-scroll t
	 evil-want-integration t
	 evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :commands
  evil-collection-dired-setup
  evil-collection-eshell-setup
  evil-collection-magit-setup
  evil-collection-buff-menu-setup)

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-c C-r" . 'ivy-resume))

(use-package counsel
  :after ivy
  :bind
  (("\C-s" . 'counsel-grep-or-swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("C-h l" . 'counsel-find-library)
   ("M-i" . 'counsel-imenu)
   ("M-x" . 'counsel-M-x)
   ("C-x b" . 'counsel-switch-buffer)))

(use-package swiper
  :after ivy)

(use-package magit
    :bind (("C-c g s" . magit-status)
	   ("C-c g b" . magit-blame))
    :config
    (evil-collection-magit-setup))

(use-package direnv)

;;; Helper functions
(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position' 
and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(define-minor-mode code-mode
  "Common bindings for coding.
\\{code-mode-map}"
  :keymap (make-sparse-keymap)
  (display-line-numbers-mode))

(evil-define-key 'normal code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-line)

(evil-define-key 'visual code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-region)

;;; Customize emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'code-mode)

(with-eval-after-load 'dired
  (progn
    (evil-collection-dired-setup)))

(with-eval-after-load 'buff-menu
  (progn
    (evil-collection-buff-menu)))

(use-package nix-mode
  :commands nix-repl
  :mode "\\.nix\\'")

(use-package racket-mode
  :commands racket-run
  :mode "\\.rkt\\'")

(use-package lispyville
  :diminish lispyville-mode
  :commands lispyville-set-key-theme
  :config (lispyville-set-key-theme
	   '(operators
	     c-w
	     text-objects
	     atom-motions
	     slurp/barf-lispy))
  :hook
  (emacs-lisp-mode . lispyville-mode)
  (racket-mode . lispyville-mode))

;; https://github.com/nix-community/emacs-overlay

(provide 'melpa-mirror-packages)
