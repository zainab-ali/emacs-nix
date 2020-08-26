;;; -*- lexical-binding: t; -*-
;;; melpa-mirror-packages.el --- Entry point
;;; Commentary:
;;;   A listing of packages for nix to download
;;;
;;;   In a conventional emacs setup, use-package will download dependencies on
;;;   startup. This mechanism downloads dependencies on nix install instead.
;;; Code:

(require 'use-package)

(setq use-package-verbose t)

(use-package diminish
  :commands (diminish))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :commands (undo-tree))

(use-package evil
  :commands
  (evil-define-minor-key evil-insert evil-delay evil-define-key)
  :init (setq
         evil-want-C-u-scroll t
         evil-want-integration t
         evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :commands
  (evil-collection-dired-setup
   evil-collection-eshell-setup
   evil-collection-magit-setup
   evil-collection-buff-menu-setup
   evil-collection-proced-setup))

(use-package
  evil-surround
  :config (global-evil-surround-mode t))

(use-package ivy
  :diminish ivy-mode
  :commands (ivy-completing-read)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-c C-r" . 'ivy-resume)
  :hook
  ((magit-mode . ivy-mode)))

(use-package counsel
  :after ivy
  :bind
  (("\C-s" . 'counsel-grep-or-swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("C-h l" . 'counsel-find-library)
   ("C-x d" . 'counsel-dired)
   ("M-i" . 'counsel-imenu)
   ("M-x" . 'counsel-M-x)
   ("C-x b" . 'counsel-switch-buffer)))

(use-package swiper
  :after ivy)

(use-package magit
    :bind (("s-g" . magit-status)
           ("C-c g b" . magit-blame))
    :config
    (evil-collection-magit-setup))

(use-package evil-magit
  :after (magit evil))

(use-package direnv)

;;; Helper functions
(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position'
and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(use-package column-enforce-mode
  :config
  (setq column-enforce-column 80)
  )

(define-minor-mode code-mode
  "Common bindings for coding.
\\{code-mode-map}"
  :keymap (make-sparse-keymap)
  (display-line-numbers-mode)
  (setq-local display-line-numbers 'relative)
  (column-enforce-mode)
  )

(evil-define-key 'normal code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-line)

(evil-define-key 'visual code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-region)

;;; Customize emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'code-mode)

(use-package dired
  :config
  (evil-collection-dired-setup)
  ;; Lets us copy from one open dired buffer to another
  (setq dired-dwim-target t)
  )

(use-package dired-x
  :after dired
  :init
  ;; Hide ignored files
  (setq dired-omit-mode t)
  )

;; since buff-menu doesn't provide anything
(eval-after-load 'buff-menu
  (evil-collection-buff-menu-setup))

(use-package proced
  :config
  (evil-collection-proced-setup))

;; Needed for nix mode
(use-package irony
  :defer t)

(use-package ffap
  :commands (ffap)
  :defines ffap-c-path)

(use-package woman
  :commands (woman)
  :defines woman-manpath)

(use-package nix-shell
  :commands (nix-eshell nix-eshell-with-packages))

(use-package nix-mode
  :after nix-shell
  :commands (nix-repl nix-shell)
  :mode "\\.nix\\'")

(use-package racket-mode
  :interpreter "racket"
  :commands racket-run
  :mode "\\.rkt\\'"
  :config
  (add-hook 'racket-mode-hook 'racket-xp-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'code-mode)

  (evil-define-key 'normal racket-mode-map
    (kbd "C-c C-c b") 'racket-run)
  (evil-define-key 'visual racket-mode-map
    (kbd "C-c C-c b") 'racket-run)
  (evil-define-key 'normal racket-mode-map
    (kbd "g d") 'racket-xp-visit-definition)
  )

(use-package rainbow-delimiters
  :config
  (add-hook
   'rainbow-delimiters-mode-hook
   (lambda ()
     (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-2-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-3-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-4-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-5-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-6-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-7-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-8-face nil :weight 'semi-bold)
     (set-face-attribute 'rainbow-delimiters-depth-9-face nil :weight 'semi-bold)
     ))
  )

(use-package lispyville
  :diminish lispyville-mode
  :commands lispyville-set-key-theme
  :config (lispyville-set-key-theme
           '(operators
             c-w
             text-objects
             atom-motions
	     additional-movement
             slurp/barf-lispy))
  :hook
  ((emacs-lisp-mode . lispyville-mode)
   (racket-mode . lispyville-mode)))

(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2))

(use-package org-agenda
  :bind
  (("C-c o a" . org-agenda)
   :map org-agenda-mode-map
   ("j" . org-agenda-previous-line)
   ("k" . org-agenda-next-line)))

;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :commands (undo-tree-mode))

(use-package ob-http)

(use-package org
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o t" . org-clock-goto)
  :config
  ;; TODO: Debug why "font-lock-function-name-face" cannot be used to set
  ;; levels
  (set-face-attribute 'org-level-1 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-2 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-3 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-4 nil :weight 'semi-bold)
  ;; ;; TODO: Why can "org-meta-line" not be used?
  (set-face-attribute 'org-block nil :family "FiraCode" :height 220)

  (setq org-todo-keywords
        '((sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED")
          (sequence "CANCELED")))

  ;; Enforce a line length of 80
  (setq fill-column 80)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  )

(defun company--set-mode-backends (mode-hook backends)
  "Set company BACKENDS for MODE-HOOK."
  (let
      ((cb (lambda ()
             (setq-local company-backends backends))))
    (add-hook mode-hook cb)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook 'code-mode))

(use-package anaconda-mode
  :hook python-mode
  :commands (anaconda-mode-find-definitions)
  :config
  (evil-define-key 'normal anaconda-mode-map
    (kbd "gd") 'anaconda-mode-find-definitions))

(use-package company-anaconda
  :commands (company-anaconda)
  :after (anaconda-mode company-mode))

(use-package scala-mode
  :mode "\\.scala\\'"
  :config
  (add-hook 'scala-mode-hook 'code-mode))

(use-package haskell-mode
  (add-hook 'haskell-mode-hook 'code-mode)
  )

(use-package company
  :diminish company-mode
  :bind
  ((:map code-mode-map
         ("C-M-i" . company-complete)))
  :init
  (progn
    (company--set-mode-backends 'emacs-lisp-mode-hook '(company-capf company-files))
    (company--set-mode-backends 'anaconda-mode-hook '(company-anaconda)))
  :hook
  ((emacs-lisp-mode . company-mode)
   (anaconda-mode . company-mode)))

;;TODO: get this building better
(use-package mu4e
  :commands (mu4e)
  :config
  (progn
    (evil-collection-mu4e-setup)
    (setq mu4e-completing-read-function 'ivy-completing-read)
    (setq mail-user-agent 'mu4e-user-agent)))

(use-package moe-theme
  :config
  (load-theme 'moe-light t)
  )

(provide 'melpa-mirror-packages)
