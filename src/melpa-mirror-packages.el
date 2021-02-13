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
         evil-want-keybinding nil)
  (global-set-key (kbd "C-SPC") #'universal-argument))



(use-package evil-collection
  :after evil
  :commands
  (evil-collection-dired-setup
   evil-collection-eshell-setup
   evil-collection-magit-setup
   evil-collection-buff-menu-setup
   evil-collection-proced-setup
   evil-collection-edebug-setup))

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
  (setq completing-read-function 'ivy-completing-read)
  :bind
  ("C-c C-r" . 'ivy-resume)
  ("C-c v" . 'ivy-switch-view)
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

;; Use counsel to find functions too (counsel-find-function) doesn't exist
;; The previous config has this
;; (ivy-set-actions
;;  'counsel-minor
;;  `(("d" ,(lambda (x) (find-function (cdr x))) "definition")
;;    ("h" ,(lambda (x) (describe-function (cdr x))) "help")))

(use-package swiper
  :after ivy)

(use-package magit
    :bind (("s-g" . magit-status)
           ("C-c g b" . magit-blame))
    :config
    (evil-collection-magit-setup))
(add-to-list 'Info-file-list-for-emacs "magit")

(use-package evil-magit
  :after (magit evil))

(use-package forge
  :after magit)

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
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'code-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'nameless-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("lambda" . 955) prettify-symbols-alist)))
;; For some reason, `SPC' doesn't get bound to `edebug-next-mode' unless we
;; normalize the keymaps. It is bound to `evil-forward-char' instead.
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(add-hook 'css-mode-hook 'code-mode)
(add-to-list 'auto-mode-alist '("\\.css.pp\\'" . css-mode))

;; Customize conf mode
(add-hook 'conf-mode-hook 'code-mode)

;; Use code-mode for all programming environments
(add-hook 'prog-mode-hook 'code-mode)

(evil-define-key 'normal Info-mode-map (kbd "l") 'evil-forward-char)
(evil-define-key 'normal Info-mode-map (kbd "h") 'evil-backward-char)


(use-package dired
  :config
  (evil-collection-dired-setup)
  ;; Lets us copy from one open dired buffer to another
  (setq dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
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
  :commands (nix-eshell nix-eshell-with-packages)
  :config
  (progn
    (require 'woman)
    (require 'irony))
  )

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
  (add-hook 'racket-mode-hook 'yas-minor-mode)

  (evil-define-key 'normal racket-mode-map
    (kbd "C-c C-c b") 'racket-run)
  (evil-define-key 'visual racket-mode-map
    (kbd "C-c C-c b") 'racket-run)
  (evil-define-key 'normal racket-mode-map
    (kbd "g d") 'racket-xp-visit-definition)
  )

(use-package rainbow-delimiters
  :config
  (set-face-attribute 'rainbow-delimiters-base-face nil :weight 'semi-light))

(use-package lispyville
  :diminish lispyville-mode
  :commands lispyville-set-key-theme
  :config (lispyville-set-key-theme
           '(operators
             c-w
             c-u
             prettify
             wrap
             mark
             text-objects
             atom-motions
             additional
             additional-movement
             slurp/barf-lispy))
  (evil-define-key 'visual lispyville-mode-map "b" 'lispy-parens)
  (evil-define-key 'normal lispyville-mode-map "Q" 'save-buffer)
  (evil-define-key 'insert lispyville-mode-map (kbd "M-l")
    'lispyville-my-insert-up-list)
  :hook
  ((emacs-lisp-mode . lispyville-mode)
   (racket-mode . lispyville-mode)))

(defun lispyville-my-insert-up-list ()
  "Assuming there is a parenthesis directly in front of the point,
this moves the point past it."
  (interactive)
  (if (evil-insert-state-p)
      (if (equal (thing-at-point 'char) ")")
          (progn (forward-char)
                 (insert ?\s))
        (error "Next character %s is not a closing parenthesis"))
    (error "Not in insert state")))

(use-package avy
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2))

(use-package org-agenda
  :bind
  (("C-c o a" . org-agenda)
   :map org-agenda-mode-map
   ("j" . org-agenda-previous-line)
   ("k" . org-agenda-next-line)))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands (undo-tree-mode))

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
  ;; TODO: Why can "org-meta-line" not be used?
  (set-face-attribute 'org-block nil :family "Source Code Pro" :height 220)
  (set-face-attribute 'org-table nil :family "Source Code Pro" :height 220)

  (setq org-todo-keywords
        '((sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED")
          (sequence "CANCELED")))

  ;; Enforce a line length of 80
  (setq fill-column 80)
  ;; (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t)
     (sql . t)
     (shell . t))))

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


;; (add-to-list 'auto-mode-alist '("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode))
(use-package scala-mode
  :mode "\\.\\(scala\\|sbt\\|sc\\)\\'"
  ;; :mode "\\.\\(scala\\||sc\\)\\'"
  :config
  (add-hook 'scala-mode-hook 'code-mode)
  (add-hook 'scala-mode-hook 'company-mode)
  (add-hook 'scala-mode-hook 'yas-minor-mode))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all yas-expand)
  :config (yas-reload-all)
  :bind
  (:map yas-minor-mode-map
        ("C-M-j" . yas-expand/evil))
  (:map yas/keymap
        ("C-M-j" . yas/next-field-or-maybe-expand)))

(defun yas-expand/evil (&rest args)
  "Expand in insert-mode. See `yas-expand'."
  (interactive)
  (cond
    ; If in visual state, go to insert state to type
   ((evil-visual-state-p) (progn
                            (evil-insert 0)
                            (apply #'yas-insert-snippet args)))
                                        ; If in insert state apply yas as usual
   ((evil-insert-state-p)
    (apply #'yas-expand args))
                                        ; We don't have a handler for normal state.  The yas-expand call was a typo
   ))

(use-package groovy-mode)

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

(use-package launchctl)
(use-package yaml-mode)

(use-package sudo-edit)

;; Used to namespace elisp when writing libraries
(use-package names)
(use-package nameless)

;; Elisp package linter
(use-package flycheck-package)

(provide 'melpa-mirror-packages)
