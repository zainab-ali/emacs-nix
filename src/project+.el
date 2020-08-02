;;; -*- lexical-binding: t; -*-

(require 'ivy)

(cl-defun project+-known-projects ()
  "Get known projects."
  (let
      ((file (concat user-emacs-directory ".project-bookmarks.el")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(cl-defun project+-find-file-in (filename dirs project)
  (cl-letf
      (((symbol-function 'completing-read) #'ivy-completing-read))
    (project-find-file-in filename dirs project)))

(cl-defun project+-switch-project (dir)
  "Switch to the project with the root DIR."
  (interactive
   (list
    (ivy-completing-read
     "Project: "
     (project+-known-projects))))
  (let
      ((project (project--find-in-directory dir)))
    (project+-find-file-in nil (project-roots project) project)))

(cl-defun project+-find-file ()
  (interactive)
  (cl-letf
      (((symbol-function 'completing-read) #'ivy-completing-read))
    (project-find-file)))

(cl-defun project+-start-nix-shell ()
  (interactive)
  (let
      ((dir (car (project-roots (project-current t)))))
    (nix-eshell (expand-file-name (concat dir "shell.nix")))))

;;;###autoload
(defvar project+-command-map
  (let
      ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'project+-switch-project)
    (define-key map (kbd "f") #'project+-find-file)
    (define-key map (kbd "s") #'project+-start-nix-shell)
    map)
  "Useful map")

;;;###autoload
(define-minor-mode
  project+-mode
  "Extra functions build on top of project.el."
  :global t)

(provide 'project+)
