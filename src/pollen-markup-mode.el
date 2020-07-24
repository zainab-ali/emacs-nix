;; -*- lexical-binding: t; -*-

(require 'rx)
(require 'project)

(defconst pollen-font-lock-keywords
  (eval-when-compile
    ;; #lang
    `(
      (,(rx (group (group "#lang")
                   (1+ " ")
                   (group (1+ not-newline))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))
      (,(rx (group (group "◊")
		   (group (1+ word))
		   (group "{")))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t)
       )
      ))
  "Pollen mode keywords")

(font-lock-add-keywords 'pollen-markup-mode pollen-font-lock-keywords)

;;;###autoload
(define-derived-mode pollen-markup-mode text-mode "Pollen markup"
  "Major mode for editing pollen markup."
  :syntax-table
  (let
      ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    table)
  :after-hook
  (font-lock-ensure))

(cl-defun pollen--info-name ()
  "Extracts the name field out of info.rkt file"
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (info (concat project-root "info.rkt")))
    (with-temp-buffer
      (insert-file-contents info)
      (goto-char (point-min))
      (search-forward-regexp
       (rx "(define"
	   (1+ " ")
	   "name"
	   (1+ " ")
	   ?\"
	   (group (1+ (not (any ?\"))))
	   ?\"))
      (match-string 1))))

(cl-defun pollen-start-server ()
  "Start a Pollen project server in the current project."
  (interactive)
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (*buffer* (get-buffer-create "*pollen*"))
       (default-directory (concat project-root (pollen--info-name))))
    (start-process "Pollen" *buffer* "raco" "pollen" "start")))

(provide 'pollen-markup-mode)
