;;; -*- lexical-binding: t; -*-

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
		   (group (1+ (or word (syntax symbol))))
		   (group "{")
		   (group (1+ (not (any ?{ ?◊ ?}))))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t)
       (5 font-lock-string-face nil t))
      (,(rx (group (1+ word)))
       (1 font-lock-string-face nil t))
      ))
  "Pollen mode keywords")

(font-lock-add-keywords 'pollen-markup-mode pollen-font-lock-keywords)

(require 'skeleton)

(define-skeleton pollen-skeleton-command
  "Pollen command"
  "Tag: "
  "◊" str "{" _ "}")

(cl-defun pollen-abbrev-tag (tag)
  "Create an abbrev entry for TAG."
  `(,tag "" (lambda () (pollen-skeleton-command ,tag))))

(define-abbrev-table 'pollen-markup-mode-abbrev-table
  `(,(pollen-abbrev-tag "p")
    ,(pollen-abbrev-tag "em")
    ,(pollen-abbrev-tag "pre")))

;;;###autoload
(define-derived-mode pollen-markup-mode text-mode "Pollen markup"
  "Major mode for editing pollen markup."
  :syntax-table
  (let
      ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    (modify-syntax-entry ?- "_")
    table)
  :abbrev-table
  pollen-markup-mode-abbrev-table
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

(defvar pollen-server-port
  8080
  "Port for Pollen server")

(cl-defun pollen-start-server ()
  "Start a Pollen project server in the current project."
  (interactive)
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (*buffer* (get-buffer-create "*pollen*"))
       (default-directory project-root))
    (start-process
     "Pollen" *buffer* "raco" "pollen" "start" (pollen--info-name) (number-to-string pollen-server-port))))


(cl-defun pollen-browse ()
  "Open a page to the active pollen server"
  (interactive)
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (document (file-relative-name
		  buffer-file-name
		  (concat project-root (pollen--info-name)))))
    (browse-url
     (concat
      "localhost:"
      (number-to-string pollen-server-port)
      "/"
      ;;TODO: regex match
      (substring document 0 (- (length document) 3))))))

(provide 'pollen-markup-mode)
