(defun mill-make-process (name commands)
  "Starts a mill process.

   The process is named mill-NAME in a *mill-NAME* buffer.
   Mill is run with the argument list COMMANDS.

   (mill \"server\" '(\"-w\" \"server.compile\")) runs 
      mill -w server.compile
   in a *mill-server* buffer"
  (let* ((proc-name (s-concat "mill-" name))
	 (project-root (cdr (project-current t)))
	(default-directory project-root)
	(output (mill--temp-buffer-name proc-name)))
    (mill--delete-existing-process proc-name)
    (make-process
     :name proc-name
     :filter 'mill-process-filter
     :buffer output
     :command (cons "mill" commands))
    (pop-to-buffer output)
    (compilation-mode)
    (read-only-mode -1)
    ))

(defun mill-process-filter (proc string)
  "The filter for mill processes.  This uses ansi colouring"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (ansi-color-apply string))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun mill--temp-buffer-name (name) (s-concat "*" name "*"))

(defun mill--delete-existing-process (name)
    (condition-case nil (delete-process name) (error nil)))

(provide 'mill)
