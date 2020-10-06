;;; -*- lexical-binding: t; -*-

(require 'hydra)

(defun light (arg-string)
  (shell-command (s-concat "light " arg-string))
  )

(defhydra hydra-light (global-map "<f5>")
  "brightness"
  ("<f5>" (light "-U 10") "dim")
  ("<f6>" (light "-A 10") "brighten"))

(provide 'light)
