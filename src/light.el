;;; -*- lexical-binding: t; -*-

(require 'hydra)

(defun light (arg-string)
  (shell-command (s-concat "light " arg-string))
  )

(defun light-keyboard (arg-string)
  (shell-command (s-concat "light -s sysfs/leds/tpacpi::kbd_backlight " arg-string))
  )

(defhydra hydra-light (global-map "<f5>")
  "brightness"
  ("<f5>" (light "-U 10") "dim")
  ("<f6>" (light "-A 10") "brighten")
  ("<f7>" (light-keyboard "-U 50") "kbd dim")
  ("<f8>" (light-keyboard "-A 50") "kbd brighten")
  )

(provide 'light)
