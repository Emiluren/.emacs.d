;; To silence warnings about missing functions at runtime
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;; TODO: maybe change mode-line color for char-mode?

(use-package exwm
  :preface
  (defun increase-brightness ()
    (interactive)
    (start-process-shell-command "brightness" nil "xbacklight -inc 5"))
  (defun decrease-brightness ()
    (interactive)
    (start-process-shell-command "brightness" nil "xbacklight -dec 5"))

  :config

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "HDMI1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI1 --auto")))
  (exwm-randr-enable)

  (exwm-input-set-key (kbd "s-;") #'other-frame)

  ;;(setq exwm-workspace-minibuffer-position 'bottom) ; Hide minibuffer when idle

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'increase-brightness)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'decrease-brightness)

  ;; TODO: not sure this is working
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command
                         "amixer" nil "amixer -D pulse sset Master 3%-")))
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command
                         "amixer" nil "amixer -D pulse sset Master 3%+")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command
                         "amixer" nil "amixer -D pulse sset Master toggle")))

    ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])))

  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido)

  ;; Make fringes smaller
  ;; TODO: Don't do this for text buffers so scroll bars are visible
  ;;(fringe-mode 1)

  ;; Enable moving of windows to other workspaces
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t))

(use-package symon
  :config
  (setq symon-monitors '(symon-linux-memory-monitor
                         symon-linux-cpu-monitor
                         symon-linux-battery-monitor
                         symon-current-time-monitor))
  (symon-mode))
