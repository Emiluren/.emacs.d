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
  (defun exec (command)
    (lambda ()
      (interactive)
      (start-process-shell-command command nil commmand)))

  :config

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "HDMI1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI1 --auto")))
  (exwm-randr-enable)

  (require 'exwm-input) ; To silence warning about set-local-sim...
  (defun firefox-hook ()
    (when (and exwm-class-name
               (string= exwm-class-name "Firefox"))
      (exwm-input-set-local-simulation-keys
       '(([?\C-b] . [left])
         ([?\C-f] . [right])
         ([?\C-p] . [up])
         ([?\C-n] . [down])
         ([?\M-v] . [prior])
         ([?\C-v] . [next])
         ([?\C-d] . [delete])
         ([?\C-k] . [S-end delete])
         ([?\C-s] . ?\C-f) ; find
         ([?\M-w] . ?\C-c) ; copy
         ([?\C-y] . ?\C-v) ; paste
         ([?\M-<] . [home])
         ([?\M->] . [end])
         ([?\C-g] . [esc]) ; TODO: [EXWM] Invalid key: <esc>
         ;; ([?\C-l] . ?\M-left) ; Go back
         ;; ([?\C-r] . ?\M-right) ; Go forward
         ))))
  (add-hook 'exwm-manage-finish-hook #'firefox-hook)

  (use-package windmove
    :config
    (exwm-input-set-key (kbd "s-h") #'windmove-left)
    (exwm-input-set-key (kbd "s-j") #'windmove-down)
    (exwm-input-set-key (kbd "s-k") #'windmove-up)
    (exwm-input-set-key (kbd "s-l") #'windmove-right))

  ;; To move to other frames
  (add-to-list 'load-path "~/.emacs.d/unpackaged")
  (require 'framemove)
  (setq framemove-hook-into-windmove t) ; TODO: Seems to move between workspaces, hmm...

  (exwm-input-set-key (kbd "s-;") #'other-frame)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'increase-brightness)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'decrease-brightness)

  ;; TODO: not sure this is working
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
    (exec "amixer -D pulse sset Master 3%-"))
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
    (exec "amixer -D pulse sset Master 3%+"))
  (exwm-input-set-key (kbd "<XF86AudioMute>")
    (exec "amixer -D pulse sset Master toggle"))

  (add-hook 'exwm-init-hook
            (lambda ()
              (start-process-shell-command
               "compton" "*compton*"
               "compton --config ~/.config/compton.conf")))

  ;;(setq exwm-workspace-minibuffer-position 'bottom) ; Hide minibuffer when idle

  (require 'exwm-config)
  (exwm-config-default)

    ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ([?\C-s] . ?\C-f) ; find
          ([?\M-w] . ?\C-c) ; copy
          ([?\C-y] . ?\C-v) ; paste
          ([?\M-<] . [home])
          ([?\M->] . [end])
          ([?\C-g] . [esc]) ; TODO: [EXWM] Invalid key: <esc>
          ;; ([?\C-l] . ?\M-left) ; Go back
          ;; ([?\C-r] . ?\M-right) ; Go forward
          ))

  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido)

  ;; Make fringes smaller
  (fringe-mode 1)

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
