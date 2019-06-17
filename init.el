;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(set-face-attribute 'default nil :height 150)

;; Get rid of scroll-bars when creating a new frame
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Load ivy
(ivy-mode 1)

(use-package ivy :demand
    :config
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "))

;; Load Backup file config

(load "~/.emacs.d/backup.el")

;; Window movement config

;; M-o works like C-x o to move to the other window
(global-set-key (kbd "M-o") 'other-window)

;; This allows directional selection with Shift:
;; S-<left>, S-<right>, S-<up>, S-<down>
(windmove-default-keybindings)

;; Org-Mode config
(setq org-log-done 'time)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; turn on which-key
(which-key-mode)
(setq which-key-popup-type 'minibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("862a0ccc73c12df4df325427f9285fa6a5bbba593a77257f43b01c84269f51b0" "8d8423e863b3fbc6346758d726bae66b3d2bacac526067d7d8f2d710203066eb" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(fci-rule-color "#14151E")
 '(package-selected-packages
   (quote
    (use-package ivy which-key kooten-theme dracula-theme)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "mediumspringgreen")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "goldenrod")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "mediumspringgreen")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "goldenrod")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "mediumspringgreen"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
