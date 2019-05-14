(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(set-face-attribute 'default nil :height 150)
