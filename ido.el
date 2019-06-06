;; IDO Mode setup
(require 'ido)
(ido-mode t)

(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; show choices vertically
(if (version< emacs-version "25")
  (progn
    (make-local-variable 'ido-separator)
    (setq ido-separator "\n"))
  (progn
    (make-local-variable 'ido-decorations)
    (setf (nth 2 ido-decorations) "\n")))

;; show any name that has the chars you typed
(setq ido-enable-flex-matching t)

;; use current pane for newly opened file
(setq ido-default-file-method 'selected-window)

;; use current pane for newly switched buffer
(setq ido-default-buffer-method 'selected-window)

;; stop ido suggestion when doing a save-as
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)
