;; Gui adjustments (more minimal)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode -1) ; Value of 1 enables tabs
(tooltip-mode -1) ; Puts help text in minibuffer instead of popup

;; Startup preferences
(setq visible-bell t) ; Visible bell on improper action
(setq inhibit-startup-screen t)
(setq initial-scratch-message "Hello Nash")

;; Visual preferences
(set-fringe-mode 10) ; Sets size of edge fringe
(column-number-mode t)
(load-theme 'gruvbox-dark-hard t)

;; Possible issues for 1000+ lines mitigated by grow-only
(setq-default display-line-numbers-width 3) 
(setq-default display-line-numbers-grow-only t) 
(global-display-line-numbers-mode t)

;; Keybindings
(define-key key-translation-map (kbd "ESC") (kbd "C-g")) 



(load "~/.emacs.d/packages.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit magit projectile evil-collection evil gruvbox-theme helpful which-key doom-modeline marginalia vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
