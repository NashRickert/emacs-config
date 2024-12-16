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
(global-display-line-numbers-mode t)


(load "~/.emacs.d/packages.el")














(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(helpful which-key doom-modeline marginalia vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
