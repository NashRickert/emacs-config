;; Package Management

;; Setting up use-package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) ; Gets packages ready (a little unsure what it actually does)
(require 'use-package)
(setq use-package-always-ensure t) ; When using UP, installs package if not already installed


;; Minibuffer Completion Frameworks
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-resize t))

(use-package marginalia
  ; I believe this adjusts level of annotation
  :bind(:map minibuffer-local-map
	     ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-default nil) ; orderless is used by default
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Could consider Embark and Consult packages in addition
;; They seem cool but beyond the scope of what I need/want rn

;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (LaTeX-mode . rainbow-delimiters-mode)
  (text-mode . rainbow-delimiters-mode))


;; Which-Key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.25))

;; Helpful
;; Should remap the most important help commands to helpful
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key))

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t) ;; important
  (setq evil-want-keybinding nil) ;; applies evil to other modes
  ;; Note that these 'want' settings are for overriding
  ;; the existing emacs keybindings with the evil mode ones
  ;; (Or turns some default ones off with nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;; Let C-g exit insert mode too
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Ret makes new line and stays in normal mode
  (define-key evil-normal-state-map (kbd "RET") (lambda () (interactive) (evil-open-below 1) (evil-normal-state)))
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))
  

;; Projectile
;; Note to self: This is awesome and can't believe I didn't use before
(use-package projectile
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; First thing on project switch is to open dired
  (setq projectile-switch-project-action #'projectile-dired))


;; Magit
(use-package magit)
  ;; This would make magit oben its buffer in the same window by default
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  

;; Ace-Window
(use-package ace-window
  :bind ("M-o" . 'ace-window)
  :init
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; YASnippet
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))


;; Company
;; I copy pasted this from the Emacs-kick init
(use-package company
  :defer t 
  :ensure t
  :custom
  (company-tooltip-align-annotations t)      ;; Align annotations with completions.
  (company-minimum-prefix-length 1)          ;; Trigger completion after typing 1 character
  (company-idle-delay 0.0)                   ;; Delay before showing completion (adjust as needed)
  (company-tooltip-maximum-width 50) 
  :config

  ;; While using C-p C-n to select a completion candidate
  ;; C-y quickly shows help docs for the current candidate
  (define-key company-active-map (kbd "C-y")
			  (lambda ()
				(interactive)
				(company-show-doc-buffer)))
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [ret] 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  :hook
  (after-init . global-company-mode)) ;; Enable Company Mode globally after initialization.


;; LSP-Mode
;; This is roughly what an LSP-Mode config would look like
;; Note I need to install necessary lsps on my system (pacman) and then possibly apply in M-x lsp-server-install
;; Also note for real usage there would be many more options to configure
;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   :hook (;; Replace XXX-mode with concrete major mode (e.g. python-mode)
;; 	 (python-mode . lsp-deferred)
;; 	 (c-mode . lsp-deferred)
;; 	 (haskell-mode . lsp-deferred)
;; 	 ;; Should auto-disconnect lsp-mode when in tramp-mode (hopefully)
;; 	 (tramp-mode . (lambda () (when (bound-and-true-p lsp-mode) (lsp-disconnect))))
;;          (lsp-mode . lsp-enable-which-key-integration)) ;; Integrate with Which Key
;;   :commands
;;   (lsp lsp-deferred)
;;   :custom
;;   (lsp-keymap-prefix "C-c l"))                           ;; Set the prefix for LSP commands.

;; ;; LSP-UI
;; (use-package lsp-ui)

;; Eglot Mode
;; I will probably only use this in tramp. For now just gonna comment it out
;; Only tricky bit of initialization would be getting it to automatically
;; init in tramp but not regular files. Chatgpt might help. Could also do manually
(use-package eglot
  :init
  (setq eglot-autoshutdown t) ;; shutdown when no more relevant buffers exist
  (setq flymake-show-diagnostics-at-end-of-line t) ; Doesn't do anything :(
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure)))  


;; Specific Language Modes
(use-package haskell-mode)

(use-package sideline-flymake
  :init
  (setq sideline-flymake-display-mode 'line))

(use-package sideline
  :after sideline-flymake
  :init
  (setq sideline-backends-right '(sideline-flymake))
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t)            ; display the backend name
  :hook
  (flymake-mode . sideline-mode))
