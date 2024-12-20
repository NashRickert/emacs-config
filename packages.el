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
  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous))
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
;; Karthink does something weird with warning which I ignore at my own peril
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  ;; Allow snippets in side of snippets
  (setq yas-triggers-in-field t))


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
  ;; Enable Company Mode globally after initialization.
  (after-init . global-company-mode) 
  ;; Use of tabs doesn't play nice in latex mode
  (cdlatex-mode . (lambda() (company-mode 0)))) 



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
;; Using this for my minimal lsp
;; Note there are some possible performance improvements from
;; (setq eglot-events-buffer-size 0)
;; (fset #'jsonrpc--log-event #'ignore)
;; But I have no performance issues so I won't bother
(use-package eglot
:init
(setq eglot-autoshutdown t) ;; shutdown when no more relevant buffers exist
(setq flymake-show-diagnostics-at-end-of-line t) ; Doesn't do anything :(
:hook ((python-mode . eglot-ensure)
	(c-mode . eglot-ensure)
	(haskell-mode . eglot-ensure)))

;; Note: wraps around emacs-lsp-booster installed from AUR
;; Theoretically improves performance
(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

;; Specific Language Modes
(use-package haskell-mode)

;; Sideline Modes
;; Used to get sideline diagnostics for eglot
;; Note sideline is the frontend, sideline-flymake the backend. See github for more info
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


;; LATEX

;; Auctex

;; Possible extra desired functionality:
;; Could try autocompleting snippers, either as karthink does
;; or with auto-activating-snippets package
;; C-c C-p to preview
;; C-c C-v and C-mouse-1 to jump between places in pdf and latex
;; Read Auctex manual (C-h i m auctex)

;; Latex settings. Note automatic auctex installation
(use-package latex
  :ensure auctex
  :hook
  ((plain-TeX-mode . LaTeX-mode)
   (LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . font-lock-mode) ; it may do this automatically already
   (LaTeX-mode . my-LaTeX-mode-dollars) ;; syntax highlights dollar signs properly
   (TeX-after-compilation-finished . TeX-revert-document-buffer))
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq LaTeX-electric-left-right-brace t)
  (setq TeX-electric-math '("$" . "$"))
  (defun my-LaTeX-mode-dollars () (font-lock-add-keywords nil `((,(rx "$") (0 'success t))) t))) 
  

;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab))
  :config
  ;; These intended to get parentheses to work, but possibly not necessary
  (setq cdlatex-paired-parens "$[{") 
  (define-key cdlatex-mode-map  "(" nil))


;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas fields
;; Unfortunately, this complexity seems necessary
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))


;; PDF-Tools

;; In Auctex, theoretically can jump to point in pdf from source with C-c C-g
;; If this doesn't work, indicates additional work needed in config
;; (And jump to source from pdf with C-mouse-1)
(use-package pdf-tools
  :custom
  (pdf-view-resize-factor 1.1)
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode nil)))
  
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))
