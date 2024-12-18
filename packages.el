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
;; Note that I copied (most of) this base config from emacs-kick
;; It's extremely possible not all these settings are suitable -- they seem opinionated
;; It seems to me that I need to install a server on arch and then add the hook in the appropriate place
;; I may also be possible to get servers from M-x install-server (?)
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (;; Replace XXX-mode with concrete major mode (e.g. python-mode)
	 ;; Note I don't have access to C-c l. Use this for debugging.
	 ;; ALso note glitchiness of options box and line numbers
	 (python-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)) ;; Integrate with Which Key
  :commands (lsp lsp-deferred)
  ;; :config
  ;; (setq lsp-auto-guess-root t) ;; Theoretically fixes 'not part of project' issues (but people say it's bad practice)
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable t)                             ;; Enable inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color nil)                  ;; Disable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all nil)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)   ;; Enable symbol numbers in the headerline.
  (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
  (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil))                     ;; Disable semantic tokens.

;; (use-package lsp-pyright
;;   :hook
;;   (python-mode . lsp)) ; Maybe this works?
