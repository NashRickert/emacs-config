;; -*- lexical-binding: t -*-
;; Note the above is necessary for consult and related packages

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

;; Note: I tried to keep the Consult and Embark implementations as minimalistic as possible
;; The githubs give slightly more expansive configs that can serve as inspiration
;; Also note Consult has a ton of commands and there is probably a lot to learn

;; Consult
;; No keybindings come predefined. I do a couple in the evil-section using a leader keybinding
;; Anything I use a lot probably deserves a keybinding
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))


;; Embark
(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)) ;; Alternative for describe-bindings

;; Embark-consult
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;; Doom Modeline
(use-package doom-modeline
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
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
  (setq which-key-idle-delay 0.20))

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
  (evil-set-undo-system 'undo-redo)
  ;; Evil mode overrides embark-act otherwise
  (define-key evil-normal-state-map (kbd "C-.") 'embark-act)
  ;; Let C-g exit insert mode too
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Ret makes new line and stays in normal mode
  (define-key evil-normal-state-map (kbd "RET") (lambda () (interactive) (evil-open-below 1) (evil-normal-state)))
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; (setq evil-want-fine-undo t)
  (setq evil-leader/in-all-states t)

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Note: I copped most/all of these keybindings from emacs-kick
  ;; A lot of them overlaod existing keybindings with a leader key
  ;; But a lot of them provide keybindings to things that don't have them, mainly the consult commands

  ;; Miscellaneous keybindings
  (evil-define-key 'normal 'global (kbd "<leader> j") 'ace-window)
  
  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> s f") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> s g") 'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s G") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader> x s") 'save-buffer)

  ;; Buffer management keybindings
  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'consult-buffer) ;; Open consult buffer list
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer
  (evil-define-key 'normal 'global (kbd "<leader> b l") 'consult-buffer) ;; Consult buffer
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer) ;; Consult buffer

  ;; Project management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer) ;; Consult project buffer
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file) ;; Find file in project
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp) ;; Find regexp in project
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Yank from kill ring
  ;; (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Embark actions for contextual commands
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  ;; Help keybindings
  (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key

  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab

  ;; Commenting functionality for single and multiple lines
  (evil-define-key 'normal 'global (kbd "gcc")
    (lambda ()
      (interactive)
      (if (not (use-region-p))
          (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  
  (evil-define-key 'visual 'global (kbd "gc")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (comment-or-uncomment-region (region-beginning) (region-end))))))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  ;; This is done to make using the leader possible in dired mode
  ;; Note with-eval-after-load to make sure it's applied after dired-mode-map exists
  (with-eval-after-load 'dired
      (evil-collection-translate-key 'normal 'dired-mode-map " " 'nil)))


;; Evil Surround
;; Note that this provides another way to surround in visual mode
;; Aside from the M-({[ keybindings I defined
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


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
  (define-key company-active-map [ret] nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  :hook
  ;; Enable Company Mode globally after initialization.
  (after-init . global-company-mode) 
  ;; For now I think eshell mode is fine so long as command doesn't complete
  ;; (eshell-mode . (lambda() (company-mode 0)))
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

;; In Auctex, theoretically can jump to point in pdf from source with C-c C-v
;; If this doesn't work, indicates additional work needed in config
;; (And jump to source from pdf with C-mouse-1)
(use-package pdf-tools
  :custom
  (pdf-view-resize-factor 1.1)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))


;; Auto-Acativating-Snippets
;; Should definitely add more snippets based on what I use most for my classes
;; The github page gives a nice example config with some more ways to use these
;; Can disable snippets in certain modes, bind the to functions, etc.
;; This just works ... wow
(use-package aas
  :hook
  (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'LaTeX-mode
    ;; "\\[" '(yas "\\[ $0 \\]")
    :cond #'texmathp ; expand only in math mode
    ;; "\\{ " '(yas "\\{ $0 \\}")
    "int" '(yas "\\int_{$1}^{$2}$0")
    "sum" '(yas "\\sum_{$1}^{$2}$0")))


;; Vterm
(use-package vterm)
