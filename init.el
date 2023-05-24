;;basic ui tweaks that come builtin.
(setq inhibit-startup-message t)    ;stops the default startup screen from appearing
(menu-bar-mode -1)    ;;stops menubar, toolbar and scrollbar
(tool-bar-mode -1)    
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)  ;;shows line numbers everywhere
(setq-default line-spacing 0.2)     ;;spacing between lines

(recentf-mode 1)    
(setq history-length 15)
(savehist-mode 1)
(save-place-mode 1)
(delete-selection-mode t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t ;;mepasns we don't have to write ":ensure t" for every package
	use-package-expand-minimally t))

(use-package sudo-edit)
(use-package smex);;remember M-x history
(smex-initialize)
				
(use-package marginalia
  :custom
  (marginalia-align 'left)
  :init
  (marginalia-mode))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(global-hl-line-mode 1)
(pixel-scroll-precision-mode)  ;;smooth scrolling :0
(setq org-emphasis-alist
      '(("*" bold)
	("/" italic)
	("-" underline)))

(use-package which-key
  :config
  (which-key-mode))
(global-set-key (kbd "C-x W") 'which-key-show-top-level)

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(add-hook 'c-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'java-mode-hook #'tree-sitter-mode)

(use-package eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode)))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0) ;;scroll margin
  (setq vertico-count 10) ;;vertico candidates number
  (setq vertico-resize t) ;;resize the vertico minibuffer
(setq vertico-cycle t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(global-set-key (kbd "C-x a f") 'affe-find)

(use-package orderless  ;;completion algorithm taht matches by any order
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
	     
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq sentence-end-double-space nil)
(setq make-backup-files nil)
(setq delete-by-moving-to-trash t)

(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package emacs
:init
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete))

(setq native-comp-async-report-warnings-errors nil)

(use-package olivetti)
(global-set-key (kbd "C-c o") 'olivetti-mode)

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  ;;footer
  (setq dashboard-set-footer t
	dashboard-footer-icon
	(all-the-icons-fileicon "emacs"
				:height 1
				:v-adjust -0.1
				:face 'font-lock-string-face))
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(solaire-global-mode)
(global-set-key (kbd "C-c i t") 'org-insert-todo-heading)


;;(page-break-lines-mode 1)

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq org-agenda-files '("~/Library/org/agenda.org"
			 "~/Library/org/university.org"
			 "~/Library/org/programming.org"
			 "~/Library/org/books.org"))
(setq org-ellipsis " ▼")
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>"))

;;spell checking with aspell
(setq ispell-program-name "aspell")   ;;apt install aspell
(setq ispell-dictionary "english")

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(add-hook 'after-init-hook #'flymake-mode)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'catppuccin t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config))

(setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

(set-face-attribute 'default nil
  :font "Rec Mono Custom"
  :height 110
  :weight 'regular)
(set-face-attribute 'variable-pitch nil
  :font "Recursive Casual"
  :height 110
  :weight 'regular)
(set-face-attribute 'fixed-pitch nil
  :font "Rec Mono Casual"
  :height 110
  :weight 'regular)
(set-fontset-font "fontset-default" 'bengali
		  (font-spec :family "Hind Siliguri"
			     :size 16))
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

(defun set-org-sentence-end ()
  (setq-local sentence-end "\\(.।\\)"))
(add-hook 'org-mode-hook 'set-org-sentence-end)


;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))
;; changes certain keywords to symbols, such as lamda!
(defun my-pretty-symbols ()
  (setq prettify-symbols-alist
        '(("#+title" . "🌟")
          ("#+author" . "👤")
          ("#+date" . "📅")))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'my-pretty-symbols)


(use-package markdown-mode)

(use-package doom-modeline
:config
(setq doom-modeline-height 35)
(setq doom-modeline-bar-width 5)
(setq doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
