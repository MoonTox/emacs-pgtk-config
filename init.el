
(recentf-mode 1)    ;;remembers of recent files
(setq history-length 25)
(savehist-mode 1)    ;;minibuffer history is saved
(save-place-mode 1);;remembers previous cursor position in a file
(global-page-break-lines-mode)
(setq auto-save-default t)
;;(delete-selection-mode t)

(require 'package) 
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; for versions before 28.1
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(setq use-package-always-defer t)

(use-package sudo-edit)    ;;allows editing in root directory
				
(use-package marginalia;; M-x details information
  :after vertico
  :custom
  (marginalia-align 'left)    ;; info aligns to left/right/center
  :init    ;; starts
  (marginalia-mode))

(add-hook 'dired-mode-hook 'auto-revert-mode)    ;; automatically updates directory after changes

(use-package which-key    ;; shows pressable keys/chords
  :config
  (which-key-mode))
(global-set-key (kbd "C-x W") 'which-key-show-top-level)    ;; instantly show which-key menu

(use-package tree-sitter
  :demand t
  :config
  (require 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(add-hook 'c-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'java-mode-hook #'tree-sitter-mode)

(use-package eglot)
;;(add-to-list 'eglot-server-programs '((c++-mode java-mode)))
;;(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-ensure)

(use-package eglot-java)
(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'eglot-java-mode-hook (lambda ()                                        
  (define-key eglot-java-mode-map (kbd "C-c l n") 'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") 'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") 'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") 'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") 'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") 'eglot-java-project-build-refresh)))


(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0) ;; scroll margin
  (setq vertico-count 10) ;; vertico candidates number
  (setq vertico-resize t) ;; resize the vertico minibuffer
(setq vertico-cycle t))   ;; minibuffer options are cyclical

(use-package vertico-directory    ;; Configure directory extension
  :after vertico
  :ensure nil
  :bind (:map vertico-map   ;; More convenient directory navigation commands
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless    ;;completion algorithm taht matches by any order
  :after marginalia
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind
  ("C-x b" . consult-buffer)    ;; preview buffer while switching
  ("M-y" . consult-yank-pop)    ;; open kill ring
  ("M-g e" . consult-compile-error)   ;; find compile error in minibuffer
  ("M-g f" . consult-flymake)    ;; flymake view like on IDEs
  ("M-g g" . consult-goto-line)    ;; goto line number
  ("M-s d" . consult-find)    ;; fuzzy find file
  ("M-s r" . consult-ripgrep))    ;; ripgrep?
;;  ("M-s l" . consult-line))    ;; better C-s search

(global-set-key (kbd "C-s") 'consult-line)
(use-package affe
  :defer t
  :bind
  ("C-x a f" . affe-find))
	     
(use-package corfu    ;; completion ui
 :custom
 (corfu-cycle t)    ;; cycles the completion options
 (corfu-auto t)    ;; autocompletion
 (corfu-popupinfo-mode t)
 (corfu-min-width 20)    ;; ui minimum width for 30 chars
 (corfu-scroll-margin 5)    ;; scroll margin
 (corfu-history-mode t)    ;; saves comp. option history
  :init
  (global-corfu-mode))

(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package multiple-cursors
  :defer t)
(global-set-key (kbd "C-S-e C-S-e") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'org-mode-hook (lambda ()
			   (setq-local sentence-end-double-space nil)))    ;; disables double space end in org mode
(setq make-backup-files nil)    ;; stops making temporary backup files
(setq delete-by-moving-to-trash t)    ;; sends deleted items to trash instead of permanent delete

;; (use-package emacs
;; :init
;; (setq completion-cycle-threshold 3)
;; (setq tab-always-indent 'complete))

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq native-comp-async-report-warnings-errors nil)    ;; hides the error reports for nativecomp build

(setq frame-resize-pixelwise t)    ;; speeds up startup

(use-package gcmh    ;; Using garbage magic hack
  :demand t
  :config
  (gcmh-mode 1))

(setq gc-cons-threshold 402653184    ;;  Setting garbage collection threshold
      gc-cons-percentage 0.6)

;; (add-hook 'prog-mode-hook 'flymake-mode)

(use-package doom-themes
  :demand t
  :config
  ;;  Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)    ;; loads doom-theme
  (doom-themes-visual-bell-config)    ;; Enable flashing mode-line on errors
  (doom-themes-neotree-config)    ;;  variable in neotree
 ;; (setq catppuccin-flavor 'macchiato)    ;; says it already
 ;;  (catppuccin-reload)     ;; reloads the macchiato variant, shit
(doom-themes-org-config))    ;; that's why I use this bloat
(solaire-global-mode)    ;; darkens some area

(use-package olivetti)    ;; centers the buffer
(global-set-key (kbd "C-c o v") 'olivetti-mode)    ;; olivetti-mode shortcut

(set-face-attribute 'default nil    ;; default type face
  :font "Rec Mono Custom"           ;; with attributes
  :height 110
  :weight 'regular)

(set-face-attribute 'font-lock-comment-face nil    ;;  Makes commented text and keywords italics
  :slant 'italic)                                   ;;  font must have italic faces
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

(setq-default line-spacing 0.12)    ;;  line spacing
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)    ;;  in markdown mode also
(setq-default fill-column 80)    ;;  paragraph column size 80 chars

;;  Needed if using emacsclient. Otherwise fonts will be smaller than expected.
;; (add-to-list 'default-frame-alist '(font . "Rec Mono Custom-11"))

(use-package doom-modeline    ;;  fancy modeline
:config
(setq doom-modeline-height 35)    ;; modeline details
(setq doom-modeline-bar-width 5)
(setq doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

(setq custom-file "~/.emacs.d/custom.el")    ;; trashes custom settings to this path
(load custom-file
      :defer t)

;; basic ui tweaks that come builtin.
(setq inhibit-startup-message t)    ;stops the default startup screen from appearing
(menu-bar-mode -1)    ;; stops menubar, toolbar and scrollbar
(tool-bar-mode -1)    
(scroll-bar-mode -1)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))    ;; starts in fullscreen

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(global-display-line-numbers-mode 1)  ;; shows line numbers everywhere

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
 (all-the-icons-completion-mode))

(add-hook 'prog-mode-hook 'hl-line-mode)
(pixel-scroll-precision-mode)  ;; smooth scrolling :0


(use-package kind-icon    ;; corfu icons
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))


(use-package all-the-icons-dired)     ;; icons for dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;;(add-hook 'dired-mode-hook 'dired-git-mode)

(use-package dired-sidebar
  :defer t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t))

;;(setq inhibit-compacting-font-caches t)

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)    ;; center dashboard, default left
  (setq dashboard-startup-banner "~/.emacs.d/xemacs_color.svg")    ;; dashboard image
  (setq dashboard-banner-logo-title "Welcome to Emacs")    ;; welcome message
  (setq dashboard-items '((bookmarks . 3)    ;; items
			  (agenda . 3)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-modify-heading-icons '((bookmarks . "book")    ;; items icons
				    (agenda . "calendar")))
  ;; ;; footer
  ;; (setq dashboard-set-footer t
  ;; 	dashboard-footer-icon
  ;; 	(all-the-icons-fileicon "emacs"
  ;;      				:height 1
  ;; 				:v-adjust -0.1
  ;; 				:face 'font-lock-string-face))
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))    ;; init buffer
(setq org-agenda-span 'month)    ;; views org agenda for the whole month

(setq org-todo-keywords    ;; todo list keywords
      '((sequence "TODO" "NEXT" "|" "DONE")))    ;; done is the separated state
(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)
;; Move "DONE" items to the bottom of the agenda
(setq org-agenda-sorting-strategy
      '((agenda todo-state-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(global-set-key (kbd "C-c i t") 'org-insert-todo-heading)    ;; insert todo heading, easier than switching

(setq org-agenda-files '("~/Library/org/tasks.org"    ;;  org agenda files list
			 "~/Library/org/uni.org"
			 "~/Library/org/coding.org"
			 "~/Library/org/books.org"))
(setq org-agenda-include-diary nil)    ;; exclude the holiday shit

(setq ispell-program-name "aspell")   ;; spell checking with aspell
(setq ispell-dictionary "english")    ;;  english dictionary for aspell

(dolist (hook '(org-mode-hook))    ;;  spell checker
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)    ;; correction of words using two-finger tap
     (define-key flyspell-mouse-map [mouse-3] 'undefined)))

(set-face-attribute 'variable-pitch nil    ;; non monospace sans font
  :font "Recursive Casual"
  :height 110
  :weight 'regular)
;; (set-face-attribute 'fixed-pitch nil     ;; maybe documantation font
;;   :font "Rec Mono Custom"
;;   :height 120
;;   :weight 'regular)
(set-fontset-font "fontset-default" 'bengali    ;; bengali font
		  (font-spec :family "Hind Siliguri"
			     :size 16))

(add-hook 'org-mode-hook 'turn-on-auto-fill)    ;;  auto setting of paragraphs
(defun my-pretty-symbols ()    ;;  changes certain keywords to symbols
  (setq prettify-symbols-alist
        '(("#+title" . "🌟")
          ("#+author" . "👤")
          ("#+date" . "📅")
	  ("#+begin_quote" . "❝")
	  ("#+end_quote" . "❝")))
	  ;; ("#+begin_src" . "🛈")
	  ;; ("#+end_src" . "🛈")))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook 'my-pretty-symbols)

(use-package markdown-mode
  :defer t)

(setq org-emphasis-alist   ;; emphasize bold, italic, underline
      '(("*" bold)
	("/" italic)
	("_" underline)))
(setq org-hide-emphasis-markers t)    ;; hide emphasis markers
(setq org-ellipsis " ▼")    ;; replace triple dots with symbol
(setq org-fontify-quote-and-verse-blocks t)

(use-package org-superstar     ;; fancy org heading icons
:config
(add-hook 'org-mode-hook 'org-superstar-mode))

(use-package ligature
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
