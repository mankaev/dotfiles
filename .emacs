;;; .emacs --- Emacs configuration file
;; Package-requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'fringe-mode) (fringe-mode '(10 . 0)))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 50000000)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Reset threshold to its default after Emacs has startup, because a large
            ;; GC threshold equates to longer delays whenever GC happens
            (setq gc-cons-threshold 800000)
            (kill-buffer "*scratch*")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; (require 'socks)
;; (setq url-gateway-method 'socks)
;; (setq socks-noproxy '("localhost"))
;; (setq socks-server '("TOR" "localhost" 9050 5))
;; (setq socks-password "")

(require 'tls)
(setq tls-checktrust t)
(setq gnutls-verify-error t)

(let ((trustfile "/etc/ssl/cert.pem"))
  (setq tls-program
        `(,(format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -no_ssl3 -ign_eof" trustfile)))
  (setq gnutls-trustfiles (list trustfile)))

;; This makes long-line buffers usable
(setq-default bidi-display-reordering nil)

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 2)

(setq-default line-spacing 0.1)
;; Focus new help windows when opened
(setq-default help-window-select t)

;; Turn off annoying settings
(blink-cursor-mode -1)
(tooltip-mode -1)
(line-number-mode -1)

;; Bootstrap `use-package' and `dash'
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'diminish)
             (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'dash))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(require 'dash)
(require 'subr-x)
(require 'time-date)

;; Cursor stretches to the current glyph's width
(setq x-stretch-cursor t)
(setq x-underline-at-descent-line t)
(setq x-gtk-use-system-tooltips nil)        ; Use Emacs tooltips
(setq cursor-in-non-selected-windows nil)   ; Keep cursors and highlights in current window only

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(setq scroll-conservatively 1000              ;; Move to beg/end of buffer before signalling an error
      scroll-error-top-bottom t               ;; Ensure M-v always undoes C-v
      scroll-preserve-screen-position 'always ;; Start recentre from top
      recenter-positions '(top middle bottom) ;; Disable mouse scrolling acceleration
      mouse-wheel-progressive-speed nil)

(setq view-read-only t)                 ; View read-only
(setq large-file-warning-threshold nil) ; No large file warning

(setq utf-translate-cjk-mode nil        ; disable CJK coding/encoding
      buffer-file-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      locale-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-language-environment 'UTF-8)

(setq confirm-nonexistent-file-or-buffer nil) ; Do not ask confirmation for new file/buffer
(setq sentence-end-double-space nil)

(defun risky-local-variable-p (&rest args) "Local variables. no ARGS." nil)

;; Fonts used:
(add-to-list 'default-frame-alist '(font . "Pragmata Pro-18"))
(add-to-list 'default-frame-alist '(cursor-color . "red"))

;; (modify-frame-parameters nil '((inhibit-double-buffering . t)))

;; Prevent emacs from creating a backup file filename~
(setq make-backup-files nil)

(setq auto-save-list-file-prefix     "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Confirm before quitting Emacs
(setq confirm-kill-emacs nil)

;; Don't ask for confirmation
;; (setq confirm-kill-processes nil)      ; emacs 26
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq echo-keystrokes 0.1) ; Faster echo keystrokes

;; Do not pop up *Messages* when clicking on the minibuffer
(bind-key [mouse-1] #'ignore minibuffer-inactive-mode-map)

;; Disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Disable startup messages
(setq ring-bell-function #'ignore
      visible-bell nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Copy to system clipboards
(setq select-enable-clipboard t
      select-enable-primary t)

(setq save-interprogram-paste-before-kill t)

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

;; Configure a reasonable fill column and enable automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

;; Truncate lines during grep
(add-hook 'grep-mode-hook #'toggle-truncate-lines)

;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows t)

;; Underline below the font bottomline instead of the baseline
(setq x-underline-at-descent-line t)

(setq pop-up-frames nil)                ; No popup frames

(setq history-length 1000               ; Store more history
      history-delete-duplicates t)

;; Unset keybinding to use with eyebrowse mode
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(setq frame-resize-pixelwise t ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq initial-major-mode 'emacs-lisp-mode)

(bind-key "C-x C-k" #'kill-this-buffer) ; Kill only the current buffer

;;; Formatting functions
(defun format-buffer (command) "Formatters function, takes COMMAND as argument."
       (let ((file-start (bufferpos-to-filepos (point-min) 'approximate 'utf-8-unix))
             (file-end (bufferpos-to-filepos (point-max) 'approximate 'utf-8-unix))
             (cursor (bufferpos-to-filepos (point) 'exact 'utf-8-unix)))
         (save-excursion
           (shell-command-on-region (point-min) (point-max) command (current-buffer) t t))
         (goto-char (filepos-to-bufferpos cursor 'exact 'utf-8-unix))))

(defun my-before-save-hook () "Format save hook."
       (cond ((eq major-mode 'c++-mode) (clang-format-buffer))
             ((eq major-mode 'go-mode) (gofmt-before-save))
             ((eq major-mode 'typescript-mode) (tide-format-before-save)))
             ;; ((eq major-mode 'haskell-mode) (hindent-reformat-buffer))
             ;; ((eq major-mode 'rust-mode) (rust-format-buffer)))
             ;; ((member 'cider-mode (--filter (and (boundp it) (symbol-value it)) minor-mode-list)) (cider-format-buffer))
             ;; ((eq major-mode 'nxml-mode) (format-buffer "xmllint --format -"))
             ;; ((eq major-mode 'js2-mode) (format-buffer "js-beautify -n -s 2 -"))
             ;; ((eq major-mode 'php-mode) (format-buffer "fmt.phar --indent_with_space=2 --no-backup -"))
             ;; ((eq major-mode 'sh-mode) (format-buffer "beautysh -i 2"))
             ;; ((eq major-mode 'python-mode) (format-buffer "yapf --style='{based_on_style: chromium, indent_width: 2}'"))
             ;; ((eq major-mode 'perl-mode) (format-buffer "perltidy -i=2 -q -st"))
             ;; ((eq major-mode 'sql-mode) (format-buffer "sqlformat -"))

       (delete-trailing-whitespace))

(add-hook 'before-save-hook #'my-before-save-hook)

;; Automatically make scripts executable (hashbang as canary)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Look for unbalanced parens when saving
(add-hook 'after-save-hook 'check-parens nil t)

(use-package server                     ; The server of `emacsclient'
  :ensure nil
  :config (or (server-running-p) (server-mode)))

;; Theme
(use-package zenburn-theme              ; Default theme
  :config (setq custom-safe-themes t)   ; Treat themes as safe
  (load-theme 'zenburn 'no-confirm))

(use-package spaceline
  :init
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode)
  (setq desktop-save t
        desktop-load-locked-desktop t))

(use-package auto-compile
  :defer t
  :config
  (setq auto-compile-display-buffer nil)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package evil
  :bind (:map evil-normal-state-map
              ("SPC" . evil-ace-jump-char-mode)
              ("S-SPC" . evil-ace-jump-word-mode)
              ("C-SPC" . evil-ace-jump-line-mode)
              ([escape] . keyboard-quit))
  :bind (:map evil-operator-state-map
              ("SPC" . evil-ace-jump-char-mode)
              ("S-SPC" . evil-ace-jump-word-mode)
              ("C-SPC" . evil-ace-jump-line-mode)
              ([escape] . evil-normal-state))
  :bind (:map evil-window-map
              ("u" . winner-undo)
              ("d" . kill-buffer)
              ("D" . kill-buffer-and-window))
  :init
  (evil-mode)
  (define-key evil-emacs-state-map [escape] #'evil-normal-state)
  (define-key evil-visual-state-map [escape] #'keyboard-quit)
  (define-key evil-motion-state-map [escape] #'evil-normal-state)

  (setq evil-default-cursor t           ; Do not overwrite cursor colour
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-want-fine-undo t)
  (evil-set-initial-state 'Info-mode 'motion)
  (evil-set-initial-state 'help-mode 'motion))

(use-package minibuffer
  :ensure nil
  :config
  ;; Don't let the cursor go into minibuffer prompt
  (let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
        (dont-touch-prompt-prop '(cursor-intangible t)))
    (setq minibuffer-prompt-properties
          (append default dont-touch-prompt-prop))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

  (setq enable-recursive-minibuffers t     ; Allow to read from minibuffer while in minibuffer.
        use-dialog-box nil)                ; Never use dialogs for minibuffer input
  (minibuffer-depth-indicate-mode t)       ; Show the minibuffer depth (when larger than 1)

  (define-key minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit))

(use-package erc
  :defer t
  :config
  (erc-track-mode t)
  (erc-notify-mode t)
  (erc-completion-mode t)
  (erc-fill-mode t)
  (erc-match-mode t)
  (erc-netsplit-mode t)
  (erc-services-mode t)
  (erc-timestamp-mode t)
  (erc-spelling-mode t)
  (erc-dcc-mode t)
  (setq erc-track-exclude-types '("JOIN" "KICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "447")
        erc-modules '(autojoin button completion fill irccontrols list
                               log match menu move-to-prompt netsplit networks
                               noncommands notifications readonly ring stamp
                               track)
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-nick "mankaev"
        erc-user-full-name "Ilia Mankaev"
        erc-server "127.0.0.1"
        erc-port "6667"
        erc-server-coding-system '(utf-8 . utf-8)
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-insert-away-timestamp-function 'erc-insert-timestamp-left
        erc-log-channels-directory "~/.emacs.d/erc"
        erc-query-display 'buffer
        erc-prompt-for-nickserv-password nil ; Do not ask for password
        erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-interpret-mirc-color t ; Interpret mIRC-style color commands in IRC chats
        erc-kill-buffer-on-part t  ; Kill buffers for channels after /part
        erc-kill-queries-on-quit t ; Kill buffers for private queries after quitting the server
        ;; Kill buffers for server messages after quitting the server
        erc-kill-server-buffer-on-quit t)
  (erc-update-modules)
  (defun erc-global-notify (match-type nick message)
    "Notify when someone sends a message that matches a regexp in `erc-keywords'."
    (when (and (eq match-type 'keyword)
               ;; I don't want to see anything from the erc server
               (null (string-match "^[sS]erver" nick))
               ;; or bots
               (null (string-match "\\(bot\\|serv\\)!" nick)))
      (notifications-notify
       :title nick
       :body message
       :urgency 'normal)))

  (add-hook 'erc-text-matched-hook 'erc-global-notify)
  (add-hook 'erc-mode-hook (lambda ()
                             (setq-local company-backends '((company-capf)))
                             (company-mode))))

(use-package page-break-lines           ; Better looking break lines
  :defer t
  :config (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package hideshow
  :ensure nil
  :defer t
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode)

(use-package hl-line
  :ensure nil
  :defer t
  :hook (prog-mode . hl-line-mode))

(use-package ace-jump-mode
  :defer t
  :config
  (setq ace-jump-mode-move-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))

(use-package parinfer
  :defer t
  :hook ((clojure-mode
          emacs-lisp-mode
          common-lisp-mode
          scheme-mode
          lisp-mode
          racket-mode) . parinfer-mode)
  :config
  (setq parinfer-extensions
        '(defaults                      ; should be included.
           ;; pretty-parens                ; different paren styles for different modes.
           evil                         ; If you use Evil.
           smart-tab                    ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))                 ; Yank behavior depend on mode.
  :diminish parinfer-mode)

(use-package smartparens                ; Parenthesis editing and balancing
  :defer t
  :hook ((emacs-lisp-mode
          clojure-mode
          cider-repl-mode
          inferior-emacs-lisp-mode) . smartparens-strict-mode)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  :config
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil
        ;; Disable debug messages
        sp-message-width nil
        sp-show-pair-from-inside t
        ;; Keep pair content overlay on backward movement
        sp-cancel-autoskip-on-backward-movement nil)
  :diminish smartparens-mode)

(use-package smartparens-config         ; Configure Smartparens
  :ensure smartparens
  :after smartparens
  :bind (:map smartparens-mode-map
              ("M-r"          . sp-raise-sexp)
              ("M-<delete>"   . sp-unwrap-sexp)
              ("C-)"          . sp-forward-sexp)
              ("C-("          . sp-backward-sexp)
              ("C-M-k"        . sp-kill-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward))
  :bind (:map smartparens-strict-mode-map
              ("M-q"          . sp-indent-defun))
  :init
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "[" "]" :wrap "M-[")

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "`" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "'" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "`" nil :actions nil)

  (sp-local-pair 'org-mode "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair 'org-mode "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair 'org-mode "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair 'org-mode "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair 'org-mode "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))

  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode))

(use-package diff-hl                    ; Show changes in fringe
  :defer t
  :config
  (setq diff-hl-draw-borders nil)
  (diff-hl-flydiff-mode)
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode))

(use-package highlight-symbol           ; Highlight and jump to symbols
  :bind (("C-c s %" . highlight-symbol-query-replace)
         ("C-c s o" . highlight-symbol-occur)
         ("C-c s n" . highlight-symbol-next-in-defun)
         ("C-c s p" . highlight-symbol-prev-in-defun))
  :hook ((prog-mode . highlight-symbol-mode)
         (prog-mode . highlight-symbol-nav-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4 ; Almost immediately
        ;; Immediately after navigation
        highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

(use-package rainbow-delimiters         ; Highlight parens
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hi-lock                    ; Custom regexp highlights
  :defer t
  :config (global-hi-lock-mode))

(use-package savehist                   ; Save minibuffer history
  :defer t
  :config
  (setq savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-autosave-interval 180)
  (savehist-mode t))

(use-package uniquify                   ; Unique buffer names
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t  ; Rename after killing uniquified
        ;; Ignore special buffers
        uniquify-ignore-buffers-re "^\\*"))

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :init (eyebrowse-mode)
  :config
  (eyebrowse-setup-opinionated-keys)
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-style 'always
        eyebrowse-new-workspace t
        eyebrowse-wrap-around t))

(use-package winner                     ; Winner mode
  :defer t
  :init (winner-mode))

(use-package ediff-wind                 ; Ediff window management
  :ensure nil
  :defer t
  :config
  ;; Restore window/buffers when you're finishd ediff-ing.
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package undo-tree                  ; Show buffer changes as a tree
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :config (delete-selection-mode))

(use-package subword                    ; Handle capitalized subwords
  :defer t
  ;; Do not override `transpose-words', it should not transpose subwords
  :config
  (global-subword-mode t)
  (bind-key [remap transpose-words] nil subword-mode-map)
  :diminish subword-mode)

(use-package aggressive-fill-paragraph  ; Automatically fill paragrah
  :defer t
  :hook (org-mode . aggressive-fill-paragraph-mode))

(use-package saveplace                  ; Save point position in files
  :config (save-place-mode t))

(use-package super-save                 ; Autosave buffers when they lose focus
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode)
  :diminish super-save-mode)

(use-package bookmark                   ; Bookmarks to files and directories
  :config
  (setq bookmark-completion-ignore-case nil)
  (bookmark-maybe-load-default-file))

;;; Syntax checking
(use-package flycheck                   ; On-the-fly syntax checker
  :defer t
  :config
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)

  :diminish flycheck-mode)

(use-package flycheck-clojure          ; Check clojure
  :defer t
  :after flycheck
  :config
  (flycheck-clojure-setup))

(use-package recentf                    ; Manage recent files
  :defer t
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

(use-package markdown-mode              ; Edit markdown files
  :hook (markdown-mode . auto-fill-mode)
  :mode "\\.md\\'"
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package dired                      ; File manager
  :ensure nil
  :bind (:map dired-mode-map
              ([return]   . dired-find-alternate-file)
              ([C-return] . open-in-external-app))
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun open-in-external-app ()
    "Open the file where point is or the marked files in external app.
The app is chosen from your OS's preference."
    (interactive)
    (let* ((file-list
            (dired-get-marked-files)))
      (mapc (lambda (file-path)
              (let ((process-connection-type nil))
                (start-process "" nil "xdg-open" file-path))) file-list)))
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-auto-revert-buffer t            ; Revert buffers on revisiting
        dired-listing-switches
        "-lFaGh1v --group-directories-first"  ; Add ls switches
        global-auto-revert-non-file-buffers t ; Auto refresh Dired
        auto-revert-verbose nil               ; But be quiet about it
        dired-dwim-target t                   ; Use other pane as target
        dired-recursive-copies 'always        ; Copy dirs recursively
        dired-recursive-deletes 'top          ; Delete dirs recursively
        ;; -F marks links with @
        dired-ls-F-marks-symlinks t))

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-imenu
  :defer t
  :after dired)

(use-package autorevert
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1)
  :diminish auto-revert-mode)

(use-package company                    ; Auto-completion
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :bind ([tab] . company-indent-or-complete-common)
  :hook ((prog-mode text-mode) . company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t ;; Easy navigation to candidates with M-<n>
        company-show-numbers t            ;; Ignore case
        company-dabbrev-ignore-case t     ;; Do not downcase completion
        company-dabbrev-downcase nil)

  :diminish company-mode)

(use-package company-web                ; Backend for web development
  :after company)

(use-package company-jedi               ; Python backend for Company
  :defer t
  :after company
  :config
  (setq jedi:complete-on-dot t
        jedi:imenu-create-index-function 'jedi:create-flat-imenu-index))

(use-package company-restclient         ; Company support for restclient
  :defer t
  :after company)

(use-package company-shell              ; Company support for shell functions
  :defer t
  :after company)

(use-package ispell                     ; Word correction
  :defer t
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-really-hunspell t
        ispell-check-comments  t
        ispell-dictionary "en_GB,ru_RU"
        ispell-choices-win-default-height 5)
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,ru_RU"))

(use-package flyspell                   ; Spell checking on-the-fly
  :hook (text-mode . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)

  ;; Free M-t for transpose words
  (unbind-key "M-t" flyspell-mode-map)
  :diminish flyspell-mode)

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-$" . flyspell-correct-word-generic)))

(use-package projectile                 ; Project management
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-globally-ignored-files '("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")
        projectile-find-dir-includes-top-level t
        projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

  (projectile-mode)
  (projectile-register-project-type 'lein-cljs '("project.clj")
                                    :compile "lein cljsbuild once"
                                    :test "lein cljsbuild test")
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  :diminish projectile-mode)

(use-package vc-hooks                   ; Simple version control
  :ensure nil
  :defer t
  :config
  (setq vc-handled-backends '(Git))     ; Enable only git
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :defer 3
  :bind (:map magit-status-mode-map
              ("q" . #'mu-magit-kill-buffers))
  :config
  ;; Refresh `diff-hl' accordingly
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (setq magit-save-repository-buffers 'dontask
        magit-completing-read-function 'ivy-completing-read
        magit-refs-show-commit-count 'all
        magit-diff-use-overlays nil
        magit-use-overlays nil)

  ;; Show status buffer in fullscreen
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)

  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package evil-magit
  :after magit
  :defer t
  :config (evil-magit-init))

(use-package git-commit                 ; Git commit message mode
  :defer t
  :config
  (global-git-commit-mode)
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))


(use-package git-timemachine            ; Git timemachine
  :defer t)

(use-package gitconfig-mode             ; Git configuration mode
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :defer t)

(use-package tramp                      ; Remote editing
  :defer t
  :config
  (setq auto-save-file-name-transforms nil)

  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(use-package org-contacts
  :defer t
  :ensure org-plus-contrib
  :init
  (setq org-contacts-files '("~/files/org/contacts.org")
        org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY"))

(use-package org                        ; Org Plus Contributions
  :defer t
  :ensure org-plus-contrib
  :config
  (setq org-src-fontify-natively t
        org-log-done 'time
        org-hide-emphasis-markers t
        org-return-follows-link t       ; Follow links by pressing ENTER on them
        org-directory (expand-file-name "~/files/org")
        org-default-notes-file (concat org-directory "/todo.org")
        org-export-coding-system 'utf-8
        org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-window-setup 'current-window
        ;; Define Agenda files
        org-agenda-files '("~/files/org/todo.org"
                           "~/files/org/tasks.org"
                           "~/files/org/notes.org")
        org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t
        org-html-inline-images t)
  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?A)
  ;; Define TODO workflow states
  (setq org-todo-keywords '("TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)"))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local company-backends '((company-ispell company-files company-dabbrev))))))

(use-package org-capture                ; Fast note taking in Org
  :defer t
  :ensure org-plus-contrib
  :config
  (setq org-capture-templates
        '(("w" "Web captures" entry (file+headline "~/files/org/notes.org" "WEB")
           "* %^{Title}    %^G\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1)
          ("p" "Contacts" entry (file "~/files/org/contacts.org")
           "** %(org-contacts-template-name)
:PROPERTIES:%(org-contacts-template-email)
:END:")
          ("t" "TODO" entry (file "~/files/org/todo.org")
           "* TODO %^{Task}  %^G\n   %?")))
  (evil-set-initial-state 'org-capture-mode 'emacs))

(use-package ox-html
  :defer t
  :ensure org-plus-contrib
  :config
  ;; Turn off preamble and postamble in HTML export
  (setq org-html-preamble nil
        org-html-postamble nil))

(use-package org-notify
  :defer t
  :ensure org-plus-contrib
  :config
  (require 'org-notify)
  (org-notify-start)
  (org-notify-add 'halfour '(:time "30m" :actions -notify/window
                                   :period "5m" :duration 60)))

(use-package ox
  :defer t
  :ensure org-plus-contrib
  :config
  (setq org-export-with-timestamps nil
        org-export-with-smart-quotes t))

;;; Programming utilities
(use-package imenu
  :ensure nil
  :defer t
  :config
  (setq imenu-auto-rescan t))

(use-package python                     ; Python editing
  :bind (:map python-mode-map
              ([C-return] . jedi:goto-definition))
  :hook (inferior-python-mode . company-mode)
  :config
  (jedi:setup)
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  (setq python-indent-offset 2)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local company-backends '((company-jedi)))
              (setq fill-column 79)))) ;; PEP 8 compliant filling rules, 79 chars maximum

(use-package slime-company
  :config
  (setq slime-company-completion 'fuzzy)
  (add-hook 'slime-lisp-mode-hook
            (lambda ()
              (setq-local company-backends '((company-slime)))))
  (add-hook 'slime-mode-hook
            (lambda ()
              (setq-local company-backends '((company-slime))))))

(use-package slime
  :hook (lisp-mode . slime-mode)
  :bind (:map slime-mode-map
              ("M-s j"    . slime)
              ([C-return] . slime-edit-definition))
  :config
  (setq slime-contribs '(slime-fancy
                         slime-sbcl-exts
                         slime-quicklisp
                         slime-company)
        slime-net-coding-system 'utf-8-unix
        inferior-lisp-program "sbcl")
  (slime-setup)
  :diminish (slime-mode slime-autodoc-mode))

(use-package elisp-mode                 ; Emacs Lisp editing
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ([C-return] . elisp-def))
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  (defconst use-package-imenu-expression
    `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                         symbol-end (1+ (syntax whitespace)) symbol-start
                         (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                         symbol-end) 1)
    "IMenu expression for `use-package' declarations.")

  (defun add-use-package-to-imenu ()
    "Add `use-package' declarations to `imenu'."
    (add-to-list 'imenu-generic-expression
                 use-package-imenu-expression))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-use-package-to-imenu)
              (setq-local company-backends '((company-capf))))))

(use-package elisp-def
  :defer t
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package cider                      ; Clojure development environment
  :defer t
  :config
  (setq
   nrepl-prompt-to-kill-server-buffer-on-quit nil
   cider-stacktrace-default-filters '(clj java repl tooling dup)
   cider-pprint-fn 'fipp
   ;; Set up Figwheel in ClojureScript REPL
   cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
   ;; Do not offer to open ClojureScript app in browser
   cider-offer-to-open-cljs-app-in-browser nil)
  (add-hook 'cider-stacktrace-mode-hook
            '(lambda ()
               (cider-stacktrace-cycle-cause 2 1)))
  (add-hook 'cider-connected-hook
            '(lambda ()
               (cider-repl-clear-banners)))

  (defvar cider-jack-in-start-time nil)

  (defun start-timing-cider-jack-in (&rest args)
    (setq cider-jack-in-start-time (current-time)))

  (defun elapsed-time-cider-jack-in (&rest args)
    (when cider-jack-in-start-time
      (prog1 (format "%.3f seconds"
                     (float-time
                      (time-since cider-jack-in-start-time)))
        (setq cider-jack-in-start-time nil))))

  (add-function :before
                (symbol-function 'cider-jack-in)
                #'start-timing-cider-jack-in)
  (setq cider-connection-message-fn
        #'elapsed-time-cider-jack-in)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert))

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :ensure cider
  :hook (cider-mode . cider-company-enable-fuzzy-completion)
  :hook cider-repl-mode
  :bind (:map cider-mode-map
              ("M-s r" . cider-refresh)
              ([C-return] . (lambda () (interactive) (cider-find-var t))))
  :config
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (evil-set-initial-state 'cider-browse-ns-mode 'motion)
  (evil-set-initial-state 'cider--debug-mode 'emacs)
  (evil-set-initial-state 'cider-docview-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (setq cider-show-eval-spinner nil)
  :diminish cider-mode)

(use-package clojure-mode               ; Major mode for Clojure files
  :bind (:map clojure-mode-map
              ("M-s j" . cider-jack-in)
              ("M-s J" . cider-jack-in-clojurescript))
  :init
  (require 'clojure-mode)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2))
  :config
  (setq clojure-align-forms-automatically t)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local company-backends '((company-capf))))))

(use-package clojure-mode-extra-font-locking ; Font-locking for Clojure mode
  :defer t
  :after clojure-mode)

(use-package nrepl-client               ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :hook (cider-repl-mode . cider-company-enable-fuzzy-completion)
  :hook (cider-repl-mode . company-mode)
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . cider-repl-clear-buffer))
  :config
  (setq cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
        cider-repl-display-help-banner nil
        cider-repl-history-display-duplicates nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-scroll-on-output nil
        cider-repl-display-in-current-window t)
  (evil-set-initial-state 'cider-repl-mode 'insert))

(use-package clj-refactor               ; Refactoring utilities
  :defer t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-suppress-middleware-warnings t
        cljr-add-ns-to-blank-clj-files t
        cljr-auto-sort-ns t
        cljr-favor-prefix-notation cljr-favor-private-functions
        cljr-warn-on-eval nil)
  :diminish clj-refactor-mode)

(use-package geiser                    ; Geiser mode
  :config
  (setq geiser-default-implementation 'chez
        geiser-active-implementations '(chez guile)
        geiser-repl-use-other-window nil
        geiser-repl-history-filename "~/.emacs.d/geiser-history")
  :diminish (geiser-mode geiser-autodoc-mode))

(use-package clojure-snippets           ; Yasnippets for Clojure
  :defer t
  :after clojure-mode)

(use-package yasnippet                  ; Snippets
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 1                 ; No need to be so verbose
        yas-wrap-around-region t)
  (yas-reload-all)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;;; Idris
(use-package idris-mode                 ; Idris language mode
  :defer t
  :config
  (setq idris-repl-banner-functions nil
        idris-repl-prompt-style 'short
        idris-repl-show-idris-version nil
        idris-semantic-source-highlighting nil)

  (evil-set-initial-state 'idris-repl-mode 'emacs)
  :diminish idris-simple-indent-mode)

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :bind (("C-c d s" . sql-connect)
         :map sql-mode-map
         ("C-c m p" . sql-set-product)))

(use-package sqlup-mode                 ; Upcase SQL keywords
  :defer t
  :hook sql-mode
  :diminish sqlup-mode)

;;; Web development
(use-package web-mode                   ; Major mode for editing web templates
  :defer t
  :mode "\\.[sx]?html?\\'"
  :mode "\\.css\\'"
  :mode "\\.xml\\'"
  :config
  (set-face-background 'web-mode-current-element-highlight-face "grey40")
  (add-hook 'web-mode-hook
            '(lambda ()
               (when (string-equal "tsx" (file-name-extension buffer-file-name))
                 (tide-setup))
               (when (string-equal "jsx" (file-name-extension buffer-file-name))
                 (tide-setup))
               (require 'company-web-html)
               (setq-local company-backends '((company-web-html company-css)))))

  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-css-colorization t
        web-mode-enable-sql-detection t
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package js2-mode                   ; Powerful JavaScript mode
  :defer t
  :config
  ;; Disable parser errors and strict warnings
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq-local company-backends '((company-tern)))
              ;; Better Imenu in j2-mode
              (js2-imenu-extras-mode))))

(use-package js2-refactor               ; Refactor JavaScript
  :defer t
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package less-css-mode
  :defer t)

(use-package php-mode                   ; Better PHP support
  :bind (:map php-mode-map
              ("C-t" . ac-php-location-stack-back)
              ([C-return] . ac-php-find-symbol-at-point))
  :mode "\\.php\\'"
  :config
  (use-package ac-php
    :defer t)
  (use-package company-php
    :defer t)
  (add-hook 'php-mode-hook
            (lambda ()
              (require 'ac-php)
              (require 'company-php)
              (setq-local company-backends '((company-ac-php-backend))))))

;;; Other languages
(use-package sh-script                  ; Shell scripts
  :ensure nil
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2       ; The basic indentation
        sh-basic-offset 2)     ; The offset for nested indentation
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq-local company-backends '((company-shell))))))

(use-package nxml-mode                  ; XML editing
  :ensure nil
  :defer t
  :config
  ;; Complete closing tags, and insert XML declarations into empty files
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t
        ;; Treat elements (with children) as sexps
        nxml-sexp-element-flag t)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq-local company-backends '((company-nxml))))))

(use-package json-mode                  ; JSON editing
  :defer t
  :mode "\\.json\\'")

(use-package clang-format               ; Clang format C/C++ code
  :defer t
  :config
  (setq clang-format-style "LLVM"))

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ([C-return] . godef-jump))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local company-backends '((company-go))))))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\.cmake\\'")
  :config
  (add-hook 'cmake-mode-hook
            (lambda ()
              (setq-local company-backends '((company-cmake))))))

(use-package gdb-mi
  :ensure nil
  :defer t
  :config
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(use-package modern-cpp-font-lock
  :defer t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  :diminish modern-c++-font-lock-mode)

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c C-c" . projectile-compile-project)
              ([C-M-tab] . clang-format-region)
              ([C-return] . counsel-gtags-dwim))
  :config
  (setq c-basic-offset 2)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local company-backends '((company-clang company-c-headers)))
              (setq-local flycheck-gcc-language-standard "c++17")
              (setq-local flycheck-clang-language-standard "c++17")))
  (add-hook 'c-mode-hook
            (lambda ()
              (setq-local company-backends '((company-clang company-c-headers)))
              (setq-local flycheck-gcc-language-standard "c11")
              (setq-local flycheck-clang-language-standard "c11"))))

(use-package tide
  :defer t
  :hook (typescript-mode . tide-mode)
  :bind (:map tide-mode-map
              ([C-return] . tide-jump-to-definition))
  :config
  (setq tide-jump-to-definition-reuse-window t)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (tide-hl-identifier-mode t)))
  :diminish tide-mode)

(use-package abbrev
  :ensure nil
  :defer t
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  :diminish abbrev-mode)

(use-package racket-mode                ; Racket language mode
  :defer t
  :mode "\\.rkt\\'"
  :hook (racket-repl-mode . company-mode)
  :bind (:map racket-mode-map
              ("M-s j" . racket-run)))

(use-package realgud                    ; Additional debug modes
  :defer t)

(use-package systemd                    ; Edit Systemd units
  :defer t)

(use-package yaml-mode                  ; Edit YAML files
  :defer t)

(use-package pkgbuild-mode              ; PKGBUILD files for Archlinux
  :defer t)

(use-package restclient                 ; Interactive HTTP client
  :defer t
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (setq-local company-backends '(company-restclient)))))

(use-package eldoc                      ; Documentation in the echo area
  :ensure nil
  :defer t
  :hook (eval-expression-minibuffer-setup-hook . eldoc-mode)
  :config
  ;; Enable Eldoc for `eval-expression', too
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  (setq eldoc-idle-delay 0.1)  ; Show eldoc more promptly
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :diminish eldoc-mode)

(use-package etags                      ; Tag navigation
  :ensure nil
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (setq tags-revert-without-query t))

(use-package compile                    ; Compile from Emacs
  :defer t
  :config
  (setq compilation-ask-about-save nil
        ;; Kill old compilation processes before starting new ones
        compilation-always-kill t
        ;; Automatically scroll and jump to the first error
        compilation-scroll-output 'first-error
        compilation-auto-jump-to-first-error t
        ;; Skip over warnings and info messages in compilation
        compilation-skip-threshold 2
        ;; Don't freeze when process reads from stdin
        compilation-disable-input t
        ;; Hide compilation buffer on success
        compilation-finish-functions '(compilation-finish-hide-buffer-on-success)
        ;; Show three lines of context around the current message
        compilation-context-lines 3)
  (defun compilation-finish-hide-buffer-on-success (buf str)
    "Could be reused by other `major-mode' after compilation.  BUF.  STR."
    (if (string-match "exited abnormally" str)
      ;; there were errors
      (message "Compilation error, press C-x ` to visit")
      ;; no errors, make the compilation window go away in 0.5 seconds
      (when (string-suffix-p "compilation*" (buffer-name buf))
        (bury-buffer (buffer-name buf))
        (winner-undo)
        (message "Compiled successfully"))))

  (defun colorize-compilation-buffer ()
    "Colourize a compilation mode buffer."
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook
            #'colorize-compilation-buffer))

(use-package ggtags
  :defer t
  :config
  (setq ggtags-use-sqlite3 t))

(use-package eshell                     ; Emacs command shell
  :ensure nil
  :bind (("M-s t" . eshell-here))
  :config
  (setq eshell-history-size 1024
        eshell-highlight-prompt nil
        eshell-banner-message ""
        eshell-save-history-on-exit t
        eshell-hist-ignoredups t
        eshell-history-size 1000
        eshell-prompt-function (lambda () "$ ")
        eshell-prompt-regexp "^$ "
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("top" "less" "more" "htop" "mc" "ncmpcpp"))

  (setq eshell-visual-subcommands '(("git" "lg" "st" "log" "diff" "show")))

  (defun eshell/clear ()
    "Clear the eshell buffer"
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)))

  (defun eshell/magit ()
    "Function to open magit-status for the current directory"
    (magit-status-internal "."))

  (defun eshell-here ()
    "Go to eshell and set current directory to the buffer's directory"
    (interactive)
    (let ((dir (file-name-directory (or (buffer-file-name)
                                        default-directory))))
      (eshell)
      (eshell/pushd ".")
      (eshell/cd dir)
      (goto-char (point-max))
      (eshell-kill-input)
      (eshell-send-input)))
  (evil-set-initial-state 'eshell-mode 'emacs))

(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

(use-package counsel-gtags
  :defer t
  :config
  (setq counsel-gtags-auto-update t)
  (counsel-gtags-mode)
  :diminish counsel-gtags-mode)

(use-package counsel
  :ensure smex
  :bind (("M-s i"   . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("M-s b"   . counsel-bookmark)
         ("M-y"     . counsel-yank-pop)
         ("M-x"     . counsel-M-x)
         ("M-s k"   . bury-buffer)
         ("M-<tab>" . mode-line-other-buffer))
  :config
  (setq counsel-git-cmd "rg --files"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (defun my/find-file-hook ()
    (when (not (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo::" buffer-file-name))))
  (add-hook 'find-file-hook 'my/find-file-hook)
  (counsel-mode)
  :diminish counsel-mode)

(use-package woman
  :ensure nil
  :bind (("M-s m"   . woman)))

(use-package find-file-in-project
  :bind (:map projectile-mode-map
              ("M-s f"   . find-file-in-project-by-selected))
  :config
  (setq ffip-use-rust-fd t))

(use-package counsel-projectile
  :bind (:map projectile-mode-map
              ("M-s a"   . counsel-projectile-rg)))

(use-package swiper
  :bind (("M-s s"   . swiper)))

(use-package ivy
  :bind (("C-x C-b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
              ([tab] . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t    ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-wrap t
        ivy-height 17                ; number of result lines to display
        ivy-count-format ""          ; does not count candidates
        ivy-initial-inputs-alist nil ; no regexp by default
        ivy-re-builders-alist        ; configure regexp engine.
        '((t . ivy--regex-plus)))    ; allow input not in order
  (ivy-mode)
  :diminish ivy-mode)

(use-package eww
  :ensure nil
  :defer t
  :init
  (setq eww-header-line-format nil
        browse-url-browser-function 'browse-url-xdg-open
        shr-width 80
        ;; shr-use-colors nil ; emacs 26
        shr-use-fonts nil)
  ;; No colours in eww buffers
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
  :config
  (evil-set-initial-state 'eww-bookmark-mode 'motion)
  (evil-set-initial-state 'eww-mode 'motion))

(use-package calendar
  :ensure nil
  :defer t
  :config
  (require 'calendar)
  (setq calendar-date-style 'european
        calendar-week-start-day 1
        calendar-holidays nil)

  (evil-set-initial-state 'calendar-mode 'emacs)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package mu4e
  :ensure nil
  :defer 3
  :config
  (setq mu4e-maildir "~/mail"
        mu4e-attachment-dir "~/tmp"
        mu4e-context-policy 'pick-first
        mu4e-user-agent-string "emacs"
        mu4e-update-interval 180
        mu4e-view-image-max-width 800 ; enable inline images
        mu4e-compose-format-flowed t
        mu4e-hide-index-messages t
        mu4e-confirm-quit nil
        mu4e-view-show-addresses t
        mu4e-headers-skip-duplicates t
        mu4e-headers-auto-update t
        mu4e-auto-retrieve-keys t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-complete-only-personal t
        mu4e-sent-folder "/"
        mu4e-view-show-images t
        mu4e-org-contacts-file "~/files/org/contacts.org"
        mu4e-get-mail-command "getmail -rgetmail_home -rgetmail_work")

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "gmail"
             :enter-func (lambda () (mu4e-message "Entering Gmail context"))
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "mankaev@gmail.com")))
             :vars '( ( user-mail-address            . "mankaev@gmail.com")
                      ( user-full-name               . "Ilya Mankaev")
                      ( mu4e-compose-signature       . "Ilya Mankaev\n")
                      ( system-name                  . "mankaev")
                      ( mu4e-sent-messages-behavior  . delete)
                      ( epg-user-id                  . "C71CD9843FE0986C61CC26722CBACD9B90C9D091")
                      ( smtpmail-stream-type         . starttls)
                      ( smtpmail-default-smtp-server . "smtp.gmail.com")
                      ( smtpmail-smtp-server         . "smtp.gmail.com")
                      ( smtpmail-smtp-service        . 587)))
           ,(make-mu4e-context
             :name "alliance"
             :enter-func (lambda () (mu4e-message "Entering Alliance context"))
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "i@totravel.online")))
             :vars '( ( user-mail-address            . "i@totravel.online")
                      ( user-full-name               . "Ilia Mankaev")
                      ( mu4e-compose-signature       . "Ilia Mankaev\nAlliance Online CTO")
                      ( system-name                  . "totravel.online")
                      ( mu4e-sent-messages-behavior  . sent)
                      ( epg-user-id                  . "6589D1B2C5A8A48A78758C763BBB1BA19FB0378D")
                      ( smtpmail-stream-type         . starttls)
                      ( smtpmail-default-smtp-server . "smtp.yandex.com")
                      ( smtpmail-smtp-server         . "smtp.yandex.com")
                      ( smtpmail-smtp-service        . 587)))))

  (setq mu4e-headers-fields '((:human-date . 8)
                              (:from . 35)
                              (:thread-subject . 80))
        mu4e-view-fields '(:from
                           :to
                           :cc
                           :bcc
                           :date
                           :subject
                           :signature
                           :attachments))

  (require 'smtpmail-async)
  (setq mail-user-agent 'mu4e-user-agent
        read-mail-command    'mu4e
        gnus-dired-mail-mode 'mu4e-user-agent)
  (setq message-send-mail-function 'async-smtpmail-send-it
        message-required-mail-headers '(From Subject Date (optional . In-Reply-To))
        mail-specify-envelope-from t  ; Use from field to specify sender name.
        message-kill-buffer-on-exit t
        message-citation-line-format "On %a %d %b %Y at %R UTC, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-cite-reply-position 'above
        mail-envelope-from 'header)   ; otherwise `user-mail-address' is used.
  (setq mml2015-use 'epg
        mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-always-trust nil
        mml-secure-cache-passphrase t
        mml-secure-passphrase-cache-expiry '36000
        mml-secure-openpgp-sign-with-sender t
        mm-verify-option 'always
        mm-decrypt-option 'always)

  ;; Use Org structures and tables in message mode
  (add-hook 'message-mode-hook
            (lambda ()
              (turn-on-orgtbl)
              (turn-on-orgstruct++)))
  (add-hook 'message-send-hook
            (lambda ()
              ;; (mml-secure-message-sign-encrypt)
              (mml-secure-message-sign)))

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (set-fill-column 65)
              (flyspell-mode)
              (epa-mail-mode)))

  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

  (add-to-list 'mu4e-view-actions
               '("browse mail" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  :diminish (epa-mail-mode orgtbl-mode orgstruct-mode mml-mode))

(use-package mu4e-alert
  :defer t
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-email-notification-types '(subjects)))

(use-package evil-mu4e
  :defer t
  :after mu4e)

(use-package evil-nerd-commenter
  :defer t
  :init
  (evilnc-default-hotkeys))

(use-package google-translate
  :defer t
  :init
  (setq google-translate-default-target-language "ru"
        google-translate-default-source-language "en"
        google-translate-output-destination 'echo-area))

(provide '.emacs)
;;; .emacs ends here
