;;; .emacs --- Emacs configuration file
;;; Package-requires: ((emacs "27.1"))

;;; Commentary:

;;; Code:
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "This config requires >=%s version of Emacs" minver)))

(defvar my-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Reset threshold to its default after Emacs has startup, because a large
            ;; GC threshold equates to longer delays whenever GC happens
            (setq gc-cons-threshold 800000
                  file-name-handler-alist my-file-name-handler-alist)
            (kill-buffer "*scratch*")))

(advice-add 'display-startup-echo-area-message :override #'ignore)

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))
(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(require 'socks)
(setq url-gateway-method 'socks
      socks-noproxy '("localhost")
      socks-server '("TOR" "localhost" 9050 5)
      socks-password "")

(setq tls-checktrust t
      tls-program '("gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h" "gnutls-cli -p %p %h")
      gnutls-verify-error t
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      gnutls-min-prime-bits 3072
      network-security-level 'medium
      nsm-save-host-names t)

(setq auth-sources '("~/.authinfo.gpg"))

;; This makes long-line buffers usable
(setq-default bidi-display-reordering nil)

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 2)

(setq-default line-spacing 0)
(setq-default line-move-visual nil)
;; Focus new help windows when opened
(setq-default help-window-select t)

;; Turn off annoying settings
(blink-cursor-mode -1)
(tooltip-mode -1)
(line-number-mode -1)

;; Cursor stretches to the current glyph's width
(setq x-stretch-cursor t)
;; Underline below the font bottomline instead of the baseline
(setq x-underline-at-descent-line t)
(setq underline-minimum-offset 0)
(setq x-gtk-use-system-tooltips nil)        ; Use Emacs tooltips
(setq-default cursor-in-non-selected-windows nil)   ; Keep cursors and highlights in current window only
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; slightly less to process at startup.
(setq command-line-x-option-alist nil)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Do not create lock files
(setq create-lockfiles nil)

(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(setq scroll-conservatively 1000              ;; Move to beg/end of buffer before signalling an error
      scroll-error-top-bottom t               ;; Ensure M-v always undoes C-v
      scroll-preserve-screen-position 'always ;; Start recentre from top
      recenter-positions '(top middle bottom) ;; Disable mouse scrolling acceleration
      mouse-wheel-progressive-speed nil)

(setq display-hourglass nil)

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

;; Fonts used:
(set-fontset-font t nil (font-spec :name "Pragmata Pro" :size 18))
(set-fontset-font t 'symbol (font-spec :name "Noto Color Emoji" :size 18))

(setq font-lock-maximum-decoration t)
;; Prevent emacs from creating a backup file filename~
(setq make-backup-files nil)

;; Do not confirm before quitting Emacs
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

(setq echo-keystrokes 0.1) ; Faster echo keystrokes

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
      initial-buffer-choice nil
      initial-scratch-message nil)

;; Copy to system clipboards
(setq select-enable-clipboard t
      select-enable-primary t)

(setq save-interprogram-paste-before-kill t)

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows t)

(setq pop-up-frames nil)                ; No popup frames
(setq pop-up-frame-function (lambda () (selected-frame)))
(setq pop-up-windows nil)               ; No popup windows

(setq frame-resize-pixelwise t ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; (setq enable-local-variables :all)
(setq shift-select-mode nil)
(setq search-whitespace-regexp nil)

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq initial-major-mode 'emacs-lisp-mode)

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-defer t
              use-package-compute-statistics t
              use-package-always-ensure t
              use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))

;;; Built-in packages

(use-package server                     ; The server of `emacsclient'
  :ensure nil
  :hook (after-init . server-start))

(use-package display-time
  :ensure nil
  :init
  (setq display-time-format "  %H:%M"
        display-time-mail-string ""
        display-time-default-load-average nil)
  (display-time-mode))

(use-package battery
  :ensure nil
  :init
  (setq battery-mode-line-format "%b%p%%")
  (display-battery-mode))

(use-package fortune
  :ensure nil
  :init (setq fortune-file (expand-file-name "~/.config/emacs/lambda.txt")))

(use-package so-long
  :ensure nil
  :diminish so-long-mode)

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode)

(use-package hl-line
  :ensure nil
  :hook (prog-mode . hl-line-mode)
  :config (setq hl-line-sticky-flag nil))

(use-package hexl-mode
  :ensure nil
  :magic ("ELF" . hexl-mode))

(use-package text-mode
  :ensure nil
  :hook (text-mode . auto-fill-mode)
  :config (setq-default fill-column 65)
  :diminish auto-fill-function)

(use-package grep
  :ensure nil
  ;; Truncate lines during grep
  :hook (grep-mode . toggle-truncate-lines))

(use-package desktop
  :ensure nil
  :init
  (setq desktop-save t
        desktop-load-locked-desktop t)
  (desktop-save-mode 1))

(use-package arc-mode
  :ensure nil
  :hook (archive-extract . read-only-mode))

(use-package ediff-wind                 ; Ediff window management
  :ensure nil
  :config
  ;; Restore window/buffers when you're finishd ediff-ing.
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package eww
  :ensure nil
  :init
  (setq eww-header-line-format nil
        browse-url-browser-function 'browse-url-xdg-open
        image-use-external-converter t
        shr-width 65
        shr-inhibit-images t
        shr-use-colors nil
        shr-use-fonts nil)
  (set-face-font 'variable-pitch "PT Serif-20"))

(use-package shr-tag-pre-highlight
  :init
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))
  (add-to-list 'shr-external-rendering-functions '(code . shr-tag-pre-highlight))
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :hook (list-diary-entries . sort-diary-entries)
  :config
  (setq calendar-date-style 'european
        calendar-standard-time-zone-name "MSK"
        calendar-latitude 55.8
        calendar-longitude 37.6
        calendar-week-start-day 1
        calendar-holidays nil)
  (setq diary-mail-addr "mankaev@gmail.com"
        number-of-diary-entries 7))

(use-package proced-mode
  :ensure nil
  :init
  (setq proced-auto-update-interval 1
        proced-auto-update-flag t
        proced-format 'long
        proced-sort 'pmem))

(use-package etags                      ; Tag navigation
  :ensure nil
  :config
  ;; Do not query before reverting TAGS tables
  (setq tags-revert-without-query t))

(use-package imenu
  :ensure nil
  :config (setq imenu-auto-rescan t))

(use-package sh-script                  ; Shell scripts
  :ensure nil
  :hook (sh-mode . (lambda ()
                     (flycheck-mode)))
  :mode ("\\.zsh\\'" . sh-mode)
  :bind (:map sh-mode-map
              ("M-s j"   . eshell-toggle))
  :init (setq sh-basic-offset 2))     ; The offset for nested indentation

(use-package gnus
  :ensure nil
  :config
  (require 'gnus-icalendar)
  (setq gnus-inhibit-images t)
  (setq gnus-icalendar-org-capture-file "~/files/org/todo.org")
  (gnus-icalendar-setup)
  (gnus-icalendar-org-setup))

(use-package mu4e
  :ensure nil
  :commands mu4e
  :hook ((message-mode . (lambda () ;; Use Org structures and tables in message mode
                           (turn-on-orgtbl)
                           (orgalist-mode)))
         (message-send . (lambda ()
                           ;; (mml-secure-message-sign-encrypt)
                           ;; (mml-secure-message-sign)
                           ))
         (mu4e-compose-mode . (lambda ()
                                (set-fill-column 65)
                                (flyspell-mode)
                                (epa-mail-mode)))
         (mu4e-view-mode . epa-mail-mode))
  :config
  (setq mu4e-view-use-gnus t)
  (require 'mu4e-icalendar)
  (require 'mu4e)
  (mu4e-icalendar-setup)
  (setq mu4e-maildir "~/mail"
        mu4e-attachment-dir "~/tmp"
        mu4e-compose-complete-only-personal t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-get-mail-command "getmail -rgetmail_home"
        mu4e-headers-auto-update t
        mu4e-headers-skip-duplicates t
        mu4e-hide-index-messages t
        mu4e-org-contacts-file "~/files/org/contacts.org"
        mu4e-sent-folder "/"
        mu4e-split-view 'same-window
        mu4e-update-interval 180
        mu4e-user-agent-string "Emacs"
        mu4e-headers-include-related nil
        mu4e-user-mail-address-list '("mankaev@gmail.com")
        mu4e-view-image-max-width 800
        mu4e-view-show-addresses t
        mu4e-view-show-images nil
        mu4e-completing-read-function 'ivy-completing-read)

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
                      ;; ( mu4e-sent-messages-behavior  . delete)
                      ( epg-user-id                  . "C71CD9843FE0986C61CC26722CBACD9B90C9D091")
                      ( smtpmail-stream-type         . starttls)
                      ( smtpmail-default-smtp-server . "smtp.gmail.com")
                      ( smtpmail-smtp-server         . "smtp.gmail.com")
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

  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          ("maildir:/trash" "Trash" ?s)
          ("maildir:/drafts" "Drafts" ?d)))
  (require 'smtpmail-async)
  (setq mail-user-agent 'mu4e-user-agent
        read-mail-command    'mu4e
        gnus-dired-mail-mode 'mu4e-user-agent)
  (setq message-send-mail-function 'async-smtpmail-send-it
        message-required-mail-headers '(From Subject Date (optional . In-Reply-To))
        mail-specify-envelope-from t ; Use from field to specify sender name.
        message-kill-buffer-on-exit t
        message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-cite-reply-position 'above
        mail-envelope-from 'header) ; otherwise `user-mail-address' is used.
  (setq mml2015-use 'epg
        mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-always-trust nil
        mml-secure-cache-passphrase t
        mml-secure-passphrase-cache-expiry '36000
        mml-secure-openpgp-sign-with-sender t
        mm-verify-option 'always
        mm-decrypt-option 'always)

  (add-to-list 'mu4e-view-actions
               '("browse mail" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  :diminish (overwrite-mode epa-mail-mode orgtbl-mode orgalist-mode mml-mode))

(use-package eshell                     ; Emacs command shell
  :ensure nil
  :bind (("M-s t"   . eshell-toggle))
  :hook (eshell-mode . (lambda ()
                         (company-mode)
                         (setq-local company-backends '(company-capf company-files company-sh))
                         (setq-local company-idle-delay 0.7)
                         ;; Hack to define key in eshell-mode
                         (define-key eshell-mode-map (kbd "M-s") nil)
                         (define-key eshell-mode-map (kbd "M-s t") 'eshell-toggle)
                         (define-key eshell-mode-map (kbd "C-l") 'eshell-truncate-buffer)
                         (define-key eshell-mode-map [tab] 'company-indent-or-complete-common)))
  :init
  (setq eshell-banner-message ""
        eshell-buffer-maximum-lines 0
        eshell-destroy-buffer-when-process-dies t
        eshell-error-if-no-glob t
        eshell-highlight-prompt nil
        eshell-hist-ignoredups t
        eshell-history-size 1024
        eshell-prefer-lisp-functions t
        eshell-prompt-function (lambda () "$ ")
        eshell-prompt-regexp "^$ "
        eshell-save-history-on-exit t
        eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-visual-commands '("less" "more" "htop" "mc" "ncmpcpp"))

  (defun eshell/magit ()
    "Function to open magit-status for the current directory"
    (magit-status-internal "."))

  (defun eshell-toggle ()
    "Go to eshell and set current directory to the buffer's directory"
    (interactive)
    (let* ((dir (file-name-directory (or (buffer-file-name)
                                         default-directory)))
           (target-buf (get-buffer-create "*eshell*"))
           (target-window (get-buffer-window target-buf)))
      (if target-window    ;hide window if target buffer is shown
          (if (one-window-p)
              (quit-window)
            (delete-window target-window))
        (progn
          (setq-local eshell-buffer-name target-buf)
          (eshell)
          (goto-char (point-max))
          (eshell-kill-input)
          (unless (string= (concat (eshell/pwd) "/") dir)
            (insert (concat "cd \"" dir "\""))
            (eshell-send-input)))))))

(use-package eldoc                      ; Documentation in the echo area
  :ensure nil
  :hook (eval-expression-minibuffer-setup . eldoc-mode)
  :config
  ;; Enable Eldoc for `eval-expression', too
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  (setq eldoc-idle-delay 0.1)  ; Show eldoc more promptly
  (global-eldoc-mode -1)
  :diminish eldoc-mode)

(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode t)
  (setq abbrev-file-name (expand-file-name ".abbrev_defs" user-emacs-directory)
        save-abbrevs 'silently)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  :diminish abbrev-mode)

(use-package cc-mode
  :ensure nil
  :hook ((c++-mode . (lambda ()
                       (aggressive-indent-mode -1)
                       (lsp)))
         (c-mode . (lambda ()
                     (aggressive-indent-mode -1)
                     (lsp))))
  :bind (:map c-mode-map
              ("C-c C-c" . projectile-compile-project)
              :map c++-mode-map
              ("C-c C-c" . projectile-compile-project))
  :config
  (require 'smartparens-c)
  (setq c-basic-offset 2))

(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-map
              ([escape] . minibuffer-keyboard-quit)
              :map minibuffer-local-ns-map
              ([escape] . minibuffer-keyboard-quit)
              :map minibuffer-local-completion-map
              ([escape] . minibuffer-keyboard-quit)
              :map minibuffer-local-must-match-map
              ([escape] . minibuffer-keyboard-quit)
              :map minibuffer-local-isearch-map
              ([escape] . minibuffer-keyboard-quit)
              :map minibuffer-inactive-mode-map
              ;; Do not pop up *Messages* when clicking on the minibuffer
              ([mouse-1] . ignore))
  :config
  (setq enable-recursive-minibuffers t     ; Allow to read from minibuffer while in minibuffer.
        use-dialog-box nil                 ; Never use dialogs for minibuffer input
        use-file-dialog nil
        completions-format 'vertical
        read-answer-short t
        completion-show-help nil)
  (minibuffer-depth-indicate-mode t))      ; Show the minibuffer depth (when larger than 1)

(use-package erc
  :ensure nil
  :hook  ((erc-mode . (lambda ()
                        (setq-local company-backends '(company-capf))
                        (company-mode)))
          (erc-text-matched . erc-global-notify))
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
        erc-log-channels-directory "~/.config/emacs/erc"
        erc-query-display 'buffer
        erc-join-buffer 'bury
        erc-prompt-for-nickserv-password nil ; Do not ask for password
        erc-prompt-for-password nil ; Do not ask for password
        erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-interpret-mirc-color t ; Interpret mIRC-style color commands in IRC chats
        erc-kill-buffer-on-part t ; Kill buffers for channels after /part
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
      (notifications-notify :title nick :body message :urgency 'normal))))

(use-package gdb-mi
  :ensure nil
  :config
  (setq
   gdb-display-io-nopopup t
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(use-package nxml-mode                  ; XML editing
  :ensure nil
  :hook (nxml-mode . (lambda ()
                       (setq-local company-backends '(company-nxml))))
  :config
  ;; Complete closing tags, and insert XML declarations into empty files
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t
        ;; Treat elements (with children) as sexps
        nxml-sexp-element-flag t))

(use-package sql                        ; SQL editing and REPL
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :hook (sql-interactive-mode . (lambda ()
                                  (toggle-truncate-lines t))))

(use-package ielm-mode
  :ensure nil
  :hook (ielm-mode . (lambda ()
                       (setq-local company-backends '(company-capf))
                       (company-mode))))

(use-package elisp-mode                 ; Emacs Lisp editing
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ([C-return] . elisp-def)
              ("C-c C-c"  . compile-defun)
              ("M-s j"    . projectile-run-ielm))
  :hook ((emacs-lisp-mode . (lambda ()
                              (elsa-setup-font-lock)
                              (flycheck-elsa-setup)
                              (setq-local company-backends '(company-capf))))
         (ielm-mode . (lambda ()
                        (setq-local comint-input-ring-file-name "~/.config/emacs/.ielm-input.hist"))))
  :interpreter ("emacs" . emacs-lisp-mode))

(use-package dired                      ; File manager
  :after evil
  :ensure nil
  :bind (("M-s d"   . dired-toggle)
         :map dired-mode-map
         ([return]   . dired-find-alternate-file)
         ([C-return] . open-in-external-app))
  :hook ((dired-mode . turn-on-gnus-dired-mode)
         (dired-mode . image-dired-minor-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-toggle (&optional dir)
    "Toggle current buffer's directory."
    (interactive)
    (let* ((win (selected-window))
           (buf (buffer-name))
           (file (buffer-file-name))
           (dir (or dir (if file (file-name-directory file) default-directory)))
           (target-bufname "*dired*")
           (target-buf (get-buffer-create target-bufname))
           (target-window (get-buffer-window target-buf))
           (dired-buffer-with-same-dir (dired-find-buffer-nocreate dir))
           (new-dired-buffer-p
            (or (not dired-buffer-with-same-dir)
                (not (string= target-bufname
                              (buffer-name dired-buffer-with-same-dir))))))
      (if target-window
          (if (one-window-p)
              (quit-window)
            (delete-window target-window))
        (progn
          (switch-to-buffer target-buf)
          (with-current-buffer target-buf
            (if new-dired-buffer-p
                (progn
                  (setq default-directory dir)
                  (if (eq 'dired-mode major-mode)
                      (setq dired-directory dir)
                    (funcall 'dired-mode dir))
                  (unwind-protect
                      (progn (dired-readin)))))
            (setq-local dired-toggle-refwin win)
            ;; try to select target file
            (if file (dired-goto-file file)))))))
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
        dired-ls-F-marks-symlinks t)
  (dired-async-mode t))

(use-package uniquify                   ; Unique buffer names
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t      ; Rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*")) ; Ignore special buffers

(use-package whitespace-mode
  :ensure nil
  :hook prog-mode
  :hook (before-save . whitespace-cleanup)
  :init (setq whitespace-style '(empty face trailing tab-mark)))

;;; Installed Packages

(use-package diminish
  :after use-package)

;; Theme
(use-package spacemacs-theme
  :init
  (setq custom-safe-themes t)           ; Treat themes as safe
  (setq spacemacs-theme-comment-italic t
        spacemacs-theme-comment-bg nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-bold nil
        spacemacs-theme-underline-parens nil)
  (load-theme 'spacemacs-light 'no-confirm))

(use-package racket-mode                ; Racket language mode
  :mode "\\.rkt\\'"
  :hook ((racket-repl-mode . (lambda ()
                               (setq-local company-backends '(company-capf))
                               (company-mode)))
         (racket-mode . (lambda ()
                          (setq-local company-backends '(company-capf)))))
  :bind (:map racket-mode-map
              ("M-s j" . racket-run)))

(use-package evil
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
              ("S-SPC"  . avy-goto-char)
              ("SPC"    . avy-goto-word-1)
              ("C-SPC"  . avy-goto-line)
              ([escape] . keyboard-quit)
         :map evil-operator-state-map
              ([escape] . evil-normal-state)
         :map evil-emacs-state-map
              ([escape] . evil-normal-state)
         :map evil-visual-state-map
              ([escape] . keyboard-quit)
         :map evil-motion-state-map
              ([escape] . evil-normal-state))
  :init
  (setq evil-emacs-state-cursor '("red" box)
        evil-normal-state-cursor '("red" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("red" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))
  (setq evil-want-keybinding nil
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-undo-system 'undo-fu))

(use-package evil-ediff
  :after evil)

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng t)
  (evil-collection-init))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-height 40
        doom-modeline-bar-width 10
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'header-line nil :box nil))

(use-package page-break-lines           ; Better looking break lines
  :config (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package avy)

(use-package aggressive-indent
  :after smartparens
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'slime-repl-mode)
  :pin melpa-stable)

(use-package smartparens                ; Parenthesis editing and balancing
  :init
  (setq sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil ;; Don't kill entire symbol on C-k
        sp-message-width nil ;; Disable debug messages
        sp-show-pair-from-inside t
        ;; Keep pair content overlay on backward movement
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0)
  (show-smartparens-global-mode t)
  :diminish smartparens-mode)

(use-package smartparens-config         ; Configure Smartparens
  :ensure smartparens
  :after smartparens
  :hook ((prog-mode
          slime-repl-mode
          ielm-mode
          cider-repl-mode) . smartparens-strict-mode)
  :hook (smartparens-mode . (lambda ()
                              (add-hook 'before-save-hook 'check-parens nil 'local)))
  :bind (:map smartparens-mode-map
              ("M-r"          . sp-raise-sexp)
              ("M-<delete>"   . sp-unwrap-sexp)
              ("M-("          . sp-wrap-round)
              ("M-{"          . sp-wrap-curly)
              ("M-["          . sp-wrap-square)
              ("C-)"          . sp-forward-sexp)
              ("C-("          . sp-backward-sexp)
              ("C-<down>"     . sp-down-sexp)
              ("C-<up>"       . sp-up-sexp)
              ("M-<down>"     . sp-backward-down-sexp)
              ("M-<up>"       . sp-backward-up-sexp)
              ("C-M-k"        . sp-kill-sexp)
              ("C-M-y"        . sp-copy-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              :map smartparens-strict-mode-map
              ("M-q"          . sp-indent-defun))
  :init
  (dolist (mode `(,@sp-lisp-modes minibuffer-inactive-mode))
    (sp-local-pair mode "'" nil :actions nil)
    (sp-local-pair mode "`" nil :actions nil)))

(use-package diff-hl                    ; Show changes in fringe
  :hook (prog-mode . diff-hl-mode)
  :init (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode -1)
  (diff-hl-flydiff-mode -1)
  :pin melpa-stable)

(use-package hi-lock                    ; Custom regexp highlights
  :config (global-hi-lock-mode))

(use-package savehist                   ; Save minibuffer history
  :hook (after-init . (lambda () (savehist-mode t)))
  :init
  (setq history-length 1000             ; Store more history
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-autosave-interval 180))

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :init
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-style 'always
        eyebrowse-new-workspace t
        eyebrowse-wrap-around t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode))

(use-package winner                     ; Winner mode
  :hook (after-init . winner-mode))

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (global-undo-fu-session-mode)
  :diminish undo-fu-session-mode)

(use-package delsel                     ; Delete the selection instead of insert
  :config (delete-selection-mode))

(use-package subword                    ; Handle capitalized subwords
  :config
  (global-subword-mode t)
  ;; Do not override `transpose-words', it should not transpose subwords
  (bind-key [remap transpose-words] nil subword-mode-map)
  :diminish subword-mode)

(use-package saveplace                  ; Save point position in files
  :config (save-place-mode t))

(use-package super-save                 ; Autosave buffers when they lose focus
  :init
  (setq super-save-auto-save-when-idle t
        super-save-remote-files nil)
  (setq auto-save-list-file-prefix     "~/.config/emacs/autosave/"
        auto-save-file-name-transforms '((".*" "~/.config/emacs/autosave/" t))
        auto-save-default nil)
  (super-save-mode)
  :diminish super-save-mode)

(use-package bookmark                   ; Bookmarks to files and directories
  :config
  (setq bookmark-completion-ignore-case nil))
  ;;(bookmark-maybe-load-default-file))

(use-package flycheck                   ; On-the-fly syntax checker
  :init
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-check-syntax-automatically '(idle-change)
        flycheck-idle-change-delay 1)
  :diminish flycheck-mode)

(use-package recentf                    ; Manage recent files
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

(use-package markdown-mode              ; Edit markdown files
  :hook (markdown-mode . auto-fill-mode)
  :mode "\\.markdown\\.md\\'"
  :config
  (require 'smartparens-markdown)
  (dolist (face '(markdown-header-face-1
                  markdown-header-face-2
                  markdown-header-face-3
                  markdown-header-face-4
                  markdown-header-face-5
                  markdown-header-face-6))
    (set-face-attribute face nil :weight 'normal))
  (setq markdown-fontify-code-blocks-natively t
        markdown-coding-system 'utf-8
        markdown-header-scaling nil))

(use-package autorevert
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1)
  :diminish auto-revert-mode)

(use-package company                    ; Auto-completion
  :bind (([tab] . company-indent-or-complete-common)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook ((prog-mode text-mode) . company-mode)
  :config
  (setq company-idle-delay 0.3
        company-ispell-dictionary ispell-dictionary
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t ;; Easy navigation to candidates with M-<n>
        company-show-numbers t            ;; Ignore case
        company-backends nil              ;; Do not set default backends
        company-dabbrev-ignore-case t     ;; Do not downcase completion
        company-dabbrev-downcase nil)
  :diminish company-mode)

(use-package company-web                ; Backend for web development
  :after company)

(use-package company-jedi               ; Python backend for Company
  :after company
  :config
  (setq jedi:complete-on-dot t
        jedi:imenu-create-index-function 'jedi:create-flat-imenu-index))

(use-package company-restclient         ; Company support for restclient
  :after company)

(use-package flyspell                   ; Spell checking on-the-fly
  :if (executable-find "hunspell")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  (setq ispell-really-hunspell t
        ispell-program-name (executable-find "hunspell")
        ispell-check-comments  t
        ispell-dictionary "gbru"
        ispell-local-dictionary-alist
        '(("gbru" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB,ru_RU") nil utf-8))
        ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
  (setq flyspell-use-meta-tab nil
        flyspell-abbrev-p t
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  :diminish flyspell-mode)

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package projectile                 ; Project management
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-globally-ignored-files '("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")
        projectile-find-dir-includes-top-level t)

  (projectile-mode)
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  :diminish projectile-mode
  :pin melpa-stable)

(use-package magit                      ; The best Git client out there
  :init
  (setq vc-handled-backends '(Git)
        vc-follow-symlinks nil
        magit-completing-read-function 'ivy-completing-read
        magit-delete-by-moving-to-trash nil
        magit-diff-use-overlays nil
        magit-refs-show-commit-count 'all
        magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask
        magit-use-overlays nil
        git-commit-fill-column 65
        git-commit-summary-max-length 65
        git-commit-finish-query-functions nil)
  ;; Show status buffer in fullscreen
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package evil-magit
  :after (evil evil-collection magit)
  :config
  (defun my-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :init
  (evil-magit-init)
  (evil-collection-define-key 'normal
    'magit-status-mode-map "q" 'my-magit-kill-buffers))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))


(use-package git-commit                 ; Git commit message mode
  :config
  (global-git-commit-mode)
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package git-timemachine)           ; Git timemachine

(use-package gitconfig-mode)            ; Git configuration mode

(use-package gitignore-mode)            ; .gitignore mode

(use-package gitattributes-mode)        ; Git attributes mode

(use-package tramp                      ; Remote editing
  :config
  (setq auto-save-file-name-transforms nil
        tramp-default-method "ssh"
        tramp-adb-connect-if-not-connected t)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(use-package org-contacts
  :ensure org-plus-contrib
  :init
  (setq org-contacts-files '("~/files/org/contacts.org")
        org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY"))

(use-package org                        ; Org Plus Contributions
  :ensure org-plus-contrib
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-files company-dabbrev))))
         (org-babel-after-execute . org-display-inline-images))
  :config
  (require 'smartparens-org)
  ;; (setq org-modules '(org-eshell org-protocol org-habit org-irc ol-eww ol-bookmark ol-elisp-symbol ol-man org-notify))
  (setq org-modules '(org-protocol org-habit ol-eww ol-bookmark ol-elisp-symbol ol-man org-notify))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t)
     (clojure . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (lisp . t)
     (scheme . t)
     (sql . t)
     (plantuml . t)
     (python . t)
     (shell . t)))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-adapt-indentation nil
        org-agenda-compact-blocks t
        org-agenda-files '("~/files/org/todo.org" "~/files/org/work.org" "~/files/org/notes.org")
        org-agenda-include-diary t
        org-agenda-span 10
        org-agenda-start-day "-3d"
        org-agenda-start-on-weekday nil
        org-agenda-window-setup 'current-window
        org-babel-clojure-backend 'cider
        ;; org-babel-lisp-eval-fn 'sly-eval
        org-confirm-babel-evaluate nil
        org-crypt-disable-auto-save 'encrypt
        org-crypt-key "C71CD9843FE0986C61CC26722CBACD9B90C9D091"
        org-default-notes-file (concat org-directory "/todo.org")
        org-directory (expand-file-name "~/files/org")
        org-display-inline-images t
        org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
        org-edit-src-content-indentation 0
        org-export-coding-system 'utf-8
        org-hide-emphasis-markers t
        org-html-inline-images t
        org-link-descriptive nil
        org-log-done 'time
        org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
        org-redisplay-inline-images t
        org-return-follows-link t       ; Follow links by pressing ENTER on them
        org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-src-tab-acts-natively t
        org-tags-exclude-from-inheritance '("crypt"))
  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?A)

  ;; # -*- eval: (save-excursion (org-babel-goto-named-src-block "startblock") (org-babel-execute-src-block)); -*-
  (defun org-insert-file (filename)
    "Insert Elisp code block recreating file named FILENAME."
    (interactive "f")
    (let ((base64-string (with-temp-buffer
                           (insert-file-contents-literally filename)
                           (base64-encode-region (point-min) (point-max))
                           (buffer-string))))
      (insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))
  ;; Define TODO workflow states
  (setq org-todo-keywords '("TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)")))

(use-package org-capture                ; Fast note taking in Org
  :ensure org-plus-contrib
  :config
  (setq org-capture-templates
        '(("w" "Web captures" entry (file+headline "~/files/org/notes.org" "Inbox")
           "* %^{Title}\nCaptured On: %U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
           :empty-lines 1)
          ("L" "Protocol Link" entry (file+headline "~/files/org/notes.org" "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ("c" "Contacts" entry (file "~/files/org/contacts.org")
           "** %(org-contacts-template-name)
:PROPERTIES:%(org-contacts-template-email)
:END:")
          ("t" "TODO" entry (file "~/files/org/todo.org")
           "* TODO %^{Task}  %^G\n   %?"))))

(use-package orgalist
  :ensure org-plus-contrib)

(use-package ox-html
  :ensure org-plus-contrib
  :config
  ;; Turn off preamble and postamble in HTML export
  (setq org-html-preamble nil
        org-html-postamble nil))

(use-package org-notify
  :ensure org-plus-contrib
  :hook (after-init . (lambda ()
                        (require 'org-notify)
                        (org-notify-start)
                        (org-notify-add 'single '(:time "60m"  :actions -notify/window)))))

(use-package ox
  :ensure org-plus-contrib
  :config
  (setq org-export-with-timestamps nil
        org-export-with-smart-quotes t))

(use-package ox-hugo
  :after ox)

(use-package ob-async
  :after ox
  :init
  (require 'ob-async)
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package ox-reveal
  :config (require 'ox-reveal))

(use-package gnuplot)

(use-package gnuplot-mode)

;;; Programming utilities
(use-package python                     ; Python editing
  :bind (:map python-mode-map
              ([C-return] . jedi:goto-definition))
  :hook ((inferior-python-mode . company-mode)
         (python-mode . (lambda ()
                          (setq-local company-backends '(company-jedi))
                          (setq fill-column 79)))
         (ein:connect-mode . ein:jedi-setup))
  :config
  (setq python-indent-offset 2)
  (jedi:setup))

(use-package elisp-def
  :hook ((emacs-lisp-mode
          ielm-mode) . elisp-def-mode)
  :diminish elisp-def-mode)

(use-package helpful
  :after evil
  :init
  (evil-collection-define-key '(visual normal) 'emacs-lisp-mode-map "K" 'helpful-at-point))

(use-package cider                      ; Clojure development environment
  :after clojure-mode
  :hook ((cider-stacktrace-mode . (lambda ()
                                    (cider-stacktrace-cycle-cause 2 1)))
         (cider-connected . (lambda ()
                              (cider-repl-clear-banners))))
  :config
  (setq cider-stacktrace-default-filters '(clj java repl tooling dup)
        ;; Do not offer to open ClojureScript app in browser
        cider-offer-to-open-cljs-app-in-browser nil)
  :pin melpa-stable)

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :after evil
  :ensure cider
  :hook (cider-mode . cider-company-enable-fuzzy-completion)
  :bind (:map cider-mode-map
              ("M-s r" . cider-ns-reload)
              ([C-return] . cider-find-var))
  :config
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (evil-set-initial-state 'cider-browse-ns-mode 'motion)
  (evil-set-initial-state 'cider--debug-mode 'emacs)
  (evil-set-initial-state 'cider-docview-mode 'motion)
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-repl-history-mode 'motion)
  (setq cider-show-eval-spinner nil
        cider-prompt-for-symbol nil)
  :diminish cider-mode)

(use-package clojure-mode               ; Major mode for Clojure files
  :hook (clojure-mode . (lambda ()
                          (setq-local completion-at-point-functions
                                      (remove 'tags-completion-at-point-function
                                              completion-at-point-functions))
                          (setq-local company-backends '(company-capf))))
  :bind (:map clojure-mode-map
              ("M-s j" . cider-jack-in)
              ("M-s J" . cider-jack-in-cljs))
  :init
  (require 'clojure-mode)
  :config
  (setq clojure-align-forms-automatically t))

(use-package clojure-mode-extra-font-locking ; Font-locking for Clojure mode
  :after clojure-mode)

(use-package nrepl-client              ; Client for Clojure nREPL
  :ensure cider
  :config
  (setq nrepl-hide-special-buffers t
        nrepl-prompt-to-kill-server-buffer-on-quit nil))

(use-package cider-repl            ; REPL interactions with CIDER
  :ensure cider
  :hook (cider-repl-mode . (lambda ()
                             (company-mode)
                             (hs-minor-mode)
                             (setq-local completion-at-point-functions
                                         (remove 'tags-completion-at-point-function
                                                 completion-at-point-functions))
                             (define-key cider-repl-mode-map (kbd "M-s") nil)
                             (setq-local company-backends '(company-capf))))
  :bind (:map cider-repl-mode-map
              ("C-l" . cider-repl-clear-buffer))
  :config
  (setq cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
        cider-repl-history-display-style 'one-line
        cider-repl-display-help-banner nil
        cider-repl-history-display-duplicates nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t))

(use-package clj-refactor               ; Refactoring utilities
  :hook (cider-mode . clj-refactor-mode)
  :config
  (setq cljr-suppress-middleware-warnings t
        cljr-add-ns-to-blank-clj-files t
        cljr-auto-sort-ns t
        cljr-favor-prefix-notation cljr-favor-private-functions
        cljr-warn-on-eval nil)
  :diminish clj-refactor-mode)

(use-package clojure-snippets           ; Yasnippets for Clojure
  :after (yasnippet clojure-mode))

(use-package geiser                    ; Geiser mode
  :init
  (setq geiser-default-implementation 'chez
        geiser-active-implementations '(chez guile)
        geiser-repl-use-other-window nil
        geiser-repl-history-filename "~/.config/emacs/geiser-history")
  :diminish (geiser-mode geiser-autodoc-mode))

(use-package elsa)
(use-package cask)
(use-package flycheck-elsa)

(use-package slime
  :after (projectile evil evil-collection)
  :mode ((".sbclrc\\'" . common-lisp-mode))
  :bind (:map slime-mode-map
              ([C-return] . slime-edit-definition))
  :hook (slime-repl-mode . (lambda ()
                             (hs-minor-mode)
                             (define-key slime-repl-mode-map (kbd "M-s") nil)
                             (local-set-key [(control l)] 'slime-repl-clear-buffer)
                             (setq-local company-backends '((company-slime company-yasnippet)))
                             (setq-local browse-url-browser-function 'eww-browse-url)))
  :hook (slime-mode . (lambda ()
                        (setq-local browse-url-browser-function 'eww-browse-url)
                        (setq-local company-backends '((company-slime company-yasnippet)))
                        (setq-local lisp-indent-function 'common-lisp-indent-function)
                        (setq-local lisp-lambda-list-keyword-parameter-alignment t)
                        (setq-local lisp-loop-indent-forms-like-keywords t)
                        (setq-local lisp-loop-indent-subclauses nil)
                        (eldoc-mode -1)
                        (turn-on-redshank-mode)))
  :init
  (slime-setup '(slime-fancy
                 slime-quicklisp
                 slime-asdf
                 slime-sprof
                 slime-tramp
                 slime-media
                 slime-xref-browser
                 slime-sbcl-exts
                 slime-hyperdoc
                 slime-company))

  (defadvice slime-repl-emit (around slime-repl-ansi-colorize activate compile)
    (with-current-buffer (slime-output-buffer)
      (let ((start slime-output-start))
        (setq ad-return-value ad-do-it)
        (ansi-color-apply-on-region start slime-output-end))))

  (defadvice slime-show-description (after slime-description-fontify activate)
    "Fontify sections of SLIME Description."
    (with-current-buffer "*slime-description*"
      (slime-company-doc-mode)))

  (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~")
                                           "/.docset/Common_Lisp.docset/Contents/Resources/Documents/HyperSpec/HyperSpec/")
        slime-auto-select-connection 'always
        slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
        slime-default-lisp 'sbcl
        slime-description-autofocus t
        slime-enable-evaluate-in-emacs t
        slime-fuzzy-explanation ""
        slime-highlight-compiler-notes t
        slime-inhibit-pipelining nil
        slime-kill-without-query-p t
        slime-lisp-implementations '((sbcl  ("sbcl" "--noinform" "--merge-core-pages")))
        slime-load-failed-fasl 'always
        slime-net-coding-system 'utf-8-unix
        slime-repl-history-remove-duplicates t
        slime-repl-history-trim-whitespaces t
        slime-startup-animation nil
        slime-when-complete-filename-expand t)
  (evil-collection-define-key '(visual normal) 'slime-repl-mode-map "K" 'slime-describe-symbol))

(use-package slime-company
  :after (slime company)
  :init
  (setq slime-company-completion 'fuzzy
        slime-company-major-modes '(lisp-mode slime-repl-mode)))

(use-package redshank
  :after slime)

(use-package dockerfile-mode)

(use-package sdcv
  :init
  (setq sdcv-word-pronounce nil
        sdcv-word-pronounce-command nil
        sdcv-word-pronounce-command-args nil)
  :config
  (evil-set-initial-state 'sdcv-mode 'normal)
  (evil-collection-define-key 'normal 'sdcv-mode-map "q" 'kill-buffer-and-window)
  (evil-collection-define-key '(visual normal) 'sdcv-mode-map
    (kbd "C-j") 'sdcv-next-dictionary
    (kbd "C-k") 'sdcv-previous-dictionary))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '("https://old.reddit.com/user/lispm/.rss"
          "https://www.archlinux.org/feeds/news/"
          "https://www.parabola.nu/feeds/news/"
          "http://planet.lisp.org/rss20.xml"
          "http://planet.emacslife.com/atom.xml")))

;; Dired hacks
(use-package dired-hacks-utils)
(use-package dired-filter
  :hook (dired-mode . dired-filter-mode)
  :init (setq dired-filter-stack nil))
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))
(use-package dired-ranger)
(use-package dired-imenu)
(use-package dired-narrow)

(use-package yasnippet                  ; Snippets
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 1                 ; No need to be so verbose
        yas-wrap-around-region t)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippet)

(use-package idris-mode                 ; Idris language mode
  :bind (:map idris-mode-map
              ("M-s j" . idris-load-file))
  :config
  (setq idris-repl-banner-functions nil
        idris-repl-prompt-style 'short
        idris-repl-show-idris-version nil
        idris-semantic-source-highlighting nil)
  :diminish idris-simple-indent-mode)

(use-package sqlup-mode                 ; Upcase SQL keywords
  :hook sql-mode
  :diminish sqlup-mode)

(use-package web-mode                   ; Major mode for editing web templates
  :mode ("\\.[sx]?html?\\'" "\\.s?css\\'")
  :hook (web-mode . (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (tide-setup))
                      (when (string-equal "jsx" (file-name-extension buffer-file-name))
                        (tide-setup))
                      (require 'company-web-html)
                      (setq-local company-backends '(company-web-html company-css))))
  :config
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
  :hook (js2-mode . (lambda ()
                      (setq-local company-backends '(company-tern))
                      ;; Better Imenu in j2-mode
                      (js2-imenu-extras-mode)))
  :config
  ;; Disable parser errors and strict warnings
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3))

(use-package js2-refactor               ; Refactor JavaScript
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package php-mode                   ; Better PHP support
  :hook (php-mode . (lambda ()
                      (require 'ac-php)
                      (require 'company-php)
                      (setq-local company-backends '(company-ac-php-backend))))
  :bind (:map php-mode-map
              ("C-t" . ac-php-location-stack-back)
              ([C-return] . ac-php-find-symbol-at-point))
  :mode "\\.php\\'"
  :config
  (use-package ac-php)
  (use-package company-php))

;;; Other languages
(use-package json-mode                  ; JSON editing
  :mode "\\.json\\'")

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
                     (setq-local company-backends '(company-go))))
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ([C-return] . godef-jump)))

(use-package fennel-mode
  :bind (:map fennel-mode-map
              ("M-s j" . run-lisp)))

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\.cmake\\'"
  :hook (cmake-mode . (lambda ()
                        (setq-local company-backends '(company-cmake)))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)
  :diminish modern-c++-font-lock-mode)

(use-package typescript-mode
  :mode "\\.tsx\\'")

(use-package tide
  :hook (typescript-mode . (lambda ()
                             (add-hook 'before-save-hook 'tide-format-before-save nil 'local)
                             (tide-setup)
                             (tide-hl-identifier-mode t)))
  :bind (:map tide-mode-map
              ([C-return] . tide-jump-to-definition))
  :config
  (setq tide-jump-to-definition-reuse-window t)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :diminish tide-mode)

(use-package realgud)                   ; Additional debug modes

(use-package systemd)                   ; Edit Systemd units

(use-package yaml-mode)                 ; Edit YAML files

(use-package pkgbuild-mode)             ; PKGBUILD files for ArchLinux

(use-package restclient                 ; Interactive HTTP client
  :hook (restclient-mode . (lambda ()
                             (setq-local company-backends '(company-restclient)))))

(use-package compile                    ; Compile from Emacs
  :hook (compilation-filter . colorize-compilation-buffer)
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
        (message "Compilation error, press C-x ` to visit")
      (when (string-suffix-p "compilation*" (buffer-name buf))
        (bury-buffer (buffer-name buf))
        (winner-undo)
        (message "Compiled successfully"))))

  (defun colorize-compilation-buffer ()
    "Colourize a compilation mode buffer."
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(use-package counsel
  :ensure smex
  :hook (find-file . (lambda ()
                       (when (and (not (file-writable-p buffer-file-name))
                                  (eq system-type 'gnu/linux)
                                  (eq 0 (nth 3 (file-attributes buffer-file-name))))
                         (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))
  :bind (("M-s i"   . counsel-imenu)
         ("C-x C-r" . counsel-recentf)
         ("M-s b"   . counsel-bookmark)
         ("M-y"     . counsel-yank-pop)
         ("M-s k"   . bury-buffer)
         ("M-<tab>" . ivy-switch-buffer)
         ("C-x C-k" . kill-current-buffer) ; Kill only the current buffer
         :map counsel-mode-map
         ([escape] . minibuffer-keyboard-quit))
  :config
  (setq counsel-git-cmd "rg --files"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."
        counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  (counsel-mode)
  :diminish counsel-mode)

(use-package counsel-projectile
  :after (projectile counsel)
  :bind (:map projectile-mode-map
              ("M-s a"   . counsel-projectile-rg)))

(use-package dash-docs
  :init
  (setq dash-docs-browser-func 'eww
        dash-docs-common-docsets '("Emacs_Lisp" "Common_Lisp" "Scala" "Clojure" "PostgreSQL")
        dash-docs-docsets-path "~/.docset"
        dash-docs-enable-debugging nil))

(use-package find-file-in-project
  :after projectile
  :bind (:map projectile-mode-map
              ("M-s f"   . find-file-in-project-by-selected))
  :config (setq ffip-use-rust-fd t))

(use-package swiper
  :bind (("M-s s"   . swiper))
  :init
  (setq swiper-use-visual-line nil
        swiper-use-visual-line-p (lambda (a) nil)))

(use-package ivy
  :bind (("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("M-<tab>" . ivy-next-line)
         ([tab]     . ivy-alt-done))
  :init
  (setq ivy-count-format "(%d/%d) "  ; do not count candidates
        ivy-height 17                ; number of result lines to display
        ivy-initial-inputs-alist nil ; no regexp by default
        ivy-use-virtual-buffers t    ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-wrap t
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode)
  (ivy-configure 'counsel-imenu :update-fn 'auto)
  :diminish ivy-mode)

(use-package guix)

(use-package mu4e-alert
  :after mu4e
  :init
  (setq mu4e-alert-email-notification-types '(subjects))
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query))

(use-package transmission
  :config
  (setq transmission-host "192.168.1.1"
        transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode)))

(use-package calc
  :init
  (setq math-additional-units
        '((GiB "1024 * MiB" "Giga Byte")
          (MiB "1024 * KiB" "Mega Byte")
          (KiB "1024 * B"   "Kilo Byte")
          (B nil "Byte")
          (Gib "1024 * Mib" "Giga Bit")
          (Mib "1024 * Kib" "Mega Bit")
          (Kib "1024 * b"   "Kilo Bit")
          (b "B / 8" "Bit"))
        math-units-table nil))

(use-package char-fold
  :init
  (setq char-fold-symmetric t
        search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :bind ("M-T" . reverse-im-translate-word)
  :init
  (require 'reverse-im)
  (reverse-im-activate "russian-computer")
  (setq reverse-im-char-fold t
        reverse-im-read-char-advice-function #'reverse-im-read-char-exclude))

(use-package package-lint)

(use-package bluetooth
  :after evil
  :init (evil-set-initial-state 'bluetooth-mode 'motion))

(provide '.emacs)
;;; .emacs ends here
