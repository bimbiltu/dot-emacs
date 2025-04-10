;;; init.el --- my main emacs config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; disable GC and some filename checking regexes for faster startup
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  ;; vue language server creates tons of garbage so this speeds up auto completion a lot. Can also look
                  ;; into a library that only runs gc when idle. Used to use 8mb threshold 0.1 percentage and these new
                  ;; settings bring an auto complete test from 8 GCs and 0.7s to 2 GCs and 0.2s
                  gc-cons-threshold (* 24 1024 1024) ;reset to 24mb,default is 800000
                  gc-cons-percentage 0.3)
            (garbage-collect)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; UI configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS menu bar doesnt take up extra space
(when (and (fboundp 'menu-bar-mode) (not (memq window-system '(mac ns))))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup package managers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

;; helper libraries
(use-package f
  :ensure t)

(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "elisp")))
(let ((secrets-file (concat user-emacs-directory "secrets.el")))
  (when (file-readable-p secrets-file) (load secrets-file nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup shells and executables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load alternative shells depending on OS
(defconst homebrew-bash "/usr/local/bin/bash")
(defconst macos-zsh "/bin/zsh")
(defconst macos-shell-to-use macos-zsh)
(defconst gitbash-shell "C:\\Program Files\\Git\\bin\\bash.exe")
(when (eq system-type 'darwin)
  (let* ((gls-exec (executable-find "gls")))
    (when gls-exec (setq insert-directory-program gls-exec)))

  (when (file-executable-p macos-shell-to-use)
    (setenv "SHELL" macos-shell-to-use)
    (setq shell-file-name macos-shell-to-use)))

(when (and (eq system-type 'windows-nt) (file-executable-p gitbash-shell))
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (setenv "SHELL" gitbash-shell)
  ;; This is used in `shell-command-to-string` which is used by things like counsel-file-jump
  ;; (shell-command-to-string (concat find-program " " counsel-file-jump-args)))
  (setq shell-file-name gitbash-shell))

;; set $PATH according to my shell (including .profile) when launching GUI emacs
;; This doesn't seem necessary on my linux mint system but can add 'x to the list
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (add-to-list 'exec-path-from-shell-variables "GOPRIVATE")
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; define some globals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; will be used to some features on large buffers like webpack bundles
;; This is not necessarially covered by so-long because those files might not have long lines
(defconst large-buffer (* 500 1000))
;; used to track when vue-mode is enabled
(defvar-local large-buffer-p nil)
(add-hook 'prog-mode-hook (defun my-check-buffer-size ()
                            (when (> (buffer-size) large-buffer)
                              (setq large-buffer-p t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure file backups and autosave ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-backup-enable-p (file)
  "Return t if FILE is part of a blacklist or passes the normal predicate test.
Otherwise, return nil.  The main purpose of this function is to not backup
lockfiles or large files."
  (unless (or (member (file-name-nondirectory file) '("yarn.lock" "package-lock.json"))
              (> (buffer-size) large-buffer))
       (normal-backup-enable-predicate file)))

;; alternative: https://www.emacswiki.org/emacs/backup-each-save.el
(defun force-backup-of-buffer ()
  "Clear `buffer-backed-up`."
  (setq buffer-backed-up nil))

;; Backup after every save, keep up to 8 backups and store them in <user-emacs-directory>/saves
;; Autosave (create #file#) every 200 keystrokes or after idle for 10s
(setq
   backup-by-copying t ;; don't clobber symlinks
   backup-directory-alist (list (cons "." (concat user-emacs-directory (file-name-as-directory "saves"))))
   delete-old-versions t ;; automatically delete backups when we have too many
   kept-new-versions 20
   kept-old-versions 0
   version-control t
   vc-make-backup-files t ;; backup files that are version controlled
   backup-enable-predicate 'my-backup-enable-p
   auto-save-interval 200
   auto-save-timeout 10)
(add-hook 'before-save-hook #'force-backup-of-buffer)


;;;;;;;;;;;;;;;;;;;;
;; core ui config ;;
;;;;;;;;;;;;;;;;;;;;
(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(setq ring-bell-function 'ignore
      visible-bell nil
      mouse-yank-at-point t
      hscroll-margin 2
      hscroll-step 1
      ;; dont recenter window on cursor when scrolling. not sure if i like this yet
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      echo-keystrokes 0.02
      ;; visit files opened from outside in the same frame
      ;; for example, when opening a json file in emacs from finder dont open the file in a separate frame
      ns-pop-up-frames nil
      frame-resize-pixelwise t
      window-resize-pixelwise t)

;; from doom:
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)

;; this really needs clipboard integration to be nice, otherwise copy/pasting
;; doesnt work well
;; (add-hook 'tty-setup-hook 'xterm-mouse-mode)

;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :defines compilation-filter-start
  :preface
  (defun color-compilation-output ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :init
  (add-hook 'compilation-filter-hook #'color-compilation-output))

;; use colors from PS1 variable in shell-mode
;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(use-package winner
  :defer 2
  :config (winner-mode))
(use-package windmove
  :bind
  (("<S-up>" . windmove-up)
   ("<S-down>" . windmove-down)
   ("<S-left>" . windmove-left)
   ("<S-right>" . windmove-right)))
;; To make these keybindings work in macOS terminal you will need to set up some key combinations to send
;; the proper escape sequence
;; https://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal
;; http://www.leonerd.org.uk/hacks/fixterms/
(use-package buffer-move
  :bind
  (("<C-S-up>" . buf-move-up) ;;\033[1;6A
   ("<C-S-down>" . buf-move-down) ;;\033[1;6B
   ("<C-S-left>" . buf-move-left) ;;\033[1;6D
   ("<C-S-right>" . buf-move-right))) ;;\033[1;6C

(use-package goto-addr
  :hook
  ((text-mode . goto-address-mode)
   (prog-mode . goto-address-prog-mode)))

;; disabled for now
(use-package dashboard
  :ensure t
  :disabled
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((projects . 10)
                     (recents  . 10)
                     (bookmarks . 5))))

;; disable font-lock and line numbers in really large buffers like webpack bundles
(add-hook 'prog-mode-hook (lambda ()
                            ;;(> (line-number-at-pos (point-max)) 5000))
                            (when (> (buffer-size) large-buffer)
                              (display-line-numbers-mode -1)
                              (display-line-numbers-mode -1)
                              (font-lock-mode -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some core editing config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: can I make Ctrl-/ work on macos for undo?
;; see https://apple.stackexchange.com/questions/24261/how-do-i-send-c-that-is-control-slash-to-the-terminal

;; these cause annoying rebuilds with webpack when a dir is being watched
(setq create-lockfiles nil)
;; helps with lsp-mode performance. requires emacs >=27
(setq read-process-output-max (* 3 1024 1024))

(global-set-key [f5] (defun my/force-revert-buffer () (interactive) (revert-buffer nil t)))
(make-variable-buffer-local 'compile-command)
(global-set-key (kbd "C-c C-c") 'compile)

;; some stuff from better-defaults
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(when (< emacs-major-version 27)
  (load (concat user-emacs-directory "so-long.el") nil t))
(use-package so-long
  :custom
  (so-long-threshold 3000)
  :config (global-so-long-mode))
(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config (show-paren-mode))
(use-package delsel
  :config (delete-selection-mode))
(use-package saveplace
  :config (save-place-mode 1)
  :custom
  ;; Make exiting emacs fast on NFS
  (save-place-forget-unreadable-files nil)
  (save-place-limit 300))
(use-package savehist
  :config
  (savehist-mode 1))
(use-package move-lines
  :bind
  (("M-<up>" . move-lines-up)
   ("ESC <up>" . move-lines-up)
   ("M-<down>" . move-lines-down)
   ("ESC <down>" . move-lines-down)))
(use-package my-functions
  :commands (my/p4-edit)
  :bind
  ("C-d" . my/duplicate-line))
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))
(use-package wgrep
  :ensure t
  :defer t)
(use-package iedit
  :ensure t
  :commands (iedit-mode-from-isearch)
  :bind
  (("C-;" . iedit-mode)
   ("C-h C-;" . iedit-mode-toggle-on-function)))

(use-package ace-jump-mode
  :ensure t
  :commands (ace-jump-line-mode ace-jump-word-mode ace-jump-char-mode)
  :bind
  (("C-c SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package ws-butler
  :ensure t
  :diminish
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook (prog-mode . ws-butler-mode))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup counsel, source control ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :diminish "proj"
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  :config
  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-projectile.el
  (let ((val (or (getenv "P4CONFIG") ".p4rc")))
    (add-to-list 'projectile-project-root-files val t)))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-height 15)
  ;; makes recent files appear when showing buffer list
  ;;(ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  :config
  ;;(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :custom
  ; I tend to author code using import aliases, so this wrong more often than not now
  ((counsel-find-file-at-point nil)
   (counsel-file-jump-args
    '("." "-type" "d" "(" "-name" "node_modules" "-o" "-name" "bower_components"
      "-o" "-name" ".git" "-o" "-name" "gemini" "-o" "-name" "tsout"
      "-o" "-name" "dist" "-o" "-false" ")" "-prune" "-false" "-o" "-type" "f")))
  :config (counsel-mode 1))

(use-package recentf
  :init
  ;(add-hook 'kill-emacs-hook 'recentf-cleanup)
  (bind-key "C-x C-r" 'counsel-recentf)
  :defer 2
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 0)
  (recentf-auto-cleanup 10)
  (recentf-filename-handlers '(substring-no-properties abbreviate-file-name))
  :config (recentf-mode 1))

;; dir-locals for better perf in big repos:
;; ((magit-status-mode
;;   . (
;;      (eval . (magit-disable-section-inserter 'magit-insert-tags-header))
;;      (eval . (magit-disable-section-inserter 'magit-insert-status-headers))
;;      ;; (eval . (magit-disable-section-inserter 'magit-insert-stashes))
;;      ;; (eval . (magit-disable-section-inserter 'forge-insert-pullreqs))
;;      (eval . (remove-hook 'server-switch-hook 'magit-commit-diff))
;;      (eval . (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))
;;      )))
(use-package magit
  :commands magit-status
  :ensure t
  :config
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  :custom
  (magit-refresh-status-buffer nil)
  :bind ("C-x g" . 'magit-status))

(use-package sqlite3
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit (quote (20 . 0)))
  (forge-pull-notifications nil)
  :bind ("C-c B" . browse-at-remote)
  :config
  ;; '(git hostname, api endpont, id typically hostname, repo class ex forge-github-repository)
  (when (boundp 'private-forge-alist) (add-to-list 'forge-alist private-forge-alist)))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config For prog modes    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit
  :diminish '(typescript-ts-mode . "TS")
  :custom
  (treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src" "tsx"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql")))))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "markdown_py")
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml'" . yaml-mode)
         ("\\.yml'" . yaml-mode)))

(use-package flyspell-mode
  :commands (flyspell-mode)
  :hook (markdown-mode . flyspell-mode))

;; We have a lot of .ts files with a node shebang in them, so just blanket deactivate js-mode activating from a shebang
(setq interpreter-mode-alist (rassq-delete-all 'js-mode interpreter-mode-alist))

(use-package npm-bin-utils
  :commands (npm-bin-utils-add-to-path npm-bin-utils-find))

;; load flycheck after 1s idle or on first save. still deciding if I like this vs just starting immediately
;; a lot of this is copied from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-next-error flycheck-previous-error flycheck-add-next-checker)
  :defer 2
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  (flycheck-idle-change-delay 1)
  ;; TODO: experiemnt with this and come up with something that works in a terminal
  ;; Maybe change the delimited error face, or use a text based delimiter when window-system?
  ;; Possible delimiters: →← »«
  ;; https://github.com/flycheck/flycheck/issues/1730
  ;;(flycheck-highlighting-style '(delimiters "a" "b"))
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :init
  (defun disable-flycheck-on-large-buffers ()
    (when (> (buffer-size) large-buffer) (flycheck-mode -1)))
  ;; why does this need to be in init? seems to give infinite recursion in :config
  (add-hook 'prog-mode-hook 'disable-flycheck-on-large-buffers)

  :config
  (add-hook 'flycheck-mode-hook #'npm-bin-utils-add-to-path)
  ;; (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
  ;;                  (js2-mode-hook        . js2-mode-map)
  ;;                  (typescript-mode-hook . typescript-mode-map)
  ;;                  (lsp-mode-hook        . lsp-mode-map)
  ;;                  (c-mode-common-hook   . c-mode-base-map)))
  ;;   (add-hook (car where)
  ;;             `(lambda ()
  ;;                (bind-key "M-n" #'flycheck-next-error ,(cdr where))
  ;;                (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))

  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're much laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.5 1)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  ; Modify built in checkers to run on other modes
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)

  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)
  ;; https://github.com/hlissner/doom-emacs/issues/2194
  ;; underline cant be a different color than the foreground on terminal
  ;; set foreground color to red on terminals to compensate
  ;; This doesnt take into account emacs running with frames both in the
  ;; terminal and GUI but im not worried about that situation.
  ;; https://stackoverflow.com/a/5801740
  (add-hook 'flycheck-mode-hook
            (defun fix-flycheck-error-face ()
              (unless window-system
                (set-face-attribute 'flycheck-error nil :foreground "red")
                (set-face-attribute 'flycheck-warning nil :foreground "yellow")
                (set-face-attribute 'flycheck-info nil :foreground "yellow"))))
  (global-flycheck-mode))


;; This or flycheck-pos-tip?
(use-package flycheck-popup-tip
  :ensure t
  :commands flycheck-popup-tip-mode
  :after flycheck
  :hook ((flycheck-mode . flycheck-popup-tip-mode))
  :custom (flycheck-popup-tip-error-prefix "\u2718 "))

(use-package prettier-js
  :ensure t
  :commands (prettier-js prettier-js-mode)
  :bind ("C-c C-p" . prettier-js))

(use-package company
  :ensure t
  :diminish "co."
  ;; :config
  ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  :bind
  ("C-c ." . company-complete)
  :custom
  ;; preserve casing of "dumb" completions
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-idle-delay 0.10)
  (company-minimum-prefix-length 1)
  (company-tooltip-flip-when-above t)
  (company-selection-wrap-around t)
  :hook ((tern-mode typescript-mode) . company-mode))

(use-package yasnippet
  :ensure t)

;;; Language servers
(use-package lsp-mode
  :ensure t
  :commands lsp
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-dap-auto-configure nil)
  (lsp-idle-delay 0.25)
  (lsp-vetur-use-workspace-dependencies t)
  (lsp-headerline-breadcrumb-enable nil)
  ;; creates file watchers on every directory in every project..
  (lsp-eslint-enable nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-render-all t)
  (lsp-auto-execute-action nil)

  ;; language server settings
  (lsp-clients-clangd-executable "clangd-11")
  (lsp-clients-clangd-args '("--suggest-missing-includes"))

  (lsp-clients-typescript-prefer-use-project-ts-server nil)
  (lsp-clients-typescript-max-ts-server-memory 8192)

  :config
  (bind-key "C-c C-f" 'lsp-execute-code-action lsp-mode-map)
  ;; which-key integration doesnt work 100% in vue files: https://github.com/emacs-lsp/lsp-mode/issues/1598
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; eslint is too slow, parses entire project. Much better to use the LSP client, except
         ;; that sets up a watcher on everything in projectile for some reason
         ;; (lsp-managed-mode . (lambda() (flycheck-add-next-checker 'lsp 'javascript-eslint)))
         (vue-mode . lsp)
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (js2-mode . lsp)
         (ruby-mode . lsp)
         (go-mode . lsp)))

(use-package lsp-ivy
  :ensure t
  :disabled
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :preface
  (defun my-setup-lsp-vue-flycheck()
    (when (boundp 'vue-modes)
      (let* ((vue-block-modes (mapcar (lambda (l) (plist-get l :mode)) vue-modes))
             (vue-file-modes (cons 'vue-mode vue-block-modes)))

        ;; lsp-ui already runs flycheck for vue-mode
        (mapc 'lsp-flycheck-add-mode vue-file-modes)
        ;; run eslint checker after lsp checker since we use eslint with vue plugin
        (flycheck-add-next-checker 'lsp 'javascript-eslint))))
  :hook
  ('lsp-diagnostics-mode . 'my-setup-lsp-vue-flycheck)
  :config
  (bind-key "C-c C-d" 'lsp-ui-doc-glance lsp-mode-map)
  :custom
  (lsp-ui-sideline-enable nil)

  ;; dont automatically show docs, instead bind lsp-ui-doc-glance
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature nil) ;; have eldoc display signatures
  (lsp-ui-doc-position 'top))

;; TODO: set up more environments
(use-package dap-mode
  :ensure t
  :disabled
  :commands dap-mode
  :config
   (defun setup-dap-mode ()
     (dap-ui-mode 1)
     (dap-ui-breakpoints)
     (dap-ui-locals)
     (dap-ui-expressions)
     ;; enables mouse hover support
     (dap-tooltip-mode 1)
     ;; use tooltips for mouse hover
     ;; if it is not enabled `dap-mode' will use the minibuffer.
     (tooltip-mode 1)

     ;; displays floating panel with debug buttons
     ;; requies emacs 26+
     (when window-system (dap-ui-controls-mode 1)))
   (setup-dap-mode))

(use-package dap-node
  :after dap-mode
  :config
  (defun my-find-package-dir ()
    (let* ((expanded-default-dir (f-expand default-directory))
           (package-dir (f-traverse-upwards
                         (lambda (path)
                           (f-exists? (f-expand "package.json" path)))

                         ;; need otherwise f-traverse-upwards can return path with ~
                         expanded-default-dir)))
           (or package-dir expanded-default-dir)))

  (let ((node-package-dir (my-find-package-dir)))
    (dap-register-debug-template
     "My Node Configuration"
     (list :type "node"
           :request "launch"
           ;; could build the expected filename rather than use glob here
           :outFiles (list (f-join node-package-dir "dist" "lib-common" "**" "*.js"))
           :cwd node-package-dir
           :program nil
           :name "Node::Run"))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes for languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package vue-mode
  :ensure t
  :custom
  (vue-dedicated-modes '(js-mode js2-mode))
  (vue-html-extra-indent 2)
  :mode "\\.vue\\'")

(use-package json-mode
  :ensure t
  :pin gnu
  :mode "\\.json\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package c++-mode
  :mode "\\.tpp\\'"
  :preface
  (defun my-setup-c++-offsets ()
    "Sets up some c offsets for c++ mode"
    (c-set-offset 'arglist-intro '++)
    (c-set-offset 'statement-cont '++)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'brace-list-open 0)
    (c-set-offset 'brace-list-entry '+))
  :hook
  ;; use a hook because :config will only run for tpp files
  (c++-mode . (lambda () (progn
                           (unbind-key "C-c C-c" c++-mode-map)
                           (my-setup-c++-offsets)))))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package js2-mode
  :ensure t
  :custom
  ((js-chain-indent nil)
   (js-enabled-frameworks (quote (javascript prototype)))
   (js-indent-level 2)
   (js2-allow-rhino-new-expr-initializer nil)
   (js2-concat-multiline-strings (quote eol))
   (js2-highlight-external-variables nil)
   (js2-include-node-externs t)
   (js2-mode-assume-strict t)
   (js2-strict-inconsistent-return-warning nil)
   (js2-strict-trailing-comma-warning nil))
  :mode "\\.js\\'")

(use-package typescript-mode
  :ensure t
  :disabled
  :diminish "TS"
  :custom (typescript-indent-level 2)
  :mode "\\.ts\\'" "\\.mjs\\'" "\\.cjs\\'")
;; Prefer tree sitter for typescript
(use-package typescript-ts-mode
  :ensure t
  :diminish "TS"
  :custom (typescript-indent-level 2)
  :mode "\\.ts\\'" "\\.mjs\\'" "\\.cjs\\'")

;; Interesting alternative https://github.com/orzechowskid/tsx-mode.el
(use-package tsx-ts-mode
  :diminish "TSX"
  :mode "\\.[jt]sx\\'")

(use-package web-mode
  :ensure t
  :preface
  ; web-mode needs linum-mode to exist, which was removed in emacs 29
  (unless (fboundp 'linum-mode)
    (defun linum-mode ()))
  :custom ((web-mode-auto-close-style 2)
           (web-mode-code-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-enable-auto-closing t)
           (web-mode-enable-auto-expanding t)
           (web-mode-enable-auto-indentation t)
           (web-mode-enable-auto-opening t)
           (web-mode-enable-auto-quoting t)
           (web-mode-enable-auto-pairing t)
           (web-mode-enable-current-column-highlight t)
           (web-mode-enable-current-element-highlight t)
           (web-mode-markup-indent-offset 2))
  :mode (("\\.html?\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;
;; Misc Testing ;;
;;;;;;;;;;;;;;;;;;
;; Delete *Completions* buffer (not as useful with ivy)
(add-hook 'minibuffer-exit-hook
   (lambda ()
      (let ((buffer "*Completions*"))
        (and (get-buffer buffer) (kill-buffer buffer)))))


;; some packages to try out:
;; doom or other modeline, doom-themes, js2-refactor, company-box, purpose, diff-hl-mode, wgrep, iedit, dumb-jump

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(compilation-always-kill t)
 '(compilation-read-command nil)
 '(compile-command "npx tsc")
 '(css-indent-offset 2)
 '(fill-column 120)
 '(indent-tabs-mode nil)
 '(mmm-submode-decoration-level 0)
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
     (eval remove-hook 'server-switch-hook 'magit-commit-diff)
     (eval magit-disable-section-inserter 'magit-insert-tags-header)
     (eval magit-disable-section-inserter 'magit-insert-status-headers)))
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(json-mode-object-name-face ((t (:inherit font-lock-keyword-face)))))

(provide 'init)

;;; init.el ends here
