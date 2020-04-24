;;; init.el --- my main emacs config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; disable GC and some filename checking regexes for faster startup
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  ;; vue language server creates tons of garbage so this speeds
                  ;; up auto completion a lot. Can also look into a library that only
                  ;; runs gc when idle. Used to use 8mb/0.1 and these new settings
                  ;; bring an auto complete test from 8 GCs and 0.7s to 2 GCs and 0.2s
                  gc-cons-threshold (* 24 1024 1024) ;reset to 16mb,default is 800000
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
(setq package-enable-at-startup nil)
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
  :ensure t)
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

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


(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "elisp")))
(let ((secrets-file (concat user-emacs-directory "secrets.el")))
  (when (file-readable-p secrets-file) (load secrets-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup shells and executables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load alternative shells depending on OS
(defconst homebrew-shell "/usr/local/bin/bash")
(defconst gitbash-shell "C:\\Program Files\\Git\\bin\\bash.exe")

;; set $PATH according to my shell (including .profile) when launching GUI emacs
;; This doesn't seem necessary on my linux mint system but can add 'x to the list
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (let* ((gls-exec (executable-find "gls")))
    (when gls-exec (setq insert-directory-program gls-exec)))

  (when (file-executable-p homebrew-shell)
    (setenv "SHELL" homebrew-shell)
    (setq shell-file-name homebrew-shell)))

(when (and (eq system-type 'windows-nt) (file-executable-p gitbash-shell))
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (setenv "SHELL" gitbash-shell)
  ;; This is used in `shell-command-to-string` which is used by things like counsel-file-jump
  ;; (shell-command-to-string (concat find-program " " counsel-file-jump-args)))
  (setq shell-file-name gitbash-shell))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; define some globals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; will be used to some features on large buffers like webpack bundles
;; This is not necessarially covered by so-long because those files might not have long lines
(defconst large-buffer (* 500 1000))
;; used to track when vue-mode is enabled
(defvar-local vue-mode-p nil)
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
                              (linum-mode -1)
                              (font-lock-mode -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some core editing config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: can I make Ctrl-/ work on macos for undo?
;; see https://apple.stackexchange.com/questions/24261/how-do-i-send-c-that-is-control-slash-to-the-terminal

;; these cause annoying rebuilds with webpack when a dir is being watched
(setq create-lockfiles nil)
;; helps with lsp-mode performance. requires emacs >=27
(setq read-process-output-max (* 1024 1024))

(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))
(global-set-key (kbd "C-c C-c") 'compile)

;; some stuff from better-defaults
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(when (< emacs-major-version 27)
  (load (concat user-emacs-directory "so-long.el")))
(use-package so-long
  :custom
  (so-long-threshold 500)
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
  (save-place-limit 200))
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

(defun delete-trailing-whitespace-except-md ()
  "Call `delete-trailing-whitespace` except when in `markdown-mode`."
  ;; use whitespace-cleanup instead?
  (unless (eq major-mode 'markdown-mode) (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-md)

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup counsel, source control ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: remove when https://github.com/magit/forge/issues/91 is resolved
(use-package browse-at-remote
  :ensure t
  :config
  ;; from secrets.el
  ;; can also git config --add browseAtRemote.type "github"
  (when (boundp 'company-github-url)
    (push (cons company-github-url "github")
          browse-at-remote-remote-type-domains))
  :bind ("C-c B" . browse-at-remote))

(use-package projectile
  :ensure t
  :diminish "proj"
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy))

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
  ((counsel-find-file-at-point t)
   (counsel-file-jump-args
    '("." "-type" "d" "(" "-name" "node_modules" "-o" "-name" "bower_components"
      "-o" "-name" ".git" "-o" "-name" "gemini" "-o" "-name" "tsout"
      "-o" "-name" "dist" "-o" "-false" ")" "-prune" "-false" "-o" "-type" "f")))
  ;;:bind ("M-x" . counsel-M-x)
  :config (counsel-mode 1))

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 0)
  (recentf-auto-cleanup 'never)
  (recentf-filename-handlers '(substring-no-properties abbreviate-file-name))
  :config
  (add-hook 'kill-emacs-hook 'recentf-cleanup)
  (recentf-mode +1)
  :bind ("C-x C-r" . counsel-recentf))

(use-package magit
  :commands magit-status
  :ensure t
  :config
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  :bind ("C-x g" . 'magit-status))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit (quote (20 . 0)))
  (forge-pull-notifications nil)
  :config
  ;; '(git hostname, api endpont, id typically hostname, repo class ex forge-github-repository)
  (when (boundp 'private-forge-alist) (add-to-list 'forge-alist private-forge-alist)))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config For prog modes    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "markdown_py")
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package flyspell-mode
  :commands (flyspell-mode)
  :hook (markdown-mode . flyspell-mode))

;; We have a lot of .ts files with a node shebang in them, so just blanket deactivate js-mode activating from a shebang
(setq interpreter-mode-alist (rassq-delete-all 'js-mode interpreter-mode-alist))

(use-package npm-bin-utils
  :commands (npm-bin-utils-add-to-path npm-bin-utils-find))

;; load flycheck after 3s idle or on first save. still deciding if I like this vs just starting immediately
;; a lot of this is copied from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-next-error flycheck-previous-error flycheck-add-next-checker)
  :defer 2
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  (flycheck-idle-change-delay 3)
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
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)
  (global-flycheck-mode))


;; This or flycheck-pos-tip?
(use-package flycheck-popup-tip
  :ensure t
  :disabled
  :commands flycheck-popup-tip-mode
  :after flycheck
  :hook ((flycheck-mode . flycheck-popup-tip-mode))
  :custom (flycheck-popup-tip-error-prefix "\u2718 "))

(use-package prettier-js
  :ensure t
  :commands (prettier-js prettier-js-mode))

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
  :ensure t
  :commands yas-minor-mode
  :hook ((lsp-mode . yas-minor-mode)))

;;; Language servers
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-vetur-use-workspace-dependencies t)

  ;; vls is pretty slow so disable live reporting
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-flycheck-live-reporting nil)
  :config
  (bind-key "C-c C-f" 'lsp-execute-code-action lsp-mode-map)
  ;; TODO: look into using lsp for other modes like js2, typescript, json to start
  ;; which-key integration doesnt work 100% in vue files: https://github.com/emacs-lsp/lsp-mode/issues/1598
  :hook ((lsp-mode . (lambda () (lsp-enable-which-key-integration t)))
         ;; TODO: maybe make company results filter based on prefix rather than fuzzy matching?
         (vue-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; vue files require some extra setup
  (when vue-mode-p
    (let* ((vue-block-modes (mapcar (lambda (l) (plist-get l :mode)) vue-modes))
           (vue-file-modes (cons 'vue-mode vue-block-modes)))

      ;; lsp-ui already runs flycheck for vue-mode
      (mapc 'lsp-ui-flycheck-add-mode vue-block-modes)
      ;; enable javascript-eslint checker for all possible major modes in a vue file
      (mapc (apply-partially 'flycheck-add-mode 'javascript-eslint) vue-file-modes)
      ;; run eslint checker after lsp checker
      (flycheck-add-next-checker 'lsp 'javascript-eslint)))

  (bind-key "C-c C-d" 'lsp-ui-doc-glance lsp-mode-map)
  :custom
  (lsp-ui-sideline-enable nil)

  (lsp-prefer-flymake nil)
  (lsp-ui-flycheck-enable t)

  ;; dont automatically show docs, instead bind lsp-ui-doc-glance
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature nil) ;; have eldoc display signatures
  )

;;TODO: use capf when its not experimental
(use-package company-lsp
  :ensure t
  :after lsp-mode
  :custom
  (company-lsp-cache-candidates 'auto)
  :commands company-lsp)

;; TODO: set up more environments
(use-package dap-mode
  :ensure t
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
  (dap-register-debug-template
   "Test Node Configuration"
   (list :type "node"
         :request "launch"
         :outFiles '("dist/*.js")
         :cwd (expand-file-name default-directory)
         :program nil
         :name "Node::Run")))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes for languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package vue-mode
  :ensure t
  :config
  (add-hook 'vue-mode-hook (lambda () (setq vue-mode-p (not vue-mode-p))))
  :custom
  (vue-dedicated-modes '(js-mode js2-mode))
  (vue-html-extra-indent 2)
  :mode "\\.vue\\'")

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

(use-package json-mode
  :ensure t
  :pin gnu
  :mode "\\.json\\'")

(use-package tern
  :ensure t
  :if nil ;;(executable-find "tern")
  ;; :config
  ;; (tern-command (append tern-command '("--no-port-file"))))
  :hook (js2-mode . (lambda () (unless (> (buffer-size) large-buffer) (tern-mode)))))

;; package seems to have been deleted off github and melpa
;; https://www.reddit.com/r/emacs/comments/g8i10n/companytern_on_melpa_pulled/
(use-package company-tern
  :if nil
  :ensure nil
  :defer
  :after (tern company)
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package typescript-mode
  :ensure t
  :custom (typescript-indent-level 2)
  :mode "\\.ts\\'")

(use-package tide
  :ensure t
  :after (:any typescript-mode js2-mode)
  :preface
  (defun setup-tide-mode ()
    "Setup tide."
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (defun teardown-tide-mode ()
    "Teardown tide."
    (company-mode -1)
    (tide-hl-identifier-mode -1)
    (eldoc-mode -1)
    (tide-mode -1))
  (defun is-ts-file ()
    "Return t if the buffer is for a .ts file."
    (and
     (stringp buffer-file-name) ;; required for vue files to get syntax highlighting in ts scripts
     (string-match "\\.ts\\'" buffer-file-name)))
  :custom
  (tide-tsserver-locator-function (lambda() (npm-bin-utils-find "tsserver")))
  (tide-format-options '(:insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces nil))
  ;; dont activate tide on vue files
  :hook ((typescript-mode js2-mode) . (lambda()
                                        (unless (or vue-mode-p large-buffer-p) (setup-tide-mode))))
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  ;; We currently do not eslint on ts files
  ;; https://github.com/ananthakumaran/tide/issues/308
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))
  ;; Dont interfere with LSP keybindings for vue files with ts scripts
  (unless vue-mode-p
    (bind-key "C-c C-d" 'tide-documentation-at-point tide-mode-map)
    (bind-key "C-c C-f" 'tide-fix tide-mode-map)))

(use-package web-mode
  :ensure t
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
  :mode "\\.html?\\'")


;;;;;;;;;;;;;;;;;;
;; Misc Testing ;;
;;;;;;;;;;;;;;;;;;
;; Delete *Completions* buffer (not as useful with ivy)
(add-hook 'minibuffer-exit-hook
   '(lambda ()
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
 '(compilation-always-kill t)
 '(compilation-read-command nil)
 '(compile-command "yarn build")
 '(css-indent-offset 2)
 '(fill-column 120)
 '(indent-tabs-mode nil)
 '(mmm-submode-decoration-level 0)
 '(package-selected-packages
   (quote
    (posframe dap-mode web-mode tide typescript-mode company-tern tern json-mode js2-mode vue-mode scss-mode lsp-ui company-lsp lsp-mode yasnippet company prettier-js flycheck-popup-tip flycheck git-timemachine forge magit hl-todo ace-jump-mode counsel-projectile projectile iedit wgrep keyfreq exec-path-from-shell diminish use-package)))
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(json-mode-object-name-face ((t (:inherit font-lock-keyword-face)))))

(provide 'init)

;;; init.el ends here
