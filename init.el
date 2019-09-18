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
                  gc-cons-threshold (* 8 1024 1024) ;reset to 8mb,default is 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; UI configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'tango-dark)
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
(add-to-list 'load-path "~/.emacs.d/elisp/")
(when (file-readable-p "~/.emacs.d/secrets.el") (load "~/.emacs.d/secrets.el"))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup shells and executables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load alternative shells depending on OS
(defconst homebrew-shell "/usr/local/bin/bash")
(defconst gitbash-shell "C:\\Program Files\\Git\\bin\\bash.exe")

(when (eq system-type 'darwin)
  ;; FIXME: this doesn't work on GUI mac as gls isnt available until after exec-path-from-shell
  (let* ((gls-exec (executable-find "gls")))
    (when gls-exec (setq insert-directory-program gls-exec)))

  (when (file-executable-p homebrew-shell)
    (setenv "SHELL" homebrew-shell)
    (setq shell-file-name homebrew-shell)))

(when (and (eq system-type 'windows-nt) (file-executable-p gitbash-shell))
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (setenv "SHELL" gitbash-shell)
  ;; This is used in `shell-command-to-string` which is used by things like counsel-file-jump
  ;; (shell-command-to-string (concat find-program " " counsel-file-jump-args)))
  (setq shell-file-name gitbash-shell))

;; set $PATH according to my shell (including .profile) when launching GUI emacs
;; This doesn't seem necessary on my linux mint system but can add 'x to the list
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;
;; core emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package winner
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
(use-package paren
  :config (show-paren-mode))
(use-package delsel
  :config (delete-selection-mode))
(use-package saveplace
  :unless noninteractive
  :config (save-place-mode 1)
  :custom
  ;; Make exiting emacs fast on NFS
  (save-place-forget-unreadable-files nil))
(use-package savehist
  :unless noninteractive
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

(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))
(global-set-key (kbd "C-c C-c") 'compile)
;;(global-set-key (kbd "C-u") 'undo) ;; i need to break this habit..

;; TODO: Look into ws-butler instead
(defun delete-trailing-whitespace-except-md ()
  "Call `delete-trailing-whitespace` except when in `markdown-mode`."
  (unless (eq major-mode 'markdown-mode) (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'delete-trailing-whitespace-except-md)

;; these cause annoying rebuilds with webpack when a dir is being watched
(setq create-lockfiles nil)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; some stuff from better-defaults
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :defines compilation-filter-start
  :preface
  (defun color-compilation-output ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :init
  (add-hook 'compilation-filter-hook #'color-compilation-output))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure file backups and autosave ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun disable-backup-for-cache (file)
  "Return t if FILE is part of a blacklist or passes the normal predicate test.
Otherwise, return nil.  The main purpose of this function is to not backup
yarn.lock files."
  (unless (member (file-name-nondirectory file) '("yarn.lock"))
       (normal-backup-enable-predicate file)))

;; possible alternative: https://www.emacswiki.org/emacs/backup-each-save.el
(defun force-backup-of-buffer ()
  "Clear `buffer-backed-up`."
  (setq buffer-backed-up nil))

;; Backup after every save, keep up to 8 backups and store them in ~/.emacs.d/saves
;; Autosave (create #file#) every 200 keystrokes or after idle for 10s
(setq
   backup-by-copying t ;; don't clobber symlinks
   backup-directory-alist '(("." . "~/.emacs.d/saves"))
   delete-old-versions t ;; automatically delete backups when we have too many
   kept-new-versions 20
   kept-old-versions 0
   version-control t
   vc-make-backup-files t ;; backup files that are version controlled
   backup-enable-predicate #'disable-backup-for-cache
   auto-save-interval 200
   auto-save-timeout 10)
(add-hook 'before-save-hook #'force-backup-of-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Programming Productivity ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package wgrep
  :ensure t
  :defer t)
(use-package iedit
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :diminish "proj"
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package ivy
  :ensure t
  :pin gnu
  :custom
  (ivy-height 15)
  ;; makes recent files appear when showing buffer list
  ;;(ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :config (counsel-mode 1))

(use-package recentf
  :defer nil
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode +1)
  :bind ("C-x C-r" . counsel-recentf))

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-c SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)))

;; disabled for now
(use-package dashboard
  :ensure nil
  :if nil
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((projects . 10)
                     (recents  . 10)
                     (bookmarks . 5))))

(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))

(use-package magit
  :commands magit-status
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit (quote (20 . 0)))
  (forge-pull-notifications nil)
  :config
  ;; '(git hostname, api endpont, id typically hostname, repo class ex forge-github-repository)
  (add-to-list 'forge-alist private-forge-alist))

(use-package git-timemachine
  :ensure t
  :defer
  :commands git-timemachine)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package npm-bin-utils
  :commands (npm-bin-utils-add-to-path npm-bin-utils-find))

;; load flycheck after 3s idle or on first save. still deciding if I like this vs just starting immediately
;; a lot of this is copied from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-next-error flycheck-previous-error)
  :defer 3
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  (flycheck-global-modes (quote (not vue-mode)))
  (flycheck-idle-change-delay 3)
  :init
  (add-hook 'flycheck-mode-hook #'npm-bin-utils-add-to-path)
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (global-flycheck-mode)
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
            'magnars/adjust-flycheck-automatic-syntax-eagerness))

;; This or flycheck-pos-tip?
(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :defer
  :custom (flycheck-popup-tip-error-prefix "\u2718 "))

(use-package prettier-js
  :ensure t
  :commands (prettier-js prettier-js-mode))

(use-package company
  :ensure t
  :diminish "co."
  ;; :config
  ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  :custom
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-idle-delay 0.10)
  (company-minimum-prefix-length 2)
  (company-tooltip-flip-when-above t)
  :hook ((tern-mode typescript-mode) . company-mode))

(use-package yasnippet
  :ensure t)

;;; Language servers
(use-package lsp-mode
  :ensure t
  :after yasnippet
  :commands lsp
  :custom
  ((lsp-vetur-use-workspace-dependencies t))
  ;; FIXME: needs to be the local node_modules/typescript/lib and not the tsserver binary
  ;;:config (setq lsp-typescript-tsdk (npm-bin-utils-find "tsserver"))

  ;; Can we get company results to filter based on prefix rather than fuzzy?
  ;; I.e. `this.p` should autocomplete to `this.props` but not `this.top`
  :init (add-hook 'vue-mode-hook #'lsp))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package eglot
  :ensure nil
  :commands eglot-ensure
  ;; using lsp-mode for now
  ;;:hook (vue-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(vue-mode . ("vls"))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes for languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  ;;:defines (mmm-js-mode-enter-hook mmm-js-mode-exit-hook
  ;;          mmm-typescript-mode-enter-hook mmm-typescript-mode-exit-hook)
  :config
  ;; these dont work the greatest, the language server is better
  ;(setq mmm-js-mode-enter-hook (lambda() (tern-mode)(company-mode)))
  ;(setq mmm-js-mode-exit-hook (lambda() (tern-mode -1)(company-mode -1)))
  ;(setq mmm-typescript-mode-enter-hook #'setup-tide-for-vue)
  ;(setq mmm-typescript-mode-exit-hook #'teardown-tide-for-vue)
  )

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :pin gnu
  :mode "\\.json\\'")

(use-package tern
  :ensure t
  :if (executable-find "tern")
  :defer
  ;; :config
  ;; (tern-command (append tern-command '("--no-port-file"))))
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :ensure t
  :defer
  :after (tern company)
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package typescript-mode
  :ensure t
  :custom (typescript-indent-level 2)
  :mode "\\.ts\\'")

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

(use-package tide
  :ensure t
  :after (typescript-mode)
  :custom
  (tide-tsserver-locator-function (lambda() (npm-bin-utils-find "tsserver")))
  (tide-format-options '(:insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces nil))
  ;; dont use tide with language servers
  :hook ((typescript-mode . (lambda() (unless (assoc 'lsp-mode minor-mode-alist) (setup-tide-mode)))))
  :bind ("C-c C-d" . 'tide-documentation-at-point))

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

;; disable font-lock in really large buffers like webpack bundles
(add-hook 'prog-mode-hook (lambda ()
                            ;;(> (line-number-at-pos (point-max)) 5000))
                            (when (> (buffer-size) (* 750 1000))
                              (linum-mode -1)
                              (font-lock-mode -1))))

;; some packages to try out:
;; doom or other modeline, doom-themes, js2-refactor, company-box, purpose, diff-hl-mode, wgrep, iedit, dumb-jump

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-read-command nil)
 '(compile-command "yarn build")
 '(counsel-file-jump-args
   "* -type d \\( -name node_modules -o -name bower_components -o -name .git -o -name gemini -o -name tsout -o -name dist -o -false \\) -prune -false -o -type f")
 '(css-indent-offset 2)
 '(fill-column 120)
 '(indent-tabs-mode nil)
 '(js-chain-indent nil)
 '(js-enabled-frameworks (quote (javascript prototype)))
 '(js-indent-level 2)
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-concat-multiline-strings (quote eol))
 '(js2-highlight-external-variables nil)
 '(js2-include-node-externs t)
 '(js2-mode-assume-strict t)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(markdown-command "markdown_py")
 '(mmm-submode-decoration-level 0)
 '(package-selected-packages
   (quote
    (web-mode tide typescript-mode company-tern tern json-mode js2-mode vue-mode scss-mode lsp-ui company-lsp lsp-mode yasnippet company prettier-js flycheck-popup-tip flycheck git-timemachine forge magit hl-todo ace-jump-mode counsel-projectile projectile iedit wgrep keyfreq exec-path-from-shell diminish use-package)))
 '(save-place-forget-unreadable-files nil)
 '(tab-width 4)
 '(vue-dedicated-modes (quote (js-mode js2-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-warning ((t (:background "red" :underline (:color "brightred" :style wave)))))
 '(js2-warning ((t (:underline (:color "orange" :style wave) :slant italic))))
 '(json-mode-object-name-face ((t (:inherit font-lock-keyword-face)))))

(provide 'init)

;;; init.el ends here
(put 'magit-diff-edit-hunk-commit 'disabled nil)
