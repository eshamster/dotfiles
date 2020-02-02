(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(defun install-packages (packages)
  (let ((refreshed nil))
    (dolist (pack packages)
      (unless (package-installed-p pack)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pack)))))

(install-packages '(auto-complete
                    magit
                    markdown-mode
                    ido-vertical-mode
                    smex
                    web-mode
                    ;; paredit
                    use-package
                    yasnippet
                    flycheck
                    powerline
                    exec-path-from-shell
                    protobuf-mode
                    flymake-go
                    company
                    company-go
                    go-errcheck ; require: go get -u github.com/kisielk/errcheck
                    projectile
                    ))

(exec-path-from-shell-initialize)

;; --- keybind --- ;;

(mapc '(lambda (pair)
         (global-set-key (kbd (car pair)) (cdr pair)))
      '(("M-g"  . goto-line)
        ("C-h"  . delete-backward-char)
        ("C-z"  . nil)
        ("C-_"  . undo)
        ("C-\\" . undo)
        ("C-o"  . nil)
        ("C-x i" . nil)
        ("C-x ;" . comment-region)
        ("C-x :" . uncomment-region)
        ("C-x C-i"   . indent-region)
        ("C-c s r" . replace-regexp)
        ("C-c s q" . query-replace)
        ("C-c s s" . replace-string)
        ("M-*" . pop-tag-mark)
        ("M-o" . other-window)
        ("C-c d" . insert-today)))

(defun insert-today ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

;; --- Environment --- ;;

(setq scroll-conservatively 100000
      scroll-margin 3)
(set-face-foreground 'font-lock-comment-face "#ee0909")
(show-paren-mode t)
(tool-bar-mode 0)
(global-linum-mode t)

;; clock

(setq display-time-string-forms
      '((format "%s/%s(%s) %s:%s:%s" month day dayname 24-hours minutes seconds)
        load
        (if mail " Mail" "")))
(setq display-time-kawakami-form t
      display-time-24hr-format t
      display-time-interval 1)

(display-time)

;; eww

(setq eww-search-prefix "https://www.google.com/search?q=")

;; --- magit --- ;;

(use-package magit
  :bind
  ("C-c g s" . magit-status))

;; --- perl --- ;;

(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  :mode (("\\.pl\\'" . cperl-mode)
         ("\\.pm\\'" . cperl-mode)
         ("\\.t\\'" . cperl-mode))
  :init
  (add-hook 'cperl-mode-hook 'hs-minor-mode)
  (add-hook 'cperl-mode-hook 'flycheck-mode)
  :config
  (setq cperl-indent-level 4
        cperl-indent-parens-as-block t
        cperl-indent-subs-specially nil
        cperl-close-paren-offset -4))

;; --- javascript --- ;;

(use-package js-mode
  :bind
  (("M-." . xref-find-definitions)))

;; --- web-mode --- ;;

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (set-face-foreground 'web-mode-html-tag-face "Blue1")
  (set-face-foreground 'web-mode-html-attr-name-face "ForestGreen"))

(use-package web-mode
  :mode (("\\.mt\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; - go - ;;

(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

(defun copy-go-function-name ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((regex "func \\(([^()]*)\\)?[^(]*"))
        (search-backward-regexp regex)
        (search-forward-regexp regex))
      (search-backward " ")
      (call-interactively 'set-mark-command)
      (search-forward "(")
      (backward-char)
      (call-interactively 'kill-ring-save))))

(use-package go-mode
  :bind
  (("M-." . godef-jump)
   ("M-[" . xref-find-references)
   ("C-c c f" . copy-go-function-name))
  :config
  (setq gofmt-command "goimports"
        c-basic-offset 4
        tab-width 4)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'before-save-hook 'gofmt-before-save))

;; - protobuf - ;;

(use-package protobuf-mode
  :init
  (setq c-basic-offset 4))

;; --- arkテンプレート用 --- ;;

(defun unindent-perl-region ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "^[ \t]*\\?" "?"))))

(defun automatically-unindent-perl-region ()
  (when (string-match "\\.mt\\(<.*>\\)?$" (buffer-name))
    (unindent-perl-region)))

(add-hook 'before-save-hook 'automatically-unindent-perl-region)

;; --- vue-mode --- ;;

(use-package vue-mode
  :init
  (setq js-indent-level 2))

;; --- yasnippet --- ;;

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode ; モードラインに非表示
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt)))

;; --- Others --- ;;

;; mode-line
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
(which-function-mode 1)

;; backup
(setq delete-auto-save-files t)
(setq backup-inhibited t)

;; use space instead of tab
(setq-default indent-tabs-mode nil)

;; dired
(defvar my-dired-before-buffer nil)
(defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

;; Note: "ls" of OS X doesn't support some options.
;;       So use gls instead (require "brew install coreutils")
(when (and (eq system-type 'darwin))
  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
      (setq insert-directory-program gls))))

(setq dired-listing-switches "-alh --group-directories-first")

(ffap-bindings)

;; recentf
(setq recentf-max-saved-items 10000
      recentf-auto-cleanup 'never
      recentf-exclude '("/recentf" "COMMIT_EDITING" "/.?TAGS" "ido\\.last")
      recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

(defun ido-recentf ()
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-c C-r") 'ido-recentf)
(global-set-key (kbd "C-c r r") 'ido-recentf)

(recentf-mode 1)

;; display the directory name of the file when files that have a same name are opened
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; auto complete
(require 'auto-complete-config)
(ac-config-default)

;; replace "option" and "command"
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)

;; use back slash instead of yen mark (¥ = 0x5c)
(define-key global-map [?¥] [?\\])

;; Open shell buffer
(defun open-shell-buffer ()
  (interactive)
  (let ((shell-buf (get-buffer "*shell*")))
    (cond (shell-buf (pop-to-buffer shell-buf))
          (t (shell)))))

(global-set-key (kbd "C-c C-z") 'open-shell-buffer)

;; 
(defun get-path-from-git-root ()
  (interactive)
  (kill-new 
   (format "%s%s"
           (replace-regexp-in-string
            "\n$" "" (shell-command-to-string "git rev-parse --show-prefix 2> /dev/null || true"))
           (replace-regexp-in-string
            "<[^<>]*>$" "" (buffer-name)))))

(global-set-key (kbd "C-c c p") 'get-path-from-git-root)

;; swap windows (only for 2 windows case)
(defun swap-windows ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (other-window 1)
    (let ((next-buf (current-buffer)))
      (switch-to-buffer cur-buf)
      (other-window 1)
      (switch-to-buffer next-buf)
      (other-window 1))))

(global-set-key (kbd "C-c s w") 'swap-windows)

;; --- ido-mode --- ;;

(use-package ido
  :bind
  ("C-x C-f" . ido-find-file)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-mathing t
        ffap-machine-p-known 'reject
        ido-use-filename-at-point nil))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only
        ido-max-window-height 0.75))

;; --- ddskk --- ;;

(use-package ddskk
  :bind
  (("C-x C-j" . skk-mode)))

(when (fboundp 'skk-mode)
  (fset 'ido-select-text 'skk-mode))

;; https://gist.github.com/nagae/1354329/cddf65aa89bb52f31434ba1164434a31517fc3c8
(add-hook 'isearch-mode-hook
          #'(lambda ()
              (when (and (boundp 'skk-mode)
                         skk-mode
                         skk-isearch-mode-enable)
                (skk-isearch-mode-setup))))

(add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (when (and (featurep 'skk-isearch)
                         skk-isearch-mode-enable)
                (skk-isearch-mode-cleanup))))

;; --- projectile --- ;;

(use-package projectile
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --- auto generated --- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile yaml-mode ido-vertical-mode markdowne-mode terraform-mode go-errcheck eglot powerline csharp-mode vue-mode dired-sidebar flycheck yasnippet use-package web-mode japanese-holidays smex markdown-mode magit auto-complete ddskk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
