(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

(defun install-packages (packages)
  (let ((refreshed nil))
    (dolist (pack packages)
      (unless (package-installed-p pack)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pack)))))

(install-packages '(magit
                    markdown-mode
                    ido-vertical-mode
                    smex
                    use-package
                    yasnippet
                    projectile))

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
        ("M-*" . pop-tag-mark)
        ("M-o" . other-window)))

;; --- Environment --- ;;

(setq scroll-conservatively 100000
      scroll-margin 3)
(show-paren-mode t)
(global-linum-mode t)
(setq linum-format "%3d|")

;; --- magit --- ;;

(use-package magit
  :bind
  ("C-c g s" . magit-status))

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
(setq recentf-save-file "~/work/.recentf"
      recentf-max-saved-items 10000
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

;; --- projectile --- ;;

(use-package projectile
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ----- load ----- ;;

(load "react-devel.el" t)

;; --- auto generated --- ;;

