;; Note: Create hardlink from dotfiles repository using Cygwin.
;; $ ln $(pwd)/win/emacs/init.el $(cygpath -u ${APPDATA})/.emacs.d/init.el

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; ----- Install packages ----- ;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
                    paredit
                    ido-vertical-mode
                    smex
                    use-package))

;; ----- keybind ----- ;;

(mapc '(lambda (pair)
         (global-set-key (kbd (car pair)) (cdr pair)))
      '(("M-g"  . goto-line)
        ("C-h"  . delete-backward-char)
        ("C-z"  . nil)
        ("C-_"  . undo)
        ("C-\\" . undo)
        ("C-o"  . nil)
        ("M-*"  . pop-tag-mark)
        ("C-x ;" . comment-region)
        ("C-x :" . uncomment-region)
        ("C-x C-i"   . indent-region)))

;; ----- Environment ----- ;;
(setq redisplay-dont-pause t
      scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-face-foreground 'font-lock-comment-face "#ee0909")
(show-paren-mode t)
(global-linum-mode t)

(setq initial-frame-alist
      '((top . 83) (left . 178) (width . 175) (height . 54)))

;; -- ido-mode -- ;;

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

;; -- -- ;;

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

(setq dired-listing-switches "-lXa")

;; recentf
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup 'never)
(setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "ido\\.last"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

(defun ido-recentf ()
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-c r r") 'ido-recentf)

(recentf-mode 1)

;; --- markdown --- ;;

(defun split-markdown-code ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (search-backward "```")
      (call-interactively 'move-end-of-line)
      (call-interactively 'set-mark-command)
      (call-interactively 'backward-word)
      (call-interactively 'kill-ring-save)))
  (insert (format "```\n\n\n\n```"))
  (insert (car kill-ring-yank-pointer))
  (call-interactively 'previous-line)
  (call-interactively 'previous-line))

(use-package markdown-mode
  :bind
  (("C-c m b" . markdown-insert-bold)
   ("C-c m s" . split-markdown-code)))

;; ----- Other libraries ----- ;;

;; display the directory name of the file when files that have a same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'auto-complete-config)
(ac-config-default)

;; ----- start server ----- ;;
(server-start)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-vertical-mode use-package w3m smex paredit markdown-mode magit auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
