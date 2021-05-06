;; Note: It is assumed that this el file will be loaded in
;;       https://github.com/eshamster/docker-cl-devel2/blob/master/init.el

;; install-packages is defined in
;; https://github.com/eshamster/docker-cl-devel2/blob/master/init.el
(install-packages '(use-package
                    ido-vertical-mode
                    projectile))

;; --- ido --- ;;

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only
                ido-max-window-height 0.75))

;; --- recentf --- ;;

(setq recentf-save-file "~/work/lisp/.recentf"
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      recentf-exclude '("/recentf" "COMMIT_EDITING" "/.?TAGS" "ido\\.last")
      recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

(defun ido-recentf ()
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-c r r") 'ido-recentf)

(recentf-mode 1)

;; --- projectile --- ;;

(use-package projectile
  :init
  (projectile-mode t)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --- others --- ;;

(global-linum-mode)
(setq linum-format "%3d|")

(global-set-key (kbd "M-o") 'other-window)

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

(defun copy-package-name ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (beginning-of-buffer)
      (search-forward "defpackage")
      (search-forward " ")
      (kill-new (thing-at-point 'symbol)))))

(defun copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(use-package lisp-mode
  :bind
  (("C-c c p" . copy-package-name)
   ("C-c c s" . copy-symbol-at-point)))
