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

;; --- projectile --- ;;

(use-package projectile
  :init
  (projectile-mode t)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
