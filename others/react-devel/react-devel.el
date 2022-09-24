(install-packages '(tide
                    company
                    flycheck))

(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq tab-width 2
        js-indent-level 2
        typescript-indent-level 2))

(setq company-tooltip-align-annotations t)

(add-hook 'js-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(defun toggle-ts-and-js-mode ()
  (interactive)
  (let ((mode (format "%s" major-mode)))
    (cond ((string= mode "typescript-mode")
           (js-mode))
          ((string= mode "js-mode")
           (typescript-mode)))))

(global-set-key (kbd "C-c m t") 'toggle-ts-and-js-mode)

;; --- company --- ;;

(use-package company
  :init
  (setq company-auto-expand t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer))
