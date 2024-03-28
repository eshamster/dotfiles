;; Note: Create hardlink from dotfiles repository using Cygwin.
;; $ ln $(pwd)/win/emacs/init.el $(cygpath -u ${APPDATA})/.emacs.d/init.el

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; ----- Install packages ----- ;;

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize))

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
                    yasnippet
                    leaf
                    leaf-convert
                    company
                    projectile))

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
        ("C-x C-i"   . indent-region)
        ("M-o" . other-window)
        ("C-c C-w" . count-words)))

;; ----- Environment ----- ;;
(setq redisplay-dont-pause t
      scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-face-foreground 'font-lock-comment-face "#ee0909")
(show-paren-mode t)

(global-display-line-numbers-mode 1)
(set-face-foreground 'line-number "#448844")
(set-face-background 'line-number "#f0f0f0")
;; (setq linum-format "%4d \u2502 ")

(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
      grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
      grep-use-null-device "/dev/null")

(setq visible-bell t)

(setq initial-frame-alist
      '((top . 83) (left . 178) (width . 175) (height . 54)))

;; -- ido-mode -- ;;

(leaf ido
  :bind (("C-x C-f" . ido-find-file))
  :config
  (with-eval-after-load 'ido
    (ido-mode t)
    (ido-everywhere t)
    (setq ido-enable-flex-mathing t
          ffap-machine-p-known 'reject
          ido-use-filename-at-point nil)))

(leaf smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(leaf ido-vertical-mode
  :require t
  :setq ((ido-vertical-define-keys quote C-n-and-C-p-only)
         (ido-max-window-height . 0.75))
  :config
  (ido-vertical-mode t))

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

;; --- company --- ;;

(leaf company
  :custom ((company-idle-delay . 0.4)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t))
  :bind ((:company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))))

;; --- C# --- ;;

(install-packages '(omnisharp))

(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))

(leaf csharp-mode
  :hook ((csharp-mode-hook . omnisharp-mode)
         (csharp-mode-hook . company-mode))
  :custom ((tab-width . 4)))

;; --- TypeScript --- ;;
;; https://github.com/eshamster/dotfiles/blob/f5a39c71b013ade45048add19db4998b1cdfd62a/others/react-devel/react-devel.el

(install-packages '(typescript-mode
                    tide
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

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

;; --- CSS --- ;;

(leaf css-mode
  :custom ((tab-width . 2)
           (css-indent-offset . 2)))

;; --- Shader --- ;;

(install-packages '(shader-mode))

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

(leaf markdown-mode
  :bind ((:markdown-mode-map
          ("C-c m b" . markdown-insert-bold)
          ("C-c m s" . split-markdown-code))))

;; --- rust --- ;;

(leaf eglot
  :hook ((eglot-managed-mode-hook . (lambda () (eglot-inlay-hints-mode -1)))))

(leaf rust-mode
  :ensure t
  :hook ((rust-mode-hook . eglot-ensure)
         (rust-mode-hook . company-mode)
         (rust-mode-hook . flycheck-mode))
  :custom ((rust-format-on-save . t)))

;; --- yasnippet --- ;;

(leaf yasnippet
  :ensure t
  :bind ((:yas-minor-mode-map
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all)))
  :custom ((yas-global-mode . 1)
           (yas-prompt-functions . '(yas-ido-prompt))))

;; --- magit --- ;;

(leaf magit
  :bind (("C-c g s" . magit-status)))

;; --- projectile -- ;;

(leaf projectile
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ----- git-bash ----- ;;

;; https://qastack.jp/emacs/22049/git-bash-in-emacs-on-windows
(prefer-coding-system 'utf-8)
(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
(setq explicit-bash.exe-args '("--login" "-i"))

;; ----- Other libraries ----- ;;

;; display the directory name of the file when files that have a same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'auto-complete-config)
(ac-config-default)

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

;; Open shell buffer
(defun open-shell-buffer ()
  (interactive)
  (let ((shell-buf (get-buffer "*shell*")))
    (cond (shell-buf (pop-to-buffer shell-buf))
          (t (shell)))))

(global-set-key (kbd "C-c C-z") 'open-shell-buffer)

;; ----- start server ----- ;;
(server-start)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rust-mode projectile tide typescript-mode shader-modea shader-mode company omnisharp omnisharp-mode leaf-convert leaf yasnippet yasnipet yasnnipet ido-vertical-mode w3m smex paredit markdown-mode bmemagit auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
