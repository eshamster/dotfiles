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
                    leaf
                    leaf-convert
                    company
                    projectile
                    breadcrumb
                    web-mode))

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

(defun find-exe-path (exe)
  (replace-regexp-in-string
   "/" "\\\\"
   (cl-find-if (lambda (dir) (file-exists-p (format "%s/%s" dir exe)))
               (list (format "%s/AppData/Local/Programs/Git/bin" (getenv "USERPROFILE"))
                     (format "%s/AppData/Local/Programs/Git/usr/bin" (getenv "USERPROFILE"))
                     (format "%s/Git/usr/bin" (getenv "ProgramFiles"))
                     (format "%s/Git/bin" (getenv "ProgramFiles"))))))

(defun find-exe (exe)
  (concat
   (replace-regexp-in-string "\\\\" "/" (find-exe-path exe))
   "/"
   exe))

;; NOTE: 当初 xxx-program にフルパスを指定していたが、
;; 実行時に "C:\\Program " で切られてエラーになったので PATH に追加することにした。
;; ※ "PROGRA~1" のように置き換えても実行時には展開されて同じ結果だった
(let ((add-paths (cl-mapcar (lambda (exe) (find-exe-path exe))
                            '("find.exe" "grep.exe"))))
  (setenv "PATH" (concat (mapconcat 'identity (delete-dups add-paths) ";")
                         ";"
                         (getenv "PATH"))))

(setq find-program "find.exe"
      grep-program "grep.exe"
      grep-use-null-device nil)

(setq visible-bell t)

(setq initial-frame-alist
      '((top . 83) (left . 178) (width . 175) (height . 54)))

;; leaf

(leaf leaf-keywords
  :ensure t
  :init
  (leaf el-get :ensure t)
  (leaf hydra :ensure t)
  (leaf major-mode-hydra :ensure t)
  :config
  (leaf-keywords-init))

;; --- fido-vertical-mode --- ;;

(fido-vertical-mode +1)

;; completion-styles が (flex) で上書きされてしまうのを阻止
;; https://www.reddit.com/r/emacs/comments/13enmhl/comment/l1hfa29/
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (kill-local-variable 'completion-styles)))

(leaf orderless
  :ensure t
  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-overrides . '((file (styles basic partial-completion)))))
  :config
  ;; https://k1low.hatenablog.com/entry/2025/01/14/095141
  (keymap-unset minibuffer-local-completion-map "SPC"))

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
(setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

(global-set-key (kbd "C-c r r") 'recentf-open)

(recentf-mode 1)

;; paredit
(leaf paredit
  :ensure t
  :bind ((paredit-mode-map
          ("C-c f" . paredit-forward-slurp-sexp)
          ("C-c b" . paredit-forward-barf-sexp)
          ("C-h" . paredit-backward-delete)
          ("C-M-p" . paredit-backward)
          ("C-M-n" . paredit-forward)))
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)))

;; --- company --- ;;

(leaf company
  :custom ((company-idle-delay . 0.4)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t))
  :bind ((:company-active-map
          ("C-n" . company-select-next)
          ("M-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-p" . company-select-previous))))

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

;; --- JavaScript --- ;;

(leaf js-mode
  :custom ((tab-width . 2)
           (js-indent-level . 2)))

;; --- web-mode --- ;;

(leaf web-mode
  :mode (("\\.html\\'" . web-mode))
  :custom ((web-mode-markup-indent-offset . 2)))

;; --- svelte --- ;;

(leaf svelte-mode
  :ensure t
  ;; M-o 系のキーバインドを無効にする
  :bind ((:svelte-mode-map
          ("M-o" . nil))))

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
  :pretty-hydra ("Insert"
                 (("i" yas-insert-snippet "insert"))
                 "Edit"
                 (("n" yas-new-snippet "new")
                  ("v" yas-visit-snippet-file "visit")
                  ("l" yas-describe-tables "list")))
  :bind (("C-c y" . yasnippet/body))
  :custom ((yas-global-mode . 1)))

;; --- magit --- ;;

(leaf magit)

;; --- projectile -- ;;

(leaf projectile
  :require t
  :pretty-hydra ("Open"
                 (("f" projectile-find-file "find file")
                  ("d" projectile-find-dir "find directory")
                  ("D" projectile-dired "dired on root")
                  ("b" projectile-switch-to-buffer "switch buffer")
                  ("B" projectile-display-buffer "display buffer")
                  ("r" projectile-recentf "recentf"))
                 "Search"
                 (("g" projectile-grep "grep")
                  ("o" projectile-multi-occur "multi occur"))
                 "Git"
                 (("v" projectile-vc "magit"))
                 "Others"
                 (("p" projectile-switch-project "switch project")
                  ("s" projectile-run-shell "run shell")))
  :bind ((projectile-mode-map
          ;; ("C-c p" . projectile-command-map)
          ("C-c p" . projectile/body)))
  :config
  (projectile-mode t))

;; ----- git-bash ----- ;;

;; https://qastack.jp/emacs/22049/git-bash-in-emacs-on-windows
(prefer-coding-system 'utf-8)
(setq explicit-shell-file-name (find-exe "bash.exe"))
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

(leaf saveplace
  :init
  (save-place-mode +1))

(leaf so-long
  :init
  (global-so-long-mode +1))

(setq-default show-trailing-whitespace t)

;; 特定モードで show-trailing-whitespace を無効にする
;; ref. https://www.reddit.com/r/emacs/comments/e1vos6/any_way_to_disable_showtrailingwhitespace_in_the/
(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook
                ;; https://github.com/abo-abo/hydra/issues/295
                lv-window-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

; --- copilot --- ;;

;; https://github.com/copilot-emacs/copilot.el
;; https://zenn.dev/lecto/articles/dad1d04c0605a1
(leaf copilot
  :el-get (copilot
           :type github
           :pkgname "copilot-emacs/copilot.el")
  :init
  (leaf editorconfig :ensure t)
  (leaf s :ensure t)
  (leaf dash :ensure t)
  :hook ((emacs-lisp-mode-hook . copilot-mode)
         (rust-mode-hook . copilot-mode)
         (svelte-mode-hook . copilot-mode)
         (tide-mode-hook . copilot-mode))
  :bind ((copilot-mode-map
          ("C-c ;" . copilot-complete))
         (copilot-completion-map
          ("C-i" . copilot-accept-completion)
          ("C-n" . copilot-next-completion)
          ("C-p" . copilot-prev-completion)))
  :config
  ;; https://github.com/copilot-emacs/copilot.el/issues/312
  (dolist (pair '((emacs-lisp-mode 2)
                  (go-mode 4)
                  (svelte-mode 2)))
    (add-to-list 'copilot-indentation-alist pair)))

(leaf copilot-chat
  :ensure t
  :pretty-hydra ("Basic"
                 (("d" copilot-chat-display "display")
                  ("s" copilot-chat-switch-to-buffer "switch to buffer")
                  ("R" copilot-chat-reset "reset"))
                 "Buffer"
                 (("ba" copilot-chat-add-current-buffer "add current buffer")
                  ("bd" copilot-chat-del-current-buffer "delete current buffer")
                  ("bl" copilot-chat-list "buffer list"))
                 "Helper"
                 (("e" copilot-chat-explain "explain selected")
                  ("f" copilot-chat-explain-defun "explain func")
                  ("wt" copilot-chat-test "write test")
                  ("wd" copilot-chat-document "write doc")
                  ("wf" copilot-chat-fix "write fix")
                  ("wo" copilot-chat-optimize "write optimized")
                  ("rs" copilot-chat-review "review selected")
                  ("rw" copilot-chat-review-whole-buffer "review whole custom"))
                 "Magit"
                 (("m" copilot-chat-insert-commit-message "insert commit message")))
  :bind (;; NOTE: c, p 辺りは使っているので "AI" の a
         ("C-c a" . copilot-chat/body))
  :custom
  ((copilot-chat-frontend . 'markdown)
   (copilot-chat-prompt-suffix . "日本語で回答して\n")
   (copilot-chat-prompt-test . "/tests 以下のコードのテストを作成して。テーブルテストの形式で書くこと。\n")
   (copilot-chat-prompt-fix . "/fix どこを修正したかも解説して。\n")
   (copilot-chat-prompt-review . "Please review the following code. 1. Show improvements as list. 2. Show improved codes as diff style while explaining which code relates to which improvement. Enclose diff sections in code blocks. 3. Show overall rating of the code from the BUD's point of view. ; Attentions: Don't show whole code. . Show only 'there is no required change' and overall rating if you find no improvements.\n")))

;; ----- start server ----- ;;
(server-start)

;; ----- (auto generated) ----- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(major-mode-hydra hydra svelte-mode breadcrumb rust-mode projectile tide typescript-mode shader-modea shader-mode company omnisharp omnisharp-mode leaf-convert leaf yasnippet yasnipet yasnnipet w3m smex paredit markdown-mode bmemagit auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
