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
                    leaf
                    leaf-keywords
                    vue-mode
                    bash-completion
                    git-link
                    yaml-mode
                    terraform-mode
                    dockerfile-mode
                    diminish
                    hydra
                    idomenu
                    breadcrumb
                    avy
                    leaf-convert
                    mermaid-mode))

(exec-path-from-shell-initialize)
(bash-completion-setup)

;; --- keybind --- ;;

(mapc #'(lambda (pair)
         (global-set-key (kbd (car pair)) (cdr pair)))
      '(("M-g"  . goto-line)
        ("C-h"  . delete-backward-char)
        ("C-z"  . nil)
        ("C-_"  . undo)
        ("C-\\" . undo)
        ("C-o"  . nil)
        ("C-x i" . nil)
        ("C-x C-i" . indent-region)
        ("M-*" . pop-tag-mark)
        ("M-o" . other-window)
        ("C-c d" . insert-today)
        ("C-c i" . insert-register)))

(defun insert-today ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(global-set-key [C-down-mouse-1] nil)

;; --- Environment --- ;;

(setq scroll-conservatively 100000
      scroll-margin 3)
(set-face-foreground 'font-lock-comment-face "#ee0909")
(show-paren-mode t)
(tool-bar-mode 0)
(if (>= emacs-major-version 29)
    (global-display-line-numbers-mode 1)
    (global-linum-mode t))
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

(defalias 'yes-or-no-p 'y-or-n-p)

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

;; leaf

(leaf leaf-keywords
  :ensure t
  :init
  (leaf el-get :ensure t)
  :config
  (leaf-keywords-init))

;; --- hydra --- ;;

(defhydra hydra-common-replace (:hint nil :exit t :color blue)
  "replace"
  ("q" query-replace "query replace")
  ("s" replace-string "replace string")
  ("r" replace-regexp "replace regexp"))

(leaf hydra
  :require t
  :bind
  (("C-c :" . (defhydra hydra-common
                (:hint nil :exit t :color blue)
                "common"
                ("c" comment-region "comment")
                ("u" uncomment-region "uncomment")
                ("b" revert-buffer "revert buffer")
                ("e" flycheck-list-errors "flycheck list errors" )
                ("r" hydra-common-replace/body ">replace")
                ("i" idomenu "imenu as ido"))))
  :config
  (setq hydra-is-helpful t))

;; --- magit --- ;;

(leaf magit
  :bind (("C-c g s" . magit-status))
  :custom
  ((magit-list-refs-sortby . "-creatordate")))

;; --- tree-sitter --- ;;

(when (>= emacs-major-version 29)
  (require 'treesit))

;; --- perl --- ;;

(defalias 'perl-mode 'cperl-mode)

(defun copy-perl-package-name ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (beginning-of-buffer)
      (search-forward " ")
      (call-interactively 'set-mark-command)
      (search-forward ";")
      (backward-char)
      (call-interactively 'kill-ring-save))))

;; https://journal.lampetty.net/entry/wp/384
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(leaf cperl-mode
  :bind ((cperl-mode-map :package cperl-mode
          ("C-c p t" . perltidy-region)
          ;; "p" is already used as "package"
          ;; so use "h" (head of buffer) instead.
          ("C-c c h" . copy-perl-package-name)
          ("M-." . xref-find-definitions)))
  :mode ("\\.pl\\'" "\\.pm\\'" "\\.t\\'")
  :hook ((cperl-mode-hook . hs-minor-mode)
         (cperl-mode-hook . flycheck-mode))
  :custom
  ((cperl-indent-level . 4)
   (cperl-indent-parens-as-block . t)
   (cperl-indent-subs-specially . nil)
   (cperl-close-paren-offset . -4)))

;; --- javascript --- ;;

(leaf js-mode
  :bind ((:js-mode-map
          ("M-." . xref-find-definitions))))

;; --- web-mode --- ;;

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (set-face-foreground 'web-mode-html-tag-face "Blue1")
  (set-face-foreground 'web-mode-html-attr-name-face "ForestGreen")
  (set (make-local-variable 'company-backends)
       '(company-dabbrev company-dabbrev-code company-semantic))
  (cond ((string-match "\\.php\\'" buffer-file-name)
         (my-php-hook-for-web-mode))))

(leaf web-mode
  :mode ("\\.mt\\'" "\\.html\\'" "\\.php\\'")
  :hook ((web-mode-hook . my-web-mode-hook))
  :setq ((web-mode-enable-auto-expanding . t)))

;; - company - ;;
(leaf company-mode
  :init
  (global-company-mode)
  :custom
  ((company-dabbrev-downcase . nil)
   (company-dabbrev-ignore-case . nil)
   (company-minimum-prefix-length . 2)
   (company-idle-delay . 0.1)
   ;; ハイフンを補完対象文字列に追加 ("\\sw" (default) + "-")
   (company-dabbrev-char-regexp . "\\(\\sw\\|-\\)"))
  :bind
  (("C-;" . company-manual-begin)
   (:company-active-map
    ("M-n". nil)
    ("M-p". nil)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-s" . company-search-filtering)))
  :hook
  (go-mode-hook . (lambda ()
                    (set (make-local-variable 'company-backends)
                         '(company-dabbrev))
                    (setq-local page-delimiter "^\\(func\\|const\\|type\\)")
                    ;; subword-mode でアンダーバー単位で移動させるための設定
                    ;; https://emacs.stackexchange.com/questions/62095/treat-hyphens-as-part-of-a-word
                    ;; FIXME: syntaxハイライトに副作用がある
                    ;; NOTE: インデント時に行頭のアンダーバーが消失する副作用があったのでいったん断念
                    ;; (modify-syntax-entry ?_ "-")
                )))

;; - go - ;;

(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

(defun copy-go-function-name ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((regex "^func \\(([^()]*)\\)?[^(]*"))
        (search-backward-regexp regex)
        (search-forward-regexp regex))
      (search-backward " ")
      (forward-char)
      (call-interactively 'set-mark-command)
      (search-forward "(")
      (backward-char)
      (call-interactively 'kill-ring-save))))

(defun go-find-test (test-name dir)
  (interactive "sTest Name: \nDBase directory: ")
  (grep-compute-defaults)
  (rgrep test-name "*_test.go" dir))

(defun goto-go-next-func ()
  (interactive)
  (end-of-line)
  (search-forward-regexp "^func")
  (beginning-of-line))

(defun goto-go-prev-func ()
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^func"))

(defun go-upcase-prev-word ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((prev-subword-mode (if subword-mode 1 -1)))
        (unwind-protect
            (progn (subword-mode -1)
                   (call-interactively 'backward-word)
                   (subword-mode 1)
                   (call-interactively 'capitalize-word))
          (subword-mode prev-subword-mode))))))

(defun go-downcase-prev-word ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((prev-subword-mode (if subword-mode 1 -1)))
        (unwind-protect
            (progn (subword-mode -1)
                   (call-interactively 'backward-word)
                   (subword-mode 1)
                   (call-interactively 'downcase-word))
          (subword-mode prev-subword-mode))))))

(leaf go-mode
  :bind
  ((:go-mode-map
    ("M-[" . xref-find-references)
    ("C-c c f" . copy-go-function-name)
    ("C-c m n" . goto-go-next-func)
    ("C-c m p" . goto-go-prev-func)
    ("C-c c u" . go-upcase-prev-word)
    ("C-c c l" . go-downcase-prev-word)))
  :custom
  ((gofmt-command . "goimports")
   (c-basic-offset . 4)
   (tab-width . 4))
  :config
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook 'subword-mode)
    (add-hook 'go-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook 'company-mode)
    (add-hook 'go-mode-hook (lambda () (auto-complete-mode -1)))))

;; .tmpl ファイル用の設定
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . my-tmpl-mode))

(defun my-tmpl-mode ()
  (setq indent-tabs-mode t))

(add-hook 'my-tmpl-mode-hook 'my-tmpl-mode)


;; - protobuf - ;;

(defun format-protobuf-hook ()
  (when (eq major-mode 'protobuf-mode)
    (shell-command (concat "buf format -w " (buffer-file-name)))
    (revert-buffer t t t)))

(leaf protobuf-mode
  :hook ((after-save-hook . format-protobuf-hook))
  :custom
  ((c-basic-offset . 2)
   (require-final-newline . t)))

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

(leaf vue-mode
  :hook ((vue-mode-hook . company-mode))
  :pre-setq ((js-indent-level . 2))
  :require t)

;; --- yasnippet --- ;;

(defun refresh-yas-minor-mode ()
  (interactive)
  (yas-minor-mode)
  (yas-minor-mode))

(leaf yasnippet
  :ensure t
  :diminish yas-minor-mode ; モードラインに非表示
  :bind (("C-c y" . (defhydra hydra-yasnippet
                      (:hint nil :exit t :color blue)
                      "yasnippet"
                      ("i" yas-insert-snippet "insert snippet")
                      ("n" yas-new-snippet "new snippet")
                      ("v" yas-visit-snippet-file "visit snippet file")
                      ("l" yas-describe-tables "list")
                      ("r" refresh-yas-minor-mode "refresh yas minor mode"))))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt)))

;; --- eglot --- ;;

(leaf eglot
  :bind (("C-c e" . (defhydra hydra-eglot
                      (:hint nil :exit t :color blue)
                      "eglot"
                      ("r" xref-find-references "reference")
                      ("s" eglot-rename "rename symbol")
                      ("." xref-find-definitions "rename symbol")))))

(leaf flycheck-eglot
  :ensure t
  :config
  (global-flycheck-eglot-mode 1))

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
        typescript-indent-level 2
        tide-server-max-response-length 204800))

(leaf tide
  :bind (("C-c t" . (defhydra hydra-tide
                      (:hint nil :exit t :color blue)
                      "tide"
                      ("f" tide-fix "fix")
                      ("r" tide-references "reference")
                      ("s" tide-rename-symbol "rename symbol")))))

(defun setup-ts-eglot ()
  (eglot-ensure)
  (company-mode +1)
  (flycheck-mode +1)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode))

;; require:
;; $ brew install tree-sitter
;; $ npm install -g typescript-language-server typescript
;; 言語用のdylibを下記からダウンロードして ~/.emacs.d/tree-sitter/ に配置
;; https://github.com/casouri/tree-sitter-module/releases
(if (>= emacs-major-version 29)
    (progn (when (not (treesit-language-available-p 'typescript))
             (treesit-install-language-grammar 'typescript))
           (add-hook 'typescript-ts-mode-hook #'setup-ts-eglot)
           (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))
    (progn (add-hook 'typescript-mode-hook #'setup-tide-mode)
           (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))))

(leaf typescript-ts-mode
  :custom
  ((company-idle-delay . 2)))

(defun toggle-ts-mode ()
  (interactive)
  (if (eq major-mode 'typescript-mode)
      (typescript-ts-mode)
      (when (eq major-mode 'typescript-ts-mode)
        (typescript-mode))))

;; --- JavaScript --- ;;

(leaf js-mode
  :hook
  (js-mode-hook . (lambda ()
                    (eglot-ensure)
                    (company-mode +1)
                    (flycheck-mode +1)
                    (flycheck-add-mode 'javascript-eslint))))

;; --- Python --- ;;

(leaf python-mode
  :hook ((python-mode-hook . company-mode)
         (python-mode-hook . flycheck-mode)))

;; --- Java --- ;;

;; 最初に .java を開いた際に Eclipse JDT language server がインストールされる
;; 手元ではインストール時に警告が出ており動作もしていなかったが、
;; Emacs を再起動してからは問題なく動作しているように見える
(leaf eglot-java
  :ensure t
  :hook ((java-mode-hook . eglot-java-mode))
  :bind ((eglot-java-mode-map
          :package eglot-java-mode
          ("C-c j" . (defhydra hydra-eglot-java
                       (:hint nil :exit t :color blue)
                       "eglot-java"
                       ("n" eglot-java-file-new "new file")
                       ("x" eglot-java-run-main "run main")
                       ("t" eglot-java-run-test "run test")
                       ("N" eglot-java-project-new "new project")
                       ("T" eglot-java-project-build-task "build task")
                       ("R" eglot-java-project-build-refresh "build refresh"))))))

;; --- PHP --- ;;

(leaf company-php
  :ensure t
  ;; ac-php-remake-tags-all はPHPファイル外 (.ac-php-conf.json) でも使うので事前にrequireしておく
  ;; (実行場所を考えれば my-php-mode-setup の中のrequireで十分かも)
  :require t)

;; flycheck-phpstan が web-mode に対応していなかった...
;; (leaf flycheck-phpstan
;;   :ensure t)

(defun indent-after (key)
  (insert key)
  (indent-for-tab-command))

;; NOTE: defun では引数の "key" が lambda に渡らなかったので defmacro で定義。
;; lexical-binding を有効にしても渡らなかったので
;; 実行時にはバインディングの情報が消えていると思われる
(defmacro make-indent-after (key)
  `(lambda ()
     (interactive)
     (indent-after ,key)))

;; https://qiita.com/tadsan/items/a76768439869f00a4e89
(defun my-php-mode-setup ()
  "My PHP-mode hook."
  (subword-mode 1)
  (setq show-trailing-whitespace t)

  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

  ;; TODO: (結局当面使うことはなさそうなので再度使う場合のメモ)
  ;; ↓まだphp-modeは諦め切れない (HTML埋め込み以外のケースであれば使いたい) ので
  ;; php-modeか否かで初期化処理を分岐させる

  ;; phpcsもphp-modeのみ対応だった...
  ;; (setq flycheck-phpcs-standard "PSR2")
  ;; (flycheck-select-checker 'php-phpcs)

  ;; (require 'flycheck-phpstan)
  (flycheck-mode t)
  ;; (flycheck-select-checker 'phpstan)
  ;; (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  ;;(add-to-list 'flycheck-disabled-checkers 'php-phpcs)

  ;; https://github.com/xcwen/ac-php
  (company-mode t)
  (require 'company-php)
  (ac-php-core-eldoc-setup)
  (set (make-local-variable 'company-backends)
       '((company-ac-php-backend company-dabbrev-code)
         company-capf
         company-files)))

(defun my-php-hook-for-web-mode ()
  (my-php-mode-setup)
  (define-key web-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
  (define-key web-mode-map (kbd "M-*") 'ac-php-location-stack-back)
  (define-key web-mode-map (kbd ";") (make-indent-after ";"))
  (define-key web-mode-map (kbd "}") (make-indent-after "}")))

;; NOTE: 純粋なPHPでは割りと良い感じに動くものの
;; HTML埋め込みだと全くうまく動かないのでいったんweb-modeの方で色々できないか模索中
;; (leaf php-mode
;;   :ensure t
;;   :hook ((php-mode . my-php-mode-setup))
;;   :bind ((:php-mode-map
;;           ("M-." . ac-php-find-symbol-at-point)
;;           ("M-*" . ac-php-location-stack-back)))
;;   :custom
;;   ((php-manual-url . 'ja)
;;    (php-mode-coding-style . 'psr2)
;;    (php-mode-template-compatibility . nil)))

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
  (let ((gls "/opt/homebrew/bin/gls"))
    (when (file-exists-p gls)
      (setq insert-directory-program gls))))

(setq dired-listing-switches "-alh --group-directories-first")

(ffap-bindings)

;; recentf
(setq recentf-max-saved-items 10000
      recentf-auto-cleanup 'never
      recentf-exclude '("/recentf" "COMMIT_EDITING" "/.?TAGS" "ido\\.last")
      ;; https://emacs.stackexchange.com/questions/45697/prevent-emacs-from-messaging-when-it-writes-recentf
      recentf-auto-save-timer (run-with-idle-timer (* 3 60) t 'recentf-save-list))

(defun ido-recentf ()
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-c C-r") 'ido-recentf)
(global-set-key (kbd "C-c r r") 'ido-recentf)

(recentf-mode 1)

;; display the directory name of the file when files that have a same name are opened
(leaf uniquify
  :require t
  :setq ((uniquify-buffer-name-style quote post-forward-angle-brackets)))

;; auto complete
;; (require 'auto-complete-config)
;; (ac-config-default)

;; replace "option" and "command"
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)

;; use back slash instead of yen mark (¥ = 0x5c)
(define-key global-map [?¥] [?\\])

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
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

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

(defun upcase-after-underbar ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match (downcase (match-string 0)) t))))

(leaf avy
  :bind
  (("C-c C-s" . avy-goto-char-timer)
   ("C-c s s" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.5))

;; --- ido-mode --- ;;

(leaf ido
  :bind (("C-x C-f" . ido-find-file))
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t
        ffap-machine-p-known 'reject
        ido-use-filename-at-point nil
        ;; https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode
        ido-auto-merge-work-directories-length -1))

(leaf smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(leaf ido-vertical-mode
  :require t
  :setq ((ido-vertical-define-keys quote C-n-and-C-p-only)
         (ido-max-window-height . 0.75))
  :config
  (ido-vertical-mode t))

;; --- ddskk --- ;;

(leaf ddskk
  :bind
  (("C-x C-j" . skk-mode))
  :hook
  (find-file-hooks . (lambda () (skk-latin-mode 1)))
  (git-commit-setup-hook . (lambda () (skk-latin-mode 1))))

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

(leaf projectile
  :bind ((projectile-mode-map
          ("C-c p" . projectile-command-map)))
  :init
  (projectile-mode t)
  :require t)

;; --- mermaid mode --- ;;

(leaf mermaid-mode
  :mode ("\\.mermaid\\'")
  :setq ((mermaid-mmdc-location . "docker")
         ;; require: docker pull minlag/mermaid-cli:11.4.0
         (mermaid-flags . "run -u 1000 -v /tmp:/tmp minlag/mermaid-cli:11.4.0")))

;; --- Common Lisp --- ;;

(leaf slime
  :ensure t
  :setq ((inferior-lisp-program . "ros run"))
  :hook ((slime-mode-hook . company-mode))
  :config (slime-setup))

(leaf slime-company
  :ensure t)

(leaf paredit
  :ensure t
  :bind ((paredit-mode-map
          ("C-c f" . paredit-forward-slurp-sexp)
          ("C-c b" . paredit-forward-barf-sexp)
          ("C-h" . paredit-backward-delete)
          ;; ("C-c p" . paredit-backward)
          ;; ("C-c n" . paredit-forward)
          ("C-M-p" . paredit-backward)
          ("C-M-n" . paredit-forward)))
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (slime-mode-hook . enable-paredit-mode)
         (slime-repl-mode-hook . enable-paredit-mode)))

;; --- obsidian --- ;;

;; https://github.com/copilot-emacs/copilot.el
;; https://github.com/licht1stein/obsidian.el?tab=readme-ov-file#installation
(leaf obsidian
  :ensure t
  :require t
  :bind ((obsidian-mode-map
          ("C-c o" . obsidian-hydra/body)
          ("M-." . obsidian-follow-link-at-point)
          ;; NOTE: 元のページに戻るをいったん簡易的に。
          ;; しばらく使って今いちなら自分で履歴を管理する
          ("M-*" . switch-to-prev-buffer)))
  :custom
  (;; This directory will be used for `obsidian-capture' if set.
   (obsidian-inbox-directory . "Inbox")
   ;; The directory for daily notes (file name is YYYY-MM-DD.md)
   (obsidian-daily-notes-directory . "Daily Notes"))
  :config
  (obsidian-specify-path "~/Google Drive/マイドライブ/Obsidian/main")
  (global-obsidian-mode t))

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
  :bind ((copilot-mode-map
          ("C-c ;" . copilot-complete))
         (copilot-completion-map
          ("C-i" . copilot-accept-completion)
          ("C-m" . copilot-accept-completion)
          ("C-n" . copilot-next-completion)
          ("C-p" . copilot-prev-completion)))
  :config
  ;; https://github.com/copilot-emacs/copilot.el/issues/312
  (dolist (pair '((emacs-lisp-mode 2)
                  (go-mode 4)))
    (add-to-list 'copilot-indentation-alist pair)))

;; --- for project --- ;;

(defun some-admin-jsx ()
  (interactive)
  (js-jsx-mode)
  (setq js-indent-level 2))

;; --- auto generated --- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-close-paren-offset -4 t)
 '(cperl-indent-level 4 t)
 '(cperl-indent-parens-as-block t t)
 '(cperl-indent-subs-specially nil t)
 '(package-selected-packages
   '(mermaid-mode leaf-convert avy breadcrumb breadcrumb-mode idomenu leaf-keywords diminish hydra highlight-indentation csv-mode dockerfile-mode tide typescript-mode jsonnet-mode git-link bash-completion leaf graphql-mode projectile yaml-mode ido-vertical-mode markdowne-mode terraform-mode go-errcheck eglot powerline csharp-mode vue-mode dired-sidebar flycheck yasnippet use-package web-mode japanese-holidays smex markdown-mode magit auto-complete ddskk)))
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
(put 'list-timers 'disabled nil)
