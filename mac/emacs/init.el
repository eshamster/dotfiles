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
        ("C-c i" . insert-register)
        ;; list-buffers は使わないが良く押し間違えるので上書き
        ("C-x C-b" . switch-to-buffer)))

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

(savehist-mode 1) ; 各種履歴をEmacs終了後も残す
(setq savehist-additional-variables '(register-alist))

;; clock
;; (モードラインの時計余り見たことないなと思ったのでdisable)
(display-time-mode -1)

;; eww

(setq eww-search-prefix "https://www.google.com/search?q=")

;; leaf

(leaf leaf-keywords
  :ensure t
  :init
  (leaf el-get :ensure t)
  (leaf hydra :ensure t)
  (leaf major-mode-hydra :ensure t)
  :config
  (leaf-keywords-init))

;; minions: モードラインの minor-mode の表示を折り畳む
(leaf minions
  :ensure t
  :config
  (minions-mode 1))

;; --- hydra --- ;;

(leaf hydra
  :require t
  :pretty-hydra ("Common"
                 (("c" comment-region "comment")
                  ("u" uncomment-region "uncomment")
                  ("b" revert-buffer "revert buffer")
                  ("e" flycheck-list-errors "show errors")
                  ("i" idomenu "imenu as ido"))
                 "Replace"
                 (("rq" query-replace "query replace")
                  ("rs" replace-string "replace string")
                  ("rr" replace-regexp "replace regexp")))
  :bind
  (("C-c :" . hydra/body))
  :config
  (setq hydra-is-helpful t))

;; --- git --- ;;

(leaf magit
  :custom
  ((magit-list-refs-sortby . "-creatordate")))

(leaf diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

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
  :pretty-hydra ("Move"
                 (("n" goto-go-next-func "next")
                  ("p" goto-go-prev-func "prev"))
                 "Edit"
                 (("u" go-upcase-prev-word "upcase")
                  ("l" go-downcase-prev-word "downcase"))
                 "Copy"
                 (("f" copy-go-function-name "copy function name"))
                 "Find"
                 (("t" go-find-test "find test")))
  :bind
  ((:go-mode-map
    ("M-[" . xref-find-references)
    ("C-c g" . go-mode/body)))
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
  :pretty-hydra ("Insert"
                 (("i" yas-insert-snippet "insert"))
                 "Edit"
                 (("n" yas-new-snippet "new")
                  ("v" yas-visit-snippet-file "visit")
                  ("l" yas-describe-tables "list"))
                 "Others"
                 (("r" refresh-yas-minor-mode "refresh")))
  :bind (("C-c y" . yasnippet/body))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt)))

;; --- eglot --- ;;

(leaf eglot
  :pretty-hydra ("Jump"
                 (("r" xref-find-references "reference")
                  ("." xref-find-definitions "definition"))
                 "Refactor"
                 (("s" eglot-rename "rename symbol"))
                 "Others"
                 (("c" eglot-reconnect "reconnect")))
  :bind (("C-c e" . eglot/body)))

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
  :pretty-hydra ("tide"
                 (("f" tide-fix "fix")
                  ("r" tide-references "reference")
                  ("s" tide-rename-symbol "rename symbol")))
  :bind ((:tide-mode-map
          ("C-c t" . tide/body))))

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

(leaf python
  :bind ((python-mode-map
          ("C-m" . (lambda ()
                     (interactive)
                     ;; カーソルの直前の文字が ")", ",", "}" の場合は改行前にインデント
                     (when (string-match "[,)}]" (string (char-before)))
                       (indent-for-tab-command))
                     (newline-and-indent)))))
  :hook ((python-mode-hook . company-mode)
         (python-mode-hook . flycheck-mode))
  :custom
  ;; 開き確固直後に改行した際のインデントを1つ分にする (default: 2)
  ((python-indent-def-block-scale . 1)))

;; --- Java --- ;;

;; 最初に .java を開いた際に Eclipse JDT language server がインストールされる
;; 手元ではインストール時に警告が出ており動作もしていなかったが、
;; Emacs を再起動してからは問題なく動作しているように見える
(leaf eglot-java
  :ensure t
  :hook ((java-mode-hook . eglot-java-mode))
  :pretty-hydra ("Common"
                 (("n" eglot-java-file-new "new file")
                  ("x" eglot-java-run-main "run main")
                  ("t" eglot-java-run-test "run test"))
                 "Project"
                 (("N" eglot-java-project-new "new project")
                  ("bt" eglot-java-project-build-task "build task")
                  ("br" eglot-java-project-build-refresh "build refresh")))
  :bind ((eglot-java-mode-map
          ("C-c j" . eglot-java/body))))

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

(global-set-key (kbd "C-c C-r") 'recentf-open)
(global-set-key (kbd "C-c r r") 'recentf-open)

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
                minibuffer-setup-hook
                ;; https://github.com/abo-abo/hydra/issues/295
                lv-window-hook))
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
  :ensure t
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
                 (("p" projectile-switch-project "switch project")))
  :bind ((projectile-mode-map
          ;; ("C-c p" . projectile-command-map)
          ("C-c p" . projectile/body)))
  :init
  (projectile-mode t))

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
          ("M-i" . indent-according-to-mode)
          ("C-w" . copilot-accept-completion-by-word)
          ("C-l" . copilot-accept-completion-by-line)
          ("M-n" . copilot-next-completion)
          ("M-p" . copilot-prev-completion)))
  :custom
  ((copilot-idle-delay . 0.1))
  :hook ((go-mode-hook . copilot-mode)
         (emacs-lisp-mode-hook . copilot-mode)
         (web-mode-hook . copilot-mode)
         (python-mode-hook . copilot-mode)
         (markdown-mode-hook . (lambda ()
                                 (copilot-mode t)
                                 ;; 一応ONにしてみたが余り役に立たないので明示的な補完のみにする
                                 (setq-local copilot-idle-delay nil))))
  :config
  ;; https://github.com/copilot-emacs/copilot.el/issues/312
  (dolist (pair '((go-mode 4)
                  (markdown-mode 4)
                  (emacs-lisp-mode 2)
                  (web-mode 2)))
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
