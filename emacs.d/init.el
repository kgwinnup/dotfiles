;; Set up the PATH and other environment variables
(mapcar (lambda (path)
          (setenv "PATH" (concat path ":" (getenv "PATH")))
          (add-to-list 'exec-path path))
        '("/usr/local/bin"
          "/usr/local/go/bin"
          "~/go/bin"
          "/opt/homebrew/bin"
          "/usr/local/texlive/2023/bin/universal-darwin/"
          "~/bin"
          "~/.local/bin"
          "~/.opam/default/bin"
          "~/.cargo/bin"))

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Various settings
(setq-default ring-bell-function 'ignore
              require-final-newline t
              warning-minimum-level :emergency
              comp-async-report-warnings-errors nil
              compilation-scroll-output t
              scroll-step 1
              gc-cons-threshold 100000000
              gc-cons-percentage 0.6
              gc-cons-threshold (* 384 1024 1024)
              read-process-output-max (* 3 1024 1024)
              scroll-conservatively  10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              column-number-mode t
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              show-paren-mode t
              initial-scratch-message nil
              inhibit-startup-screen t
              auto-save-default nil
              make-backup-files nil
              shell-file-name "bash"
              initial-major-mode 'org-mode
              eldoc-echo-area-use-multiline-p 4
              semantic-idle-truncate-long-summaries t
              eldoc-prefer-doc-buffer t
              kg/last-shell-cmd ""
              compilation-environment '("TERM=xterm-256color")
              backup-directory-alist '(("" . "~/.emacs.d/backup"))
              comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output t
              comint-move-point-for-output t
              ;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L184
              auto-mode-case-fold nil
              evil-want-keybinding nil)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(use-package magit :ensure t :demand t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package poly-markdown :ensure t :defer t)

(use-package helm
  :ensure t
  :config
  (custom-set-faces
   '(helm-source-header
     ((t (:foreground "#ffffff" :background "#333333" :weight bold :height 1.3)))))
  (custom-set-faces
   '(helm-selection ((t (:background "#333333")))))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-projectile
  :ensure t)

(setq browse-url-browser-function 'eww-browse-url)
;; eww-mode browser keybinding
(setq shr-use-fonts nil
      shr-use-fonts nil
      shr-use-colors nil
      shr-external-rendering-functions nil
      shr-indentation 2
      eww-search-prefix "https://ddg.gg/html?q="
      shr-width 80)

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds '(; ("https://lobste.rs/rss" lobsters)
                       ; ("https://hnrss.org/frontpage" hackernews)
                       ("https://drewdevault.com/blog/index.xml" devault)
                       ("https://danluu.com/atom.xml" danluu)
                       ("https://lwn.net/headlines/rss" lwn)
                       ("https://tilde.news/rss" tildeverse)))
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "<tab>") 'company-select-next))

(defun kg/start-code-block ()
  "starts a code block in org mode"
  (interactive)
  (insert "#+begin_src R :session :results output\n\n#+end_src")
  ;(insert "#+begin_src\n\n#+end_src")
  (previous-line)
  (previous-line))

(use-package org
  :ensure t
  :defer t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-startup-folded t)
  (setq org-src-preserve-indentation t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (local-set-key (kbd "C-c C-r") 'org-babel-remove-result)
              (define-key evil-normal-state-local-map (kbd "SPC g i") 'org-toggle-inline-images)
              (define-key evil-normal-state-local-map (kbd "SPC g s") 'org-sort)
              (define-key evil-normal-state-local-map (kbd "SPC s e") 'org-sort-entries)
              (define-key evil-normal-state-local-map (kbd "SPC s n") 'kg/start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC s o") 'org-edit-src-code)
              ;(define-key evil-normal-state-local-map (kdb "SPC l") 'org-latex-preview)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC o") 'org-toggle-checkbox)))
    :config
    (setq org-todo-keyword-faces
          '(("NOTES" . "coral")
            ("TODO" . "dodger blue")
            ("IN-PROGRESS" . "orange")
            ("DONE" . "systemGrayColor")
            ("1-1" . "systemRedColor")
            ("IDEA" . "lime green")))
    (setq org-todo-keywords
          '((sequence "NOTES" "TODO" "IN-PROGRESS" "DONE" "1-1" "IDEA"))))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (turn-on-orgtbl))))

(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'alien)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (setq projectile-project-search-path '("~/workspace/"))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (projectile-mode +1))

(use-package eldoc-box
  :ensure t
  :config
  ;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (set-face-attribute 'eldoc-box-border nil :background (face-attribute 'mode-line-inactive :background)))

(defmacro kg/lang-std ()
  `(progn
     (show-paren-mode)
     (eglot-ensure)
     (company-mode)))

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode web-mode) . (eglot-deno "deno" "lsp")))
  (add-hook 'web-mode-hook  (lambda ()
                              (kg/lang-std)
                              (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-hook 'rust-mode-hook
            (lambda ()
              (kg/lang-std)
              (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (kg/lang-std)
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(add-hook 'c-mode-hook (lambda () (kg/lang-std)))
(add-hook 'c++-mode-hook (lambda () (kg/lang-std)))
(add-hook 'emacs-lisp-mode-hook (lambda () (progn (show-paren-mode) (company-mode))))

(use-package dired-sidebar
  :ensure t
  :init
  (setq dired-sidebar-close-sidebar-on-file-open t)
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :ensure t
  :demand t
  :init
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package bind-map
  :ensure t
  :init
  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :bindings ("c o" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
               "c l" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))
               "s s" 'ispell
               "s r" 'ispell-region
               ;; cli integrations
               "v u" 'projectile-compile-project
               ;; perspectives
               "p p" 'persp-prev
               "p n" 'persp-next
               "p s" 'persp-switch
               "p r" 'persp-rename
               "p k" 'persp-kill
               ;; buffer keybindings
               "n k" (lambda () (interactive) (mapc 'kill-buffer (buffer-list)) (switch-to-buffer "*scratch*"))
               "n n" 'next-buffer
               "n p" 'previous-buffer
               ; "n t" (lambda () (interactive) (dired (projectile-acquire-root)))
               "n t" (lambda () (interactive) (dired-sidebar-toggle-sidebar))
               "n s" 'next-multiframe-window
               "n o" 'delete-other-windows
               "n d" 'kill-buffer-and-window
               "n b" 'helm-mini
               "n r" (lambda () (interactive) (switch-to-buffer "*scratch*"))
               "n a" (lambda () (interactive) (find-file "~/workspace/org/notes.org"))
               "n m" (lambda () (interactive) (helm-find-files-1 "~/workspace/org/"))
               "n f" 'helm-projectile-find-file
               "n g" 'projectile-grep
               "n w" 'list-buffers
               "_" 'split-window-vertically
               "|" 'split-window-horizontally
               ;; general movement
               "j" 'evil-scroll-down
               "k" 'evil-scroll-up
               ;; magit
               "m s" 'magit
               "m b" 'magit-blame-addition
               "m f" 'magit-log-buffer-file
               "m l" 'elfeed
               "m e" (lambda () (interactive) (eww-browse-url (read-string "url: ")))
               ;; view
               "=" (lambda () (interactive) (kg/global-font-size 10))
               "-" (lambda () (interactive) (kg/global-font-size -10))))
  (bind-map-for-mode-inherit my-eglot-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :major-modes (rust-mode go-mode c-mode c++-mode web-mode tuareg-mode js-mode)
    :bindings ("g g" 'xref-find-definitions
               "g p" 'pop-tag-mark
               "g r" 'eglot-rename
               "g h" 'eldoc
               "t"   'eldoc-box-help-at-point
               "g u" 'eglot-reconnect
               "g l" 'xref-find-references)))


(use-package darcula-theme
  :ensure t
  :config
  ;; for whatever reason, the darcula theme doesn't set this value
  ;; correctly and the default value for the foreground color is Blue
  ;; which is kind of out of place.
  (custom-set-faces '(persp-selected-face ((t (:weight bold :background "#cc7832")))))
  (custom-set-faces '(company-tooltip-selection ((t (:weight bold :foreground "#9876aa")))))
  (load-theme 'darcula t)
  (set-frame-font "JetBrains Mono"))

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
