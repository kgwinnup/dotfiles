;;
;; Basic init stuff
;;

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

;; Do not load outdated byte code files.
(setq load-prefer-newer t)

;; Default was too low.
;; Increase for better lsp performance.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L184
(setq auto-mode-case-fold nil)

(require 'package)

;; Don't auto-initialize.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(setq package--init-file-ensured t)

;; Save custom vars to separate file from init.el.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; From https://irreal.org/blog/?p=8243
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

(when (< emacs-major-version 27)
  (unless package--initialized
    (package-initialize)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)
(require 'use-package)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Set up the PATH and other environment variables
(mapcar (lambda (path)
          (setenv "PATH" (concat path ":" (getenv "PATH")))
          (add-to-list 'exec-path path))
        '("/usr/local/bin"
          "/usr/local/go/bin"
          "~/go/bin"
          "/opt/homebrew/bin"
          "~/bin"
          "~/.local/bin"
          "~/.opam/default/bin"
          "~/.cargo/bin"))

(with-eval-after-load "tramp"
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Make modeline less noisy
(setq-default mode-line-format
              '("%l:%c "
                "%b "
                mode-line-misc-info))


;; Disable line numbers in compile-mode
(add-hook 'compilation-mode-hook (lambda ()
                                   (display-line-numbers-mode -1)))

;; Various settings
(setq-default ring-bell-function 'ignore
              require-final-newline t
              warning-minimum-level :emergency
              comp-async-report-warnings-errors nil
              compilation-scroll-output t
              scroll-step 1
              gc-cons-threshold 100000000
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
              comint-move-point-for-output t)

;; other settings that are not global variables
(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)
(global-hl-line-mode)

;; Locate for the OS X 
(if (eq system-type 'darwin)
    (setq helm-locate-command
          "glocate %s %s"
          helm-locate-create-db-command
          "gupdatedb --output='%s' --localpaths='%s'"))

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;;
;; Packages and settings
;;

;; eww-mode browser keybinding
(add-hook 'eww-mode-hook
          (lambda ()
            (setq shr-use-fonts nil
                  shr-use-fonts nil
                  shr-use-colors nil
                  shr-indentation 2
                  eww-search-prefix "https://ddg.gg/html?q="
                  shr-width 120)))

(use-package xterm-color
  :ensure t)

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds '(("https://lobste.rs/rss" lobsters)
                       ("https://hnrss.org/frontpage" hackernews)
                       ("https://lwn.net/headlines/rss" lwn)
                       ("https://tilde.news/rss" tildeverse)))
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(use-package ess
  :ensure t)

(use-package eldoc-box
  :ensure t
  :config
  ;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  (set-face-attribute 'eldoc-box-border nil :background (face-attribute 'mode-line-inactive :background)))

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-projectile
  :ensure t)

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package magit :ensure t)

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
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC g i") 'org-toggle-inline-images)
              (define-key evil-normal-state-local-map (kbd "SPC g s") 'org-sort)
              (define-key evil-normal-state-local-map (kbd "SPC s e") 'org-sort-entries)
              (define-key evil-normal-state-local-map (kbd "SPC s n") 'kg/start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC s o") 'org-edit-src-code)
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

(use-package eglot :ensure t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package poly-markdown :ensure t :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (turn-on-orgtbl))))

(use-package tide
  :ensure t)

(defmacro kg/lang-std ()
  `(progn
     (show-paren-mode)
     (eglot-ensure)
     (company-mode)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
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

(use-package dune
  :ensure t)

(load "~/.emacs.d/opam-user-setup.el")
(use-package tuareg
  :ensure t
  :config
  (setq tuareg-match-patterns-aligned t)
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'eglot-format nil t)
              (kg/lang-std)))
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam))))

(add-hook 'c-mode-hook (lambda () (kg/lang-std)))
(add-hook 'c++-mode-hook (lambda () (kg/lang-std)))
(add-hook 'emacs-lisp-mode-hook (lambda () (progn (show-paren-mode) (company-mode))))
      
(defun kg/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'kg/advice-compilation-filter)

(defun kg/toggle-shell ()
  (interactive)
  ;; if shell exists toggle view on/off
  (let ((eshell-name (concat "*eshell " (projectile-project-name) "*")))
    (if (get-buffer eshell-name)
        (if (and (get-buffer-window eshell-name))
            (delete-other-windows)
          (let ((w2 (split-window-sensibly)))
            (set-window-buffer w2 eshell-name)))
      ;; else split the screen and create shell
      (let ((w1 (selected-window))
            (w2 (split-window-sensibly)))
        (select-window w2)
        (projectile-run-eshell)
        (display-line-numbers-mode -1)
        (select-window w1)
        (set-window-buffer w2 eshell-name)))))

;;
;; Eshell
;;
(defun kg/eshell-git-info ()
  (if (magit-get-current-branch)
      (propertize (concat " (" (magit-get-current-branch) ")") 'face `(:foreground "#ebdbb2"))
    ""))

(defun kg/eshell-pwd ()
  (let ((ret (if (cl-search (getenv "HOME") (eshell/pwd))
                 (concat "~" (substring (eshell/pwd) (length (getenv "HOME")) nil))
               (eshell/pwd))))
    (propertize ret 'face `(:foreground "#b8bb26"))))

(setq eshell-prompt-function
      (lambda ()
        (concat (kg/eshell-pwd)
                (kg/eshell-git-info)
                (propertize " $ " 'face `(:foreground "#ebdbb2")))))

(defun kg/eshell-send (cmd &optional set-last-cmd-p)
  (interactive)
  (with-current-buffer "*eshell*"
    (evil-insert-state)
    (eshell-kill-input)
    (end-of-buffer)
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (eshell-bol)
    (if set-last-cmd-p
        (setq kg/last-shell-cmd cmd))))

(defun kg/shell-send-again ()
  "sends the previous command to the active shell"
  (interactive)
  (kg/shell-send kg/last-shell-cmd t))

(defun kg/shell-input ()
  "gets the user command and sends to the buffer containing an active shell"
  (interactive)
  (kg/shell-send (read-string "CMD: ") t))

(use-package eshell
  :ensure t
  :hook ((eshell-mode . turn-on-hide-mode-line-mode))
  :config
  (require 'xterm-color)
  (setq eshell-prefer-lisp-functions nil)
  (setq eshell-scroll-to-bottom-on-output t)
  (setq xterm-color-preserve-properties t)
  (setq-local global-hl-line-mode nil)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (define-key company-active-map (kbd "RET") nil)
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "man")
              (add-to-list 'eshell-visual-subcommands '("docker" "attach"))
              (add-to-list 'eshell-visual-subcommands '("git" "log"))
              (add-to-list 'eshell-visual-subcommands '("git" "status"))
              (add-to-list 'eshell-visual-subcommands '("git" "diff"))
              (setenv "TERM" "xterm-256color"))))

(defun kg/kill-eshell-on-exit() 
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'kg/kill-eshell-on-exit)

(setq kg/font-size 140)
(defun kg/global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size kg/font-size))
  (setq kg/font-size (+ size kg/font-size)))

(use-package dired-sidebar
  :ensure t
  :init
  (setq dired-sidebar-close-sidebar-on-file-open t)
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

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
               "v s" 'kg/toggle-shell
               "v u" 'projectile-compile-project
               "v p" 'kg/shell-input
               "v l" 'kg/shell-send-again
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
               "t"   'eldoc-box-eglot-help-at-point
               "g u" 'eglot-reconnect
               "g l" 'xref-find-references)))


;(use-package gruvbox-theme
;  :ensure t
;  :config
;  ;; for whatever reason, the darcula theme doesn't set this value
;  ;; correctly and the default value for the foreground color is Blue
;  ;; which is kind of out of place.
;  (custom-set-faces '(persp-selected-face ((t (:weight bold :foreground "#cc7832")))))
;  (load-theme 'gruvbox-dark-medium t)
;  (set-frame-font "JetBrains Mono"))

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(vscode-dark-plus-theme bind-map dired-sidebar go-mode rust-mode web-mode yasnippet poly-markdown yaml-mode dockerfile-mode eglot org-present company magit evil-collection evil helm-projectile helm perspective eldoc-box elfeed gruvbox-theme xterm-color use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
