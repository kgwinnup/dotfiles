;;
;; Packages and setup stuff
;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; for use when running shells within emacs, this sets the path for
;; those shells
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/.local/share/nvm/v12.22.1/bin" (getenv "PATH")))
(setenv "GTAGSLIBPATH" "~/.gtags")

;; for use when emacs it self calls out to find programs needed for
;; various plugin features
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/.local/share/nvm/v12.22.1/bin")

;; some basic global settings
(setq-default ring-bell-function 'ignore
              require-final-newline t
              warning-minimum-level :emergency
              comp-async-report-warnings-errors nil
              mac-allow-anti-aliasing nil
              scroll-step 1
              scroll-conservatively  10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              initial-scratch-message nil
              inhibit-startup-screen t
              auto-save-default nil
              make-backup-files nil
              my-last-shell-cmd ""
              shell-file-name "/usr/local/bin/fish"
              backup-directory-alist '(("" . "~/.emacs.d/backup"))
              default-directory "~/workspace/"
              custom-file "~/.emacs.d/custom.el")

;(setq browse-url-browser-function
;      '(("lobste.rs" . eww-browse-url)
;        ("slashdot.org" . eww-browse-url)
;        ("." . browse-url)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode)
(global-hl-line-mode)

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)))

(shell-command "touch ~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/llvm-mode.el")

;; this is for vterm and R shells, will move cursor to bottom of
;; buffer after sending command
(eval-after-load "comint"
  '(progn
     (setq comint-move-point-for-output 'others)))

;; eww-mode browser keybinding
(add-hook 'eww-mode-hook
          (lambda ()
            (setq shr-use-fonts nil
                  shr-use-fonts nil
                  shr-use-colors nil
                  shr-indentation 2
                  eww-search-prefix "https://ddg.gg/html?q="
                  shr-width 120)
            (local-set-key "n" 'shr-next-link)
            (local-set-key "p" 'shr-previous-link)
            (local-set-key "u" 'eww-back-url)
            (define-key evil-normal-state-local-map (kbd "SPC g p") 'eww-back-url)))

;;
;; eshell 
;;

(defun my-eshell-git-info ()
  (if (magit-get-current-branch)
      (propertize (concat " (" (magit-get-current-branch) ")") 'face `(:foreground "#ebdbb2"))
    ""))

(defun my-eshell-pwd ()
  (let ((ret (if (cl-search (getenv "HOME") (eshell/pwd))
                 (concat "~" (substring (eshell/pwd) (length (getenv "HOME")) nil))
               (eshell/pwd))))
    (propertize ret 'face `(:foreground "#b8bb26"))))

(setq eshell-prompt-function
      (lambda ()
        (concat (my-eshell-pwd)
                (my-eshell-git-info)
                (propertize " $ " 'face `(:foreground "#ebdbb2")))))

(defun my-clear-eshell ()
  "clears the eshell buffer, does not set my-last-eshell-cmd"
  (interactive)
  (my-send-to-eshell "clear 1"))

(defun my-send-to-eshell (cmd &optional set-last-cmd-p)
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
        (setq my-last-shell-cmd cmd))))

(defun my-send-to-eshell-again ()
  "sends the previous command to the active shell"
  (interactive)
  (my-send-to-eshell my-last-shell-cmd t))

(defun my-send-to-eshell-input ()
  "gets the user command and sends to the buffer containing an active shell"
  (interactive)
  (my-send-to-eshell (read-string "CMD: ") t))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key company-active-map (kbd "RET") nil)
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "man")
            (add-to-list 'eshell-visual-subcommands '("docker" "attach"))
            (add-to-list 'eshell-visual-subcommands '("git" "log"))
            (add-to-list 'eshell-visual-subcommands '("git" "status"))
            (add-to-list 'eshell-visual-subcommands '("git" "diff"))
            ;; adds color support to eshell stdout
            (setenv "TERM" "xterm-256color")))

;;
;; general shell functions
;;
(defun my-toggle-shell (the-shell)
  "toggles the shells visibility to the right split window,
'the-shell' parameter should be the symbol name as a string for the
shell"
  (interactive)
  (let ((shell-string (concat "*" the-shell "*")))
    ;; if shell exists toggle view on/off
    (if (get-buffer shell-string)
        (if (and (get-buffer-window shell-string))
            (delete-other-windows)
          (let ((w2 (split-window-horizontally)))
            (set-window-buffer w2 shell-string)))
      ;; else split the screen and create shell
      (let ((w1 (selected-window))
            (w2 (split-window-horizontally)))
        (select-window w2)
        (funcall (intern the-shell))
        (display-line-numbers-mode -1)
        (select-window w1)
        (set-window-buffer w2 shell-string)))))

;;
;; org-mode functions 
;;

(defun my-start-code-block ()
  "starts a code block in org mode"
  (interactive)
  (insert "#+begin_src\n\n#+end_src")
  (previous-line)
  (previous-line))

(defun my-org-refresh ()
  "refreshes tag alignment and table contents"
  (interactive)
  (org-align-all-tags)
  (org-table-recalculate-buffer-tables))

(defun my-org-timestamp ()
  "sets heading timestamp field"
  (interactive)
  (insert ":DATE: ")
  (org-insert-time-stamp (current-time)))

(setq my-font-size 170)
(defun my-global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size my-font-size))
  (setq my-font-size (+ size my-font-size)))


(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package vterm
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package helm
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
    :config
  (setq helm-boring-buffer-regexp-list
      (quote
       (  "\\Minibuf.+\\*"
          "\\` "
          "\\*Messages\\*"
          "\\*Async.+\\*"
          "\\*Warnings\\*"
          "\\*elfeed.+\\*"
          "\\*helm.+\\*"
          "\\*Shell.+\\*"
          "\\magit"))))

(use-package helm-gtags
  :ensure t
  :init
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'helm-gtags-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'helm-gtags-find-tag-from-here)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'helm-gtags-pop-stack)
              (define-key evil-normal-state-local-map (kbd "SPC g f") 'helm-gtags-select)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'helm-gtags-parse-file)
              (define-key evil-normal-state-local-map (kbd "SPC g u") 'helm-gtags-update-tags))))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package helm-themes
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

(use-package magit
  :ensure t)

(require 'ox-latex)
(use-package org
  :ensure t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (R . t)
     (shell . t)
     ))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (use-package ox-gfm
    :ensure t)
  (use-package org-present
    :ensure t)
    (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              ;(add-hook 'after-save-hook 'org-preview-latex-fragment)
              (define-key evil-normal-state-local-map (kbd "SPC r r") 'org-preview-latex-fragment)
              (define-key evil-normal-state-local-map (kbd "SPC E") 'org-gfm-export-to-markdown)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC R") 'my-org-refresh)
              (define-key evil-normal-state-local-map (kbd "SPC T") 'my-org-timestamp)
              (define-key evil-normal-state-local-map (kbd "SPC p") 'org-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'org-global-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC g t") '(lambda () (interactive) (org-insert-time-stamp (current-time))))
              (define-key evil-normal-state-local-map (kbd "SPC s n") 'my-start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC s o") 'org-edit-src-code)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC o") 'org-toggle-checkbox)))
    :config
    (setq org-todo-keyword-faces
          '(("project" . "dodger blue")
            ("notes" . "coral")
            ("mgmt" . "magenta")
            ("review" . "deep pink")
            ("todo" . "plum")
            ("doing" . "lime green")
            ("backlog" . "dim gray")))
    (setq org-todo-keywords
          '((sequence "project" "notes" "mgmt" "todo" "doing" "backlog")))
    (setq org-latex-create-formula-image-program 'dvipng)
    (setq org-preview-latex-default-process 'dvipng)
    (eval-after-load "org-present"
      '(progn
         (add-hook 'org-present-mode-hook
                   (lambda ()
                     (local-set-key (kbd "C-c +") '(lambda () (interactive) (my-global-font-size 10)))
                     (local-set-key (kbd "C-c -") '(lambda () (interactive) (my-global-font-size -10)))
                     (turn-off-evil-mode)
                     (org-present-big)
                     (display-line-numbers-mode -1)
                     (org-display-inline-images)
                     (org-present-hide-cursor)
                     (org-present-read-only)))
         (add-hook 'org-present-mode-quit-hook
                   (lambda ()
                     (turn-on-evil-mode)
                     (display-line-numbers-mode t)
                     (org-present-small)
                     (org-remove-inline-images)
                     (org-present-show-cursor)
                     (org-present-read-write))))))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (turn-on-orgtbl)
              (turn-on-orgstruct++))))

(require 'ansi-color)
(use-package projectile
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (projectile-mode +1))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  :init
  (add-hook 'rust-mode-hook
			(lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'lsp-find-definition)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'pop-tag-mark)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'lsp-find-references)
              (define-key evil-normal-state-local-map (kbd "SPC g i") 'lsp-describe-thing-at-point))))

(use-package lsp-ui
  :ensure t
  :after lsp-mode)

(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :commands lsp-deferred
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024 3)) ;; 1mb
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.1)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-log-io nil)
  :init 
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
                    :major-modes '(go-mode)
                    :remote? t
                    :server-id 'gopls-remote)))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package go-mode
  :ensure t
  :mode "\\*\\.go"
  :init
  (add-hook 'go-mode-hook
			(lambda ()
			  (setq gofmt-command "goimports")
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'lsp-find-definition)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'pop-tag-mark)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'lsp-find-references)
              (define-key evil-normal-state-local-map (kbd "SPC g i") 'lsp-describe-thing-at-point)
			  (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package js2-mode
  :ensure t)

(defun my-ess-toggle-R ()
  (interactive)
  (let ((name (concat "*R:" (projectile-project-name) "*")))
    (if (get-buffer name)
        (if (and (get-buffer-window name))
            (delete-other-windows)
          (let ((w2 (split-window-horizontally)))
            (set-window-buffer w2 name))))))

(use-package ess
  :ensure t
  :mode (("\\*\\.R" . ess-site)
         ("\\*\\.Rmd" . ess-site))
  :commands R
  :hook (ess-mode-hook . subword-mode)
  :config
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq scroll-down-aggressively 0.01)
  (setq ess-fancy-comments nil)
  :init
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+R-mode))
  (add-hook 'ess-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC r s") 'my-ess-toggle-R)
              (define-key evil-normal-state-local-map (kbd "SPC r r") (lambda () (interactive) (ess-eval-function-or-paragraph-and-step))))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-lsp-cache-candidates t)
  (setq company-lsp-async t)
  ;(setq evil-collection-company-use-tng nil)
  (define-key company-active-map (kbd "C-SPC") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (add-to-list 'company-backends 'company-gtags)
  (add-hook 'after-init-hook 'global-company-mode))
  ;(with-eval-after-load 'company
  ;  (define-key company-active-map [tab] 'company-complete-cycle-next)
  ;  (define-key company-active-map (kbd "TAB") 'company-complete-cycle-next)))

;;
;; neotree
;;
(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly"
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil)
  :init
  (display-line-numbers-mode -1)
  (add-hook 'neotree-mode-hook
			(lambda ()
              (display-line-numbers-mode -1)
			  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter-hide)
			  (define-key evil-normal-state-local-map (kbd "SPC n t") 'neotree-hide)
			  (define-key evil-normal-state-local-map (kbd "SPC n h") 'neotree-hidden-file-toggle)
			  (define-key evil-normal-state-local-map (kbd "SPC n r") 'neotree-refresh)
			  (define-key evil-normal-state-local-map (kbd "SPC n s") 'next-multiframe-window)
			  (define-key evil-normal-state-local-map (kbd "SPC n p") 'neotree-change-root))))

(use-package restclient
  :ensure t
  :init
  (add-hook 'restclient-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC r r") 'restclient-http-send-current-stay-in-window))))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds '(("https://lobste.rs/rss" lobsters)
                       ("https://tilde.news/rss" tildeverse)
                       ("feed:https://lwn.net/headlines/rss" lwn)
                       ("http://rss.slashdot.org/Slashdot/slashdotMain" slashdot)))
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(use-package writegood-mode
  :ensure t)

(use-package bind-map
  :ensure t
  :init
  (use-package default-text-scale
	:ensure t)
  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :bindings ("c o" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")) 
               "c l" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
               "c k" 'describe-function
               "s s" 'ispell
               "s r" 'ispell-region
               "s g" 'writegood-mode
               ;; cli integrations
               "t t" '(lambda () (interactive) (my-toggle-shell "eshell"))
               "t T" 'eshell
               "v p" 'my-send-to-eshell-input
               "v l" 'my-send-to-eshell-again
               "v u" 'projectile-compile-project
               ;; buffer keybindings
               "n e" 'window-swap-states
               "n k" (lambda () (interactive) (mapc 'kill-buffer (buffer-list)))
               "n t" 'neotree-toggle
               "n n" 'next-buffer
               "n s" 'next-multiframe-window 
               "n p" 'previous-buffer
               "n o" 'delete-other-windows
               "n d" 'kill-buffer-and-window
               "n b" 'helm-buffers-list
               "n r" '(lambda () (interactive) (switch-to-buffer "*scratch*"))
               "n a" '(lambda () (interactive) (find-file "~/workspace/notes.org"))
               "n f" 'make-frame
               "n w" 'list-buffers
               ;; general movement
               "j" 'evil-scroll-down
               "k" 'evil-scroll-up
               ;; magit
               "m s" 'magit
               "m b" 'magit-blame-addition
               "m l" 'elfeed
               "m g" 'gnus
               "m e" '(lambda () (interactive) (eww-browse-url (read-string "url: ")))
               "m r" 'restclient-mode
               ;; view
               "d t" (lambda () (interactive) (progn (disable-theme 'gruvbox-dark-hard) (load-theme 'tsdh-light) (set-face-background 'mode-line "gold")))
               "d g" (lambda () (interactive) (load-theme 'gruvbox-dark-hard))
               "d f" (lambda () (interactive) (toggle-frame-fullscreen))
               "|" 'split-window-right
               "=" (lambda () (interactive) (my-global-font-size 10))
               "-" (lambda () (interactive) (my-global-font-size -10)))))

;(load-theme 'tsdh-light)
;(set-face-background 'mode-line "gold")
(load-theme 'gruvbox-dark-medium t)

(set-face-attribute 'default nil
                    :family "mononoki"
                    :height my-font-size)
                    ;:weight 'medium)

