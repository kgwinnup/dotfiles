;;
;; Packages and General stuff
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(shell-command "touch ~/.emacs.d/custom.el")
(setq custom-file "~/.emacs.d/custom.el")
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/go/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/go/bin")
(load custom-file)
(setq default-directory "~/workspace/")
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Bootstrap `use-package`
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;
;; custom functions
;;

(defun my-toggle-shell ()
  "toggles the shell window"
  (interactive)
    (if (and (get-buffer-window "*shell*"))
        (delete-other-windows)
      (progn (shell)
             (display-line-numbers-mode -1)
             (switch-to-buffer "*shell*")
             (other-window -1))))

(defun my-send-to-shell (cmd)
  "sends a command to the buffer containing an active shell"
  (interactive)
  (let ((proc (get-process "shell"))
        (curbuf (current-buffer)))
    (setq command (concat cmd "\n"))
    (process-send-string proc command)
    (setq last-shell-cmd cmd)
    (switch-to-buffer "shell")
    (end-of-buffer)
    (switch-to-buffer curbuf)))

(defun my-send-to-shell-again ()
  "sends the previous command to the active shell"
  (interactive)
  (my-send-to-shell last-shell-cmd))

(defun my-send-to-shell-input ()
  "gets the user command and sends to the buffer containing an active shell"
  (interactive)
  (my-send-to-shell (read-string "CMD: ")))

(defun my-start-code-block ()
  "starts a code block in org mode"
  (interactive)
  (insert "#+BEGIN_SRC\n\n#+END_SRC")
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

(use-package helm
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-boring-buffer-regexp-list
      (quote
       (  "\\Minibuf.+\\*"
          "\\` "
          "\\magit"
          "\\*.+\\*"))))

(use-package helm-gtags
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :init)

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
  :ensure t
  :init
  (use-package evil-magit
    :ensure t))

(use-package org
  :ensure t
  :init
  (use-package ox-gfm
    :ensure t)
  (use-package org-present
    :ensure t)
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (turn-off-evil-mode)
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (turn-on-evil-mode)
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write)))))
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (define-key evil-normal-state-local-map (kbd "SPC E") 'org-gfm-export-to-markdown)
              (define-key evil-normal-state-local-map (kbd "SPC r") 'my-org-refresh)
              (define-key evil-normal-state-local-map (kbd "SPC p") 'org-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-global-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC e r") 'my-start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC e e") 'org-edit-src-code)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC T") 'my-org-timestamp)
              (define-key evil-normal-state-local-map (kbd "SPC o p") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC o c") 'org-toggle-checkbox))))

(use-package markdown-mode
  :ensure t
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

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package lsp-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :commands lsp-deferred
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024 3)) ;; 1mb
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.1)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-log-io nil))



;(use-package company-lsp
;  :ensure t
;  :commands company-lsp
;  :after (lsp-mode company))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package go-mode
  :ensure t
  :mode "\\*\\.go"
  :config
  (add-hook 'go-mode-hook
			(lambda ()
			  (setq exec-path (append exec-path '("~/go/bin/")))
			  (setq gofmt-command "goimports")
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'godef-jump)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'pop-tag-mark)
              (define-key evil-normal-state-local-map (kbd "SPC g d") 'godoc-at-point)
			  (add-hook 'before-save-hook 'gofmt-before-save))))

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

(use-package ess
  :ensure t
  :mode (("\\*\\.R" . ess-site)
         ("\\*\\.Rmd" . ess-site))
  :commands R
  :hook (ess-mode-hook . subword-mode)
  :defer t
  :init
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq scroll-down-aggressively 0.01)
  (setq ess-fancy-comments nil)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (progn
          (setq w1 (selected-window))
          (setq w2 (split-window-horizontally))
          (R)
          (set-window-buffer w2 "*R*")
          (select-window w1))))
  (add-hook 'ess-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC r s") 'my-ess-start-R)
              (define-key evil-normal-state-local-map (kbd "SPC r r") 'ess-eval-function-or-paragraph-and-step))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-lsp-cache-candidates t)
  (setq company-lsp-async t)
  (add-hook 'after-init-hook 'global-company-mode))


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
  :init
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil)
  (add-hook 'neotree-mode-hook
			(lambda ()
			  (display-line-numbers-mode -1)
			  ;;(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
			  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter-hide)
			  (define-key evil-normal-state-local-map (kbd "SPC n t") 'neotree-hide)
			  (define-key evil-normal-state-local-map (kbd "SPC n h") 'neotree-hidden-file-toggle)
			  (define-key evil-normal-state-local-map (kbd "SPC n r") 'neotree-refresh)
			  (define-key evil-normal-state-local-map (kbd "SPC n s") 'next-multiframe-window)
			  (define-key evil-normal-state-local-map (kbd "SPC n p") 'neotree-change-root))))

(use-package bind-map
  :ensure t
  :init
  (use-package default-text-scale
	:ensure t)
  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :bindings ("n t" 'neotree-toggle
               "c o" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")) 
               "c l" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
               "t t" 'my-toggle-shell
               "v p" 'my-send-to-shell-input
               "v l" 'my-send-to-shell-again
               "s s" 'ispell
               ;; buffer keybindings
               "n n" 'next-buffer
               "n s" 'next-multiframe-window 
               "n p" 'previous-buffer
               "n o" 'delete-other-windows
               "n d" 'kill-buffer-and-window
               "n b" 'helm-mini
               "n r" '(lambda () (interactive) (switch-to-buffer "*scratch*"))
               "j" 'evil-scroll-down
               "k" 'evil-scroll-up
               ;; magit
               "m s" 'magit
               ;; view
               "=" 'default-text-scale-increase
               "-" 'default-text-scale-decrease)))

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 160
                    :weight 'extra-light)

(global-display-line-numbers-mode)
(global-hl-line-mode)
(load-theme 'gruvbox-dark-medium 1)
(setq-default mac-allow-anti-aliasing nil)

(setq-default ring-bell-function 'ignore
              scroll-step 1
              scroll-conservatively  10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              initial-scratch-message nil)

(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+R-mode))

