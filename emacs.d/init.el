;;
;; Packages and General stuff
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(shell-command "touch ~/.emacs.d/custom.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'exec-path "/usr/local/bin")

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defun my-send-to-shell (cmd)
  "sends a command to the buffer containing an active shell"
  (interactive)
  (let ((proc (get-process "shell"))
        (curbuf (current-buffer)))
    (setq command (concat cmd "\n"))
    (process-send-string proc command)
    (setq last-shell-cmd cmd)
    (switch-to-buffer curbuf)))

(defun my-send-to-shell-again ()
  "sends the previous command to the active shell"
  (interactive)
  (my-send-to-shell last-shell-cmd))

(defun my-send-to-shell-input ()
  "gets the user command and sends to the buffer containing an active shell"
  (interactive)
  (my-send-to-shell (read-string "CMD: ")))

(use-package yaml-mode
  :ensure t)

(use-package solarized-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox-dark-soft 1))

(use-package evil
  :ensure t
  :init
  (evil-mode))

(use-package magit
  :ensure t
  :init
  (use-package evil-magit
    :ensure t))

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

(use-package org
  :ensure t
  :init
  (use-package ox-gfm
    :ensure t)
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC E") 'org-gfm-export-to-markdown)
              (define-key evil-normal-state-local-map (kbd "SPC r") 'my-org-refresh)
              (define-key evil-normal-state-local-map (kbd "SPC p") 'org-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC e") 'my-start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC o c") 'org-toggle-checkbox))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package go-mode
  :ensure t
  :mode "\\*\\.go"
  :config
  (add-hook 'go-mode-hook
			(lambda ()
			  (define-key evil-normal-state-local-map (kbd "SPC t") 'godef-describe)
			  (setq exec-path (append exec-path '("~/go/bin/")))
			  (setq gofmt-command "goimports")
			  (add-hook 'before-save-hook 'gofmt-before-save)
			  (use-package company-go
				:ensure t
				:init
				(set (make-local-variable 'company-backends) '(company-go))
				(company-mode)))))

(use-package poly-markdown
  :ensure t)

(use-package poly-R
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
			  (define-key evil-normal-state-local-map (kbd "SPC r r") 'ess-eval-function-or-paragraph-and-step)
              )))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
    (add-hook 'after-init-hook 'global-company-mode))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil)
  (add-hook 'neotree-mode-hook
			(lambda ()
			  (display-line-numbers-mode -1)
			  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
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
                       "t t" 'shell
                       "v l" 'my-send-to-shell-input
                       "v p" 'my-send-to-shell-again
					   ;; buffer keybindings
					   "n n" 'next-buffer
					   "n s" 'next-multiframe-window 
					   "n p" 'previous-buffer
					   "n o" 'delete-other-windows
					   "n d" 'kill-buffer-and-window
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
                    :height 110
                    :weight 'extra-light)
(global-display-line-numbers-mode)
(load-theme 'gruvbox-dark-soft 1)

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
