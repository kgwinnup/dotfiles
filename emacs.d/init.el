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

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-font "mononoki 14" nil t)

(setq-default
 ring-bell-function 'ignore
 scroll-step 1
 scroll-conservatively  10000
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse 't
 indent-tabs-mode t
 c-basic-offset 4
 tab-width 4)

;;(load-theme 'naysayer 1)
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
			  (setq exec-path (append exec-path '("~/go/bin/")))
			  (setq gofmt-command "goimports")
			  (add-hook 'before-save-hook 'gofmt-before-save)
			  (use-package company-go
				:ensure t
				:init
				(set (make-local-variable 'company-backends) '(company-go))
				(company-mode)))))

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
			  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
			  (define-key evil-normal-state-local-map (kbd "SPC n t") 'neotree-hide)
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
					   ;; buffer keybindings
					   "n n" 'next-buffer
					   "n s" 'next-multiframe-window 
					   "n p" 'previous-buffer
					   "n o" 'delete-other-windows
					   "n d" 'kill-buffer-and-window
					   ;; visual/theme stuff
					   "t d" '(lambda () (interactive) (load-theme 'solarized-dark))
					   "t l" '(lambda () (interactive) (load-theme 'solarized-light))
					   "t g" '(lambda () (interactive) (load-theme 'gruvbox-dark-soft))
					   "=" 'default-text-scale-increase
					   "-" 'default-text-scale-decrease)))
