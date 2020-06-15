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

(defun open-config-file ()
  "opens the local configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-config-file ()
  "reloads the init.el file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow))

(use-package flycheck
  :ensure t)

(use-package go-mode
  :ensure t
  :mode "\\*\\.go"
  :config
  (add-hook 'go-mode-hook
			(lambda ()
			  (setq exec-path (append exec-path '("~/go/bin/")))
			  ;;(use-package go-errcheck
			  ;;	:ensure t)
			  ;;(flycheck-mode)
			  (setq gofmt-command "goimports")
			  (add-hook 'before-save-hook 'gofmt-before-save)
			  (use-package company-go
				:ensure t
				:config (set (make-local-variable 'company-backends)
							 '(company-go))
				(company-mode)))))
			  

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package bind-map
  :ensure t
  :init
  (bind-map my-base-leader-map
			:keys ("M-m")
			:evil-keys ("SPC")
			:evil-states (normal motion visual)
			:bindings ("n t" 'neotree-toggle
					   "c o" 'open-config-file
					   "c l" 'reload-config-file
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
					   "=" 'text-scale-increase
					   "-" 'text-scale-decrease))
  (add-hook 'neotree-mode-hook
		  (lambda ()
			(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "SPC n t") 'neotree-hide)
			(define-key evil-normal-state-local-map (kbd "SPC n r") 'neotree-refresh)
			(define-key evil-normal-state-local-map (kbd "SPC n s") 'next-multiframe-window)
			(define-key evil-normal-state-local-map (kbd "SPC n p") 'neotree-change-root))))

