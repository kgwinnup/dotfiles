;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ))
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;(load-theme 'naysayer 1)
(load-theme 'solarized-dark 1)
(tool-bar-mode -1)
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
  :ensure t)

(require 'bind-map)
(bind-map my-base-leader-map
		  :keys ("M-m")
		  :evil-keys ("SPC")
		  :evil-states (normal motion visual)
		  :bindings ("n t" 'neotree-toggle
					 "c o" 'open-config-file
					 "c l" 'reload-config-file
					 ;; buffer keybindings
					 "n n" 'next-buffer
					 "n p" 'previous-buffer
					 "n o" 'delete-other-windows
					 "n d" 'kill-buffer-and-window))

(add-hook 'neotree-mode-hook
		  (lambda ()
			(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
			(define-key evil-normal-state-local-map (kbd "t") 'neotree-hide)
			(define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)))

(use-package go-mode
  :ensure t
  :init)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))
