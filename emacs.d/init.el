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
(setq mac-command-modifier 'meta)

;; for use when running shells within emacs, this sets the path for
;; those shells
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setenv "PATH" (concat "~/.local/share/nvm/v12.22.1/bin" (getenv "PATH")))
(setenv "GTAGSLIBPATH" "~/.gtags")

;; for use when emacs it self calls out to find programs needed for
;; various plugin features
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/.local/share/nvm/v12.22.1/bin")

;; some basic global settings
(setq-default ring-bell-function 'ignore
              require-final-newline t
              warning-minimum-level :emergency
              comp-async-report-warnings-errors nil
              compilation-scroll-output t
              mac-allow-anti-aliasing nil
              scroll-step 1
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
              my-last-shell-cmd ""
              shell-file-name "fish"
              backup-directory-alist '(("" . "~/.emacs.d/backup"))
              default-directory "~/workspace/")

(require 'ox-latex)
(require 'ansi-color)

(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode)
(global-hl-line-mode)
(and (display-graphic-p) (scroll-bar-mode -1))

(shell-command "touch ~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")

;; this is for shell and R shells, will move cursor to bottom of
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
;; general shell functions
;;
(defun my-toggle-shell ()
  (interactive)
  ;; if shell exists toggle view on/off
  (if (get-buffer "*shell*")
      (if (and (get-buffer-window "*shell*"))
          (delete-other-windows)
        (let ((w2 (split-window-horizontally)))
          (set-window-buffer w2 "*shell*")))
    ;; else split the screen and create shell
    (let ((w1 (selected-window))
          (w2 (split-window-horizontally)))
      (select-window w2)
      (shell)
      (display-line-numbers-mode -1)
      (select-window w1)
      (set-window-buffer w2 "*shell*"))))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package transpose-frame
  :ensure t
  :defer t)

(use-package helm
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-boring-buffer-regexp-list
      (quote
       (  "\\Minibuf.+\\*"
          "\\` "
          "\\*.+\\*"
          "<*.+>$"
          "\\magit"))))

(use-package yaml-mode
  :ensure t
  :defer t)

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

;;
;; org-mode functions 
;;

(defun my-start-code-block ()
  "starts a code block in org mode"
  (interactive)
  (insert "#+begin_src\n\n#+end_src")
  (previous-line)
  (previous-line))

(use-package org
  :ensure t
  :defer t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (use-package ox-gfm
    :ensure t)
  (use-package org-present
    :ensure t)
    (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC p") 'org-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'org-global-cycle)
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
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda ()
              (show-paren-mode)
              (eglot-ensure)
              (company-mode)
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'xref-find-definitions)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'pop-tag-mark)
              (define-key evil-normal-state-local-map (kbd "SPC g r") 'eglot-rename)
              (define-key evil-normal-state-local-map (kbd "SPC g h") 'eldoc)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'xref-find-references))))

(use-package go-mode
  :ensure t
  :mode "\\*\\.go"
  :init
  (add-hook 'go-mode-hook
			(lambda ()
              (show-paren-mode)
              (eglot-ensure)
              (company-mode)
			  (setq gofmt-command "goimports")
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'xref-find-definitions)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'pop-tag-mark)
              (define-key evil-normal-state-local-map (kbd "SPC g r") 'eglot-rename)
              (define-key evil-normal-state-local-map (kbd "SPC g h") 'eldoc)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'xref-find-references)
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package eglot
  :ensure t)
  
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" . gfm-mode))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.jsx\\'" . rjsx-mode))

(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode))

(defun my-ess-toggle-R ()
  (interactive)
  (let ((name (concat "*R:" (projectile-project-name) "*")))
    (if (get-buffer name)
        (if (and (get-buffer-window name))
            (delete-other-windows)
          (let ((w2 (split-window-horizontally)))
            (set-window-buffer w2 name))))))

(use-package poly-markdown
  :ensure t
  :defer t)

(use-package poly-R
  :ensure t
  :defer t)

(use-package ess
  :ensure t
  :defer t
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
  (define-key company-active-map (kbd "<tab>") 'company-select-next))

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

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds '(("https://lobste.rs/rss" lobsters)
                       ("https://tilde.news/rss" tildeverse)
                       ("https://lwn.net/headlines/rss" lwn)
                       ("https://news.ycombinator.com/rss" hn)
                       ("http://rss.slashdot.org/Slashdot/slashdotMain" slashdot)))
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(setq my-font-size 170)
(defun my-global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size my-font-size))
  (setq my-font-size (+ size my-font-size)))

(use-package bind-map
  :ensure t
  :init
  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :bindings ("c o" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")) 
               "c l" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
               "c k" 'describe-function
               "s s" 'ispell
               "s r" 'ispell-region
               ;; cli integrations
               "t t" 'my-toggle-shell
               "t T" 'shell
               "v u" 'projectile-compile-project
               ;; buffer keybindings
               "n e" 'transpose-frame
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
               "m e" '(lambda () (interactive) (eww-browse-url (read-string "url: ")))
               ;; view
               "," 'rename-buffer
               "=" (lambda () (interactive) (my-global-font-size 10))
               "-" (lambda () (interactive) (my-global-font-size -10)))))

(load-theme 'gruvbox-dark-hard t)
;(load-theme 'adwaita t)
;(global-hl-line-mode 0)

(set-face-attribute 'default nil
                    ;:family "mononoki"
                    :family "Fira Code Retina"
                    :height my-font-size)

