
(getenv "HOME")

;; Packages and setup stuff
;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(require 'ox-latex)

;; set up all the PATH and other environment variables
(mapcar (lambda (path)
          (setenv "PATH" (concat path ":" (getenv "PATH")))
          (add-to-list 'exec-path path))
        '("/usr/local/bin"
          "/usr/local/go/bin"
          "~/go/bin"
          "~/bin"
          "~/workspace/jdk-17.0.1.jdk/Contents/Home/bin"
          "~/.cargo/bin"))

(mapcar (lambda (path)
          (setenv "JAVA_HOME" (concat path ":" (getenv "JAVA_HOME"))))
        '("~/workspace/jdk-17.0.1.jdk/Contents/Home"))

(mapcar (lambda (path)
          (setenv "CLASSPATH" (concat path ":" (getenv "CLASSPATH"))))
        '("/Users/kgwinnup/workspace/jdt-language-server-1.6.0/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar"))

;; make modeline less noisy
(setq-default mode-line-format
              '("%l:%c "
                "%b "
                mode-line-misc-info
                ))

;; some basic global settings
(setq-default ring-bell-function 'ignore
              require-final-newline t
              warning-minimum-level :emergency
              comp-async-report-warnings-errors nil
              compilation-scroll-output t
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
              shell-file-name "bash"
              initial-major-mode 'org-mode
              eldoc-echo-area-use-multiline-p 4
              semantic-idle-truncate-long-summaries t
              eldoc-prefer-doc-buffer t
              kg/last-shell-cmd ""
              backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; other settings that are not global variables
(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(add-hook 'after-make-frame-functions (lambda () (interactive) (scroll-bar-mode -1)))
(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)
(global-hl-line-mode)

;; locate for the OS X 
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
                  shr-width 120)))

(defun kg/toggle-shell ()
  (interactive)
  ;; if shell exists toggle view on/off
  (if (get-buffer "*eshell*")
      (if (and (get-buffer-window "*eshell*"))
          (delete-other-windows)
        (let ((w2 (split-window-sensibly)))
          (set-window-buffer w2 "*eshell*")))
    ;; else split the screen and create shell
    (let ((w1 (selected-window))
          (w2 (split-window-sensibly)))
      (select-window w2)
      (eshell)
      (display-line-numbers-mode -1)
      (select-window w1)
      (set-window-buffer w2 "*eshell*"))))

(use-package perspective
  :ensure t
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
     (python . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-startup-folded t)
  (setq org-src-preserve-indentation t)
  (use-package org-present :ensure t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC g i") 'org-toggle-inline-images)
              (define-key evil-normal-state-local-map (kbd "SPC s e") 'org-sort-entries)
              (define-key evil-normal-state-local-map (kbd "SPC s n") 'kg/start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC s o") 'org-edit-src-code)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC o") 'org-toggle-checkbox)))
    :config
    (setq org-todo-keyword-faces
          '(("NOTES" . "coral")
            ("TODO" . "dodger blue")
            ("DOING" . "lime green")
            ("DONE" . "dark gray")))
    (setq org-todo-keywords
          '((sequence "NOTES" "TODO" "DOING" "DONE")))
    (setq org-latex-create-formula-image-program 'dvipng)
    (setq org-preview-latex-default-process 'dvipng)
    (eval-after-load "org-present"
      '(progn
         (add-hook 'org-present-mode-hook
                   (lambda ()
                     (local-set-key (kbd "C-c +") #'(lambda () (interactive) (kg/global-font-size 10)))
                     (local-set-key (kbd "C-c -") #'(lambda () (interactive) (kg/global-font-size -10)))
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

(use-package projectile
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (setq projectile-project-search-path '("~/workspace/"))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (projectile-mode +1))

(use-package eglot :ensure t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package poly-markdown :ensure t :defer t)
(use-package poly-R :ensure t :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (turn-on-orgtbl)
              (turn-on-orgstruct++))))
  
(defun kg/toggle-ess-r ()
  (interactive)
  (let ((name (concat "*R:" (projectile-project-name) "*")))
    (if (get-buffer name)
        (if (and (get-buffer-window name))
            (delete-other-windows)
          (let ((w2 (split-window-horizontally)))
            (set-window-buffer w2 name))))))

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
              (define-key evil-normal-state-local-map (kbd "SPC r s") 'kg/toggle-ess-r)
              (define-key evil-normal-state-local-map (kbd "SPC r r") (lambda () (interactive) (ess-eval-function-or-paragraph-and-step))))))

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

(use-package rust-mode
  :ensure t
  :init
  ;(setq rust-format-on-save t)
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
(add-hook 'java-mode-hook (lambda () (kg/lang-std) (add-hook 'before-save-hook 'eglot-format nil t)))

(use-package xterm-color
  :ensure t
  :config
  (setq compilation-environment '("TERM=xterm-256color")))

(defun kg/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;;
;; eshell 
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

(defun kg/clear-eshell ()
  (interactive)
  (kg/eshell-send "clear 1"))

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

(defun kg/eshell-send-again ()
  "sends the previous command to the active shell"
  (interactive)
  (kg/eshell-send kg/last-shell-cmd t))

(defun kg/eshell-input ()
  "gets the user command and sends to the buffer containing an active shell"
  (interactive)
  (kg/eshell-send (read-string "CMD: ") t))

(add-hook 'eshell-mode-hook
          (setq eshell-scroll-to-bottom-on-output t)
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

(setq kg/font-size 140)
(defun kg/global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size kg/font-size))
  (setq kg/font-size (+ size kg/font-size)))

(use-package bind-map
  :ensure t
  :init
  (bind-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :bindings ("c o" (lambda () (interactive) (find-file "~/.emacs.d/init.el")) 
               "c l" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))
               "c k" 'describe-function
               "s s" 'ispell
               "s r" 'ispell-region
               ;; cli integrations
               "t t" 'kg/toggle-shell
               "t T" 'eshell
               "v u" 'projectile-compile-project
               "v p" 'kg/eshell-input
               "v l" 'kg/eshell-send-again
               ;; perspectives
               "p p" 'persp-prev
               "p n" 'persp-next
               "p s" 'persp-switch
               "p r" 'persp-rename
               ;; buffer keybindings
               "n k" (lambda () (interactive) (mapc 'kill-buffer (buffer-list)) (switch-to-buffer "*scratch*"))
               "n n" 'next-buffer
               "n p" 'previous-buffer
               "n s" 'next-multiframe-window 
               "n o" 'delete-other-windows
               "n d" 'kill-buffer-and-window
               "n b" 'helm-mini
               "n r" (lambda () (interactive) (switch-to-buffer "*scratch*"))
               "n a" (lambda () (interactive) (find-file "~/workspace/notes.org"))
               "n f" 'helm-projectile-find-file
               "n w" 'list-buffers
               ;; general movement
               "j" 'evil-scroll-down
               "k" 'evil-scroll-up
               ;; magit
               "m s" 'magit
               "m b" 'magit-blame-addition
               "m e" (lambda () (interactive) (eww-browse-url (read-string "url: ")))
               ;; view
               "," 'rename-buffer
               "=" (lambda () (interactive) (kg/global-font-size 10))
               "-" (lambda () (interactive) (kg/global-font-size -10))))
  (bind-map-for-mode-inherit my-eglot-map my-base-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :major-modes (rjsx-mode rust-mode go-mode java-mode c-mode c++-mode)
    :bindings ("g g" 'xref-find-definitions
               "g p" 'pop-tag-mark
               "g r" 'eglot-rename
               "g h" 'eldoc
               "g f" 'projectile-grep
               "g =" 'comment-or-uncomment-region
               "g l" 'xref-find-references)))

(use-package 'flatland-theme
  :ensure t)

(load-theme 'flatland t)

(set-face-attribute 'default nil
                    :family "Fira Code Retina"
                    :height kg/font-size)

(scroll-bar-mode -1)

