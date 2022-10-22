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
          "~/.asdf/shims"
          "~/.asdf/bin"
          "~/bin"
          "~/.local/bin"
          "~/.cargo/bin"))

; make modeline less noisy
(setq-default mode-line-format
              '("%l:%c "
                "%b "
                mode-line-misc-info))

;; some basic global settings
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
              shell-file-name "fish"
              initial-major-mode 'org-mode
              eldoc-echo-area-use-multiline-p 4
              semantic-idle-truncate-long-summaries t
              eldoc-prefer-doc-buffer t
              kg/last-shell-cmd ""
              compilation-environment '("TERM=xterm-256color")
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

;; eww-mode browser keybinding
(add-hook 'eww-mode-hook
          (lambda ()
            (setq shr-use-fonts nil
                  shr-use-fonts nil
                  shr-use-colors nil
                  shr-indentation 2
                  eww-search-prefix "https://ddg.gg/html?q="
                  shr-width 120)))

(use-package vterm
  :ensure t)

(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.9/libexec/languagetool-commandline.jar"))

(use-package eldoc-box
  :ensure t
  :config
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
          '((sequence "NOTES" "TODO" "IN-PROGRESS" "DONE" "1-1" "IDEA")))
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
  (setq projectile-indexing-method 'alien)
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
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-hook 'rust-mode-hook
            (lambda ()
              (kg/lang-std)
              (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package ruby-mode
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
            (lambda ()
              (show-paren-mode))))

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
      
(defun kg/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'kg/advice-compilation-filter)

(defun kg/toggle-shell ()
  (interactive)
  ;; if shell exists toggle view on/off
  (let ((shell-name (concat "*vterm " (projectile-project-name) "*")))
    (if (get-buffer shell-name)
        (if (and (get-buffer-window shell-name))
            (delete-other-windows)
          (let ((w2 (split-window-sensibly)))
            (set-window-buffer w2 shell-name)))
      ;; else split the screen and create shell
      (let ((w1 (selected-window))
            (w2 (split-window-sensibly)))
        (select-window w2)
        (projectile-run-vterm)
        (display-line-numbers-mode -1)
        (select-window w1)
        (set-window-buffer w2 shell-name)))))

(defun kg/shell-send (cmd &optional set-last-cmd-p)
  (interactive)
  (with-current-buffer (concat "*vterm " (projectile-project-name) "*")
    (read-only-mode -1)
    (message cmd)
    (vterm-send-string cmd)
    (vterm-send-return)
    (end-of-buffer)
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

(setq kg/font-size 140)
(defun kg/global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size kg/font-size))
  (setq kg/font-size (+ size kg/font-size)))

(defun kg/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (kg/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun kg/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun kg/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (kg/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(use-package dired-sidebar
  :ensure t
  :init
  (setq dired-sidebar-close-sidebar-on-file-open t)
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

(transient-define-prefix kg/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" kg/magit-diff-with-difftastic)
   ("s" "Difftastic Show" kg/magit-show-with-difftastic)])

(transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" kg/magit-aux-commands))

(define-key magit-status-mode-map (kbd "#") #'kg/magit-aux-commands)

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
               "n f" 'helm-projectile-find-file
               "n g" 'projectile-grep
               "n w" 'list-buffers
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
    :major-modes (rjsx-mode rust-mode go-mode java-mode c-mode c++-mode)
    :bindings ("g g" 'xref-find-definitions
               "g p" 'pop-tag-mark
               "g r" 'eglot-rename
               "g h" 'eldoc
               "t"   'eldoc-box-eglot-help-at-point
               "g u" 'eglot-reconnect
               "g l" 'xref-find-references)))

(load-theme 'gruvbox-dark-medium t)

(set-face-attribute 'default nil
                    :family "Fira Code Retina"
                    :height kg/font-size)

(set-frame-font "Fira Code Retina")


(scroll-bar-mode -1)

