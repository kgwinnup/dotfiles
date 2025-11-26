;; -*- lexical-binding: t; -*-

;;; ============================================================================
;;; PREVENT PACKAGE.EL CONFLICTS
;;; ============================================================================

;; This MUST be at the top before anything else loads
(setq package-enable-at-startup nil)

;;; ============================================================================
;;; PERFORMANCE & STARTUP OPTIMIZATION
;;; ============================================================================

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.1)
(setq read-process-output-max (* 3 1024 1024)) ; default 4k is too low
(setq warning-suppress-types '((treesit)))

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 10 1024 1024))
            (setq gc-cons-percentage 0.6)))

;; Enable native compilation
(require 'comp)
(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

;; ============================================================================
;;; ENVIRONMENT SETUP
;;; ============================================================================

;; Path configuration
(let ((paths '("/opt/homebrew/bin"
               "/opt/homebrew/opt/gcc/bin"
               "~/.cargo/bin"
               "~/.rustup/toolchains/stable-aarch64-apple-darwin/bin"
               "~/go/bin")))
  (dolist (path paths)
    (let ((expanded-path (expand-file-name path)))
      (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))
      (add-to-list 'exec-path expanded-path))))

;; Library paths for libgccjit
(setenv "LIBRARY_PATH"
        (concat "/opt/homebrew/opt/libgccjit/lib/gcc/current:"
                (getenv "LIBRARY_PATH")))
(setenv "DYLD_LIBRARY_PATH"
        (concat "/opt/homebrew/opt/libgccjit/lib/gcc/current:"
                (getenv "DYLD_LIBRARY_PATH")))

;;; ============================================================================
;;; PACKAGE MANAGER SETUP
;;; ============================================================================

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el as backend for use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Prevent built-in packages from conflicting with straight.el versions
(setq straight-fix-org nil)

;; Use built-in project package, don't let straight.el override it
(straight-use-package '(project :type built-in))

;;; ============================================================================
;;; GLOBAL SETTINGS & DEFAULTS
;;; ============================================================================

;; UI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-auto-revert-mode t)

;; Editor behavior
(setq-default
 ring-bell-function 'ignore
 require-final-newline t
 org-src-block-faces nil
 compilation-scroll-output t
 scroll-step 1
 scroll-conservatively 10000
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 column-number-mode t
 indent-tabs-mode nil
 c-basic-offset 4
 tab-width 4
 show-paren-mode t
 initial-scratch-message nil
 inhibit-startup-screen t
 auto-save-default nil
 make-backup-files nil
 backup-directory-alist '(("" . "~/.emacs.d/backup"))
 auto-mode-case-fold nil
 fill-column 80
 initial-major-mode 'org-mode
 compilation-environment '("TERM=xterm-256color" "CARGO_TERM_COLOR=always")
 )

;; Shell configuration
(setq shell-file-name (executable-find "fish")
      explicit-shell-file-name (executable-find "fish"))

;; Browse URL settings
(setq browse-url-browser-function 'eww-browse-url
      shr-use-fonts nil
      shr-use-colors nil
      shr-external-rendering-functions nil
      shr-indentation 2
      eww-search-prefix "https://ddg.gg/html?q="
      shr-width 80)

;;; ============================================================================
;;; MACOS TERMINAL INTEGRATION
;;; ============================================================================

;; Clipboard integration for terminal Emacs on macOS
(defun my/copy-to-clipboard (text)
  "Copy TEXT to macOS clipboard using pbcopy."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my/paste-from-clipboard ()
  "Paste from macOS clipboard using pbpaste."
  (shell-command-to-string "pbpaste"))

(unless (display-graphic-p)
  (setq interprogram-cut-function 'my/copy-to-clipboard
        interprogram-paste-function 'my/paste-from-clipboard))

;; Enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;;; ============================================================================
;;; THEME & APPEARANCE
;;; ============================================================================

(use-package solarized-theme
  :straight t
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  :config
  (load-theme 'solarized-dark t))

(defvar my/solarized-current-theme 'dark
  "Current Solarized theme: 'light or 'dark.")

(defun my/toggle-solarized-theme ()
  "Toggle between Solarized Light and Dark themes."
  (interactive)
  ;; Disable current theme first
  (mapc #'disable-theme custom-enabled-themes)
  ;; Switch theme
  (if (eq my/solarized-current-theme 'light)
      (progn
        (load-theme 'solarized-dark t)
        (setq my/solarized-current-theme 'dark))
    (progn
      (load-theme 'solarized-light t)
      (setq my/solarized-current-theme 'light))))

;; Set default font
(defvar my/font-list '("JetBrains Mono" "Menlo" "Monaco" "Courier New" "monospace")
  "Preferred fonts in order of preference.")

(defun my/set-preferred-font ()
  "Set the first available font from `my/font-list` as default."
  (catch 'done
    (dolist (font my/font-list)
      (when (member font (font-family-list))
        (set-face-attribute 'default nil :family font :height 120)
        (throw 'done font)))))

(my/set-preferred-font)

(defun my/global-font-size (delta)
  "Change the global font size by DELTA."
  (let* ((current-height (face-attribute 'default :height))
         (new-height (+ current-height delta)))
    (set-face-attribute 'default nil :height new-height)))

;;; ============================================================================
;;; CORE PACKAGES
;;; ============================================================================

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package ansi-color
  :straight t
  :config
  (defun my/colorize-compilation-buffer ()
    "Apply ANSI color codes in compilation buffer."
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

;;; ============================================================================
;;; TREE-SITTER SETUP
;;; ============================================================================

;; Configure tree-sitter grammar sources
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

;; Install tree-sitter grammars automatically
(use-package treesit-auto
  :straight t
  :config
  (setq treesit-auto-install t)  ; Changed from 'prompt to t
  (global-treesit-auto-mode)

  ;; Don't block on grammar installation
  (setq treesit-auto-recipe-list
        (cl-remove-if-not
         (lambda (recipe) (treesit-language-available-p (treesit-auto-recipe-lang recipe)))
         treesit-auto-recipe-list)))

; Helper function to install missing grammars
(defun my/install-treesit-grammars ()
  "Install all configured tree-sitter grammars."
  (interactive)
  (dolist (lang '(typescript tsx rust go json yaml))
    (unless (treesit-language-available-p lang)
      (message "Installing tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang))))

;; Install grammars on first run if missing
(when (not (treesit-language-available-p 'typescript))
  (my/install-treesit-grammars))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;;; ============================================================================
;;; EVIL MODE
;;; ============================================================================

(setq evil-want-keybinding nil)

(use-package evil
  :straight t
  :init
  ;; Ensure insert state has no normal mode keybindings
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  
  ;; Explicitly clear any unwanted bindings in insert state
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-j") nil))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader
  :straight t
  :after evil
  :init
  (setq evil-leader/in-all-states nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Enable evil-leader in special modes
(add-hook 'messages-buffer-mode-hook
          (lambda ()
            (evil-local-mode 1)
            (evil-normal-state)))

;; Ensure insert state works correctly in text modes
(defun my/ensure-proper-evil-insert-state ()
  "Ensure insert state doesn't have normal mode keybindings."
  (when (eq evil-state 'insert)
    (setq-local evil-insert-state-map (make-sparse-keymap))))

(add-hook 'org-mode-hook
          (lambda ()
            ;; Ensure we start in normal state
            (evil-normal-state)))

(add-hook 'markdown-mode-hook
          (lambda ()
            ;; Ensure we start in normal state
            (evil-normal-state)))

;;; ============================================================================
;;; COMPLETION & NAVIGATION
;;; ============================================================================

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode)
  :config
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "<tab>") #'corfu-next)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "<return>") #'corfu-insert))

(use-package eldoc-box
  :straight t
  :after eldoc)

(add-hook 'prog-mode-hook 'eldoc-mode)

(use-package helm
  :straight t
  :after bookmark
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  :config
  (helm-mode 1)
  
  ;; Fix helm file extension colors to match nord theme
  (custom-set-faces
   '(helm-ff-file-extension ((t (:foreground "#88C0D0" :weight normal))))
   '(helm-grep-file ((t (:foreground "#88C0D0" :underline nil))))
   '(helm-match ((t (:foreground "#EBCB8B" :weight bold)))))  ; Nord yellow for matches
  )

(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (setq helm-ag-show-status-function (lambda () ""))
  (setq projectile-globally-ignored-directories '("node_modules" "dist" ".git"))
  (setq projectile-generic-command
        "rg --files --hidden --follow --glob '!.git/*' --glob '!node_modules/' --glob '!.direnv' --glob '!cov/'")
  
  ;; Fix rg file extension colors in projectile
  (custom-set-faces
   '(helm-grep-file ((t (:foreground "#88C0D0" :underline nil))))
   '(compilation-info ((t (:foreground "#88C0D0" :weight normal))))))

(use-package helm-rg
  :straight t
  :after (helm projectile)
  :config
  (setq helm-rg-default-directory 'git-root)
  (add-hook 'helm-rg-mode-hook
            (lambda ()
              (setq-local header-line-format nil))))

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/workspace/")))

(straight-use-package 'rg)

;;; ============================================================================
;;; FILE MANAGEMENT
;;; ============================================================================

(use-package dired-sidebar
  :ensure t
  :init
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-sidebar-close-sidebar-on-file-open t
        dired-sidebar-sort-order 'folders-first)
  :config
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local mode-line-format nil)
              (setq-local truncate-lines t)
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

;;; ============================================================================
;;; LANGUAGE SERVER PROTOCOL (LSP)
;;; ============================================================================

(setq eglot-connect-timeout 120)

(use-package eglot
  :straight t
  :commands eglot-ensure
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (setq jsonrpc-event-hook nil
        eglot-events-buffer-size 0
        eglot-sync-connect nil)

  ;; Server configurations
  (add-to-list 'eglot-server-programs
               `((typescript-ts-mode tsx-ts-mode) . ("/opt/homebrew/bin/deno" "lsp")))
  
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode rustic-mode)
                 . ("rustup" "run" "stable" "rust-analyzer")))
  
  (add-to-list 'eglot-server-programs
               `(go-mode . ("gopls")))
  
  ;; Rust-analyzer configuration
  (defun my/eglot-rust-setup ()
    (setq-local eglot-workspace-configuration
                '(:rust-analyzer (:check (:command "check")))))
  (add-hook 'rust-mode-hook #'my/eglot-rust-setup)
  
  ;; Deno configuration
  (defun my/eglot-deno-setup ()
    (let ((config (or (locate-dominating-file default-directory "deno.json")
                      (locate-dominating-file default-directory "deno.jsonc"))))
      (when config
        (setq-local eglot-workspace-configuration
                    `(:deno (:enable t
                             :lint t
                             :config ,(expand-file-name "deno.json" config))))
        (when (fboundp 'projectile-reset-project-root)
          (projectile-reset-project-root config)))))
  
  (add-hook 'typescript-ts-mode-hook #'my/eglot-deno-setup))

(defun my/eglot-format-buffer-if-active ()
  "Format buffer with Eglot if it is active."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-format-buffer))
    (eglot-format-buffer)))

;;; ============================================================================
;;; PROGRAMMING LANGUAGE MODES
;;; ============================================================================

;; TypeScript / Deno - SINGLE DEFINITION
(use-package typescript-ts-mode
  :ensure nil  ; Built-in
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :init
  (setq typescript-ts-mode-indent-offset 4))

;; Go
(use-package go-mode
  :straight t
  :ensure t 
  :hook ((go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :init
  (setq gofmt-command "goimports"))

;; Rust
(use-package rustic
  :straight t
  :hook (rustic-mode . eglot-ensure)
  :config
  (setq rust-ts-mode-indent-offset 4
        rust-indent-offset 4
        rustic-lsp-client 'eglot
        rustic-format-on-save t))

(use-package ess
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))  ; Changed from R-mode to ess-r-mode
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . ess-r-mode))  ; Changed from R-mode
  (require 'ess-r-mode)
  :config
  (setq ess-ask-for-ess-directory nil
        ess-indent-with-fancy-comments nil
        comment-indent-function #'identity
        org-babel-R-command "R --no-save --no-restore"
        org-babel-R-evaluate-session t)

  ;; Disable line numbers in R console
  (add-hook 'inferior-ess-r-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))


(require 'ess)

(defun my/send-line-or-region-to-ess-r ()
  "Send the active region to ESS R if any, otherwise send the current line and step."
  (interactive)
  (if (use-region-p)
      (ess-eval-region (region-beginning) (region-end) nil 'buffer)
    (ess-eval-function-or-paragraph-and-step)))

(evil-leader/set-key "rr" 'my/send-line-or-region-to-ess-r)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (setq json-reformat:indent-width 4
        js-indent-level 4))

(use-package yaml-mode
  :straight t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

;; Dockerfile
(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;; ============================================================================
;;; ORG MODE
;;; ============================================================================

(use-package org
  :straight t
  :defer t
  :init
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (shell . t)
     (ruby . t)))

  ;; Hooks
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (local-set-key (kbd "<tab>") 'org-cycle)
              (local-set-key (kbd "TAB") 'org-cycle)))

  ;; Settings
  (setq org-startup-folded t
        org-src-preserve-indentation t
        org-startup-with-inline-images t
        org-image-actual-width nil)

  :config
  ;; TODO keywords
  (setq org-todo-keyword-faces
        '(("IN-PROGRESS" . "orange")
          ("TODO" . "dodger blue")
          ("DONE" . "systemGrayColor")
          ("NOTES" . "coral")
          ("1-1" . "systemRedColor")))

  (setq org-todo-keywords
        '((sequence "IN-PROGRESS" "TODO" "DONE" "NOTES" "1-1"))))

;;; ============================================================================
;;; SPELL CHECKING
;;; ============================================================================

(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell"
        ispell-dictionary "en_US"
        ispell-local-dictionary-alist
        '(("en_US" 
           "[A-Za-z]" "[^A-Za-z]" 
           "[']" nil ("-d" "en_US") nil utf-8))))

(use-package flyspell
  :ensure nil
  :hook (org-mode . flyspell-mode))

;;; ============================================================================
;;; VERSION CONTROL (GIT/MAGIT)
;;; ============================================================================

(use-package magit
  :straight t
  :commands (magit-status magit-blame-addition magit-log-buffer-file)
  :config
  (setq magit-git-executable "/opt/homebrew/bin/git"
        magit-diff-refine-hunk t
        magit-list-refs-sortby "-committerdate"
        magit-refresh-verbose nil
        magit-refresh-status-buffer nil
        magit-revision-insert-related-refs nil
        magit-section-initial-visibility-alist '((stashes . hide))
        )
  (with-eval-after-load 'magit
    (define-key magit-blame-read-only-mode-map (kbd "q") #'magit-blame-quit)))

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)  ; Auto-enable for programming modes
         (conf-mode . diff-hl-mode))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(defun my/github-link-to-line-or-region ()
  "Copy a GitHub URL to the current line or selected region using the current commit hash."
  (interactive)
  (require 'magit)
  (let* ((file (buffer-file-name))
         (root (magit-toplevel))
         (relative (file-relative-name file root))
         (start-line (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
         (end-line (if (use-region-p) (line-number-at-pos (region-end)) start-line))
         (commit (magit-rev-parse "HEAD"))
         (remote-url (magit-get "remote" "origin" "url"))
         (https-url (if (string-match "\\`git@github\\.com:\\(.*\\)\\.git\\'" remote-url)
                        (concat "https://github.com/" (match-string 1 remote-url))
                      (replace-regexp-in-string "\\.git\\'" "" remote-url)))
         (line-part (if (= start-line end-line)
                        (format "L%d" start-line)
                      (format "L%d-L%d" start-line end-line)))
         (link (format "%s/blob/%s/%s#%s" https-url commit relative line-part)))
    (kill-new link)
    (message "Copied GitHub link: %s" link)))

(defun my/magit-review-pr (branch)
  "Checkout BRANCH and soft reset to merge base with origin/main for PR review.
Supports both local and remote branches."
  (interactive 
   (list (magit-read-branch-or-commit "Branch to review")))
  (let ((base-branch "origin/main"))
    (message "Fetching from origin...")
    (magit-git-fetch "origin" nil)
    (message "Checking out %s..." branch)
    (magit-checkout branch)
    (let ((merge-base (string-trim 
                       (magit-git-string "merge-base" base-branch "HEAD"))))
      (unless merge-base
        (user-error "Could not find merge base between %s and HEAD" base-branch))
      (magit-reset-soft merge-base)
      (message "Ready to review: %s (all changes staged)" branch)
      (magit-status))))

;;; ============================================================================
;;; FEED READER
;;; ============================================================================

(use-package elfeed
  :straight t
  :commands (elfeed)
  :config
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" hackernews)
          ("https://lobste.rs/rss" lobsters)
          ("https://lwn.net/headlines/rss" lwn)
          ("https://v8.dev/blog.atom" v8)
          ("https://research.swtch.com/feed.atom" russ-cox)
          ("https://drewdevault.com/blog/index.xml" devault)
          ("https://danluu.com/atom.xml" danluu)))
  
  ;; Define a custom sort function for Elfeed
  (defun elfeed-sort-entries-oldest-first (a b)
    (time-less-p (elfeed-entry-date a) (elfeed-entry-date b)))

  ;; Set the custom sort function
  (setq elfeed-search-sort-function #'elfeed-sort-entries-oldest-first))

;;; ============================================================================
;;; LLM INTEGRATION (GPTEL)
;;; ============================================================================

(use-package agent-shell
  :ensure t)

(defun my/agent-shell-send-region (start end)
  "Send the highlighted region to the agent-shell buffer with file context.
Prompts for additional instructions to append. The region is sent with
the filename and line numbers for context."
  (interactive "r")
  (let* ((project-name (projectile-project-name))
         (buffer-name (format "Cursor Agent @ %s" project-name))
         (agent-buffer (get-buffer buffer-name))
         (file-name (or (buffer-file-name) (buffer-name)))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (region-text (buffer-substring-no-properties start end))
         (user-prompt (read-string "Prompt: "))
         (full-message (format "File: %s\nLines: %d-%d\n\n```\n%s\n```\n\n%s"
                               file-name
                               start-line
                               end-line
                               region-text
                               user-prompt)))
    ;; Create agent-shell if it doesn't exist
    (unless agent-buffer
      (save-window-excursion
        (agent-shell-cursor-start-agent))
      (setq agent-buffer (get-buffer buffer-name))
      (unless agent-buffer
        (error "Failed to create agent shell buffer '%s'" buffer-name)))
    (with-current-buffer agent-buffer
      (shell-maker-submit :input full-message))
    (message "Sent to %s" buffer-name)))

(defun my/set-gptel-system-prompt ()
  "Set GPTel system prompt based on major mode."
  (setq gptel-system-prompt
        (cond
         ((derived-mode-p 'org-mode 'markdown-mode)
          "If you are working with Markdown or Org-mode content, ensure that your responses focus on documentation and formatting. When analyzing the writing, keep in mind the elements of style: be concise, clear, and avoid unnecessary words."
          (derived-mode-p 'prog-mode)
          "Your are an expert typescript and rust developer. Provide technical and coding-related assistance. If asked to provide code of any kind, output only the code with no code fences or markdown.")
         (t
          "Provide general assistance.")))) ; Default prompt

(add-hook 'gptel-mode-hook #'my/set-gptel-system-prompt)
(add-hook 'org-mode-hook #'my/set-gptel-system-prompt)
(add-hook 'markdown-mode-hook #'my/set-gptel-system-prompt)

(defun my/read-openai-api-key ()
  "Read OpenAI API key from file."
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/openai.txt")
    (string-trim (buffer-string))))

(use-package gptel
  :straight t
  :after (evil transient)
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq gptel-api-key (my/read-openai-api-key) 
        gptel-model 'gpt-4o
        gptel-default-mode 'org-mode
        gptel-playback t))

(defun my/gptel-send-with-prompt ()
  "Send the current region or buffer to GPTel with an additional user prompt."
  (interactive)
  (let* ((user-prompt (read-string "Prompt: "))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-string)))
         (full-content (concat user-prompt "\n\n" content)))
    (let ((gptel-buffer-text full-content))
      (gptel-send))))

(defun my/gptel-ephemeral-popup-once ()
  "Send region or buffer text to GPTel once and show response in a markdown popup."
  (interactive)
  (let* ((user-prompt (read-string "Prompt: "))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-string)))
         (full-content (concat user-prompt "\n\n" content))
         (buf (get-buffer-create "*GPTel Output*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    
    (display-buffer buf '(display-buffer-pop-up-window))

    (with-current-buffer buf
      (markdown-mode)
      (when (bound-and-true-p flyspell-mode)
        (flyspell-mode -1))
      (local-set-key (kbd "q") #'quit-window))

    (gptel-request
        full-content
      :buffer buf
      :callback (lambda (response _info)
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert response)
                      (goto-char (point-min))
                      (setq fill-column 80)
                      (fill-region (point-min) (point-max))))))

    (with-current-buffer buf
      (local-set-key (kbd "q") #'quit-window))))

(defun my/gptel-ephemeral-popup-with-context ()
  "Send region/line with full buffer context to GPTel and show response in a markdown popup.
Format: full buffer, separator text, highlighted/line section, user prompt."
  (interactive)
  (let* ((user-prompt (read-string "Extra prompt: "))
         (full-buffer (buffer-string))
         (selected-content (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (thing-at-point 'line t)))
         (full-content (concat "### CONTEXT\n\n"
                               full-buffer
                               "\n\n### QUESTION\n\n"
                               selected-content
                               "\n\n"
                               user-prompt))
         (buf (get-buffer-create "*GPTel Output*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    
    (display-buffer buf '(display-buffer-pop-up-window))

    (with-current-buffer buf
      (markdown-mode)
      (when (bound-and-true-p flyspell-mode)
        (flyspell-mode -1))
      (local-set-key (kbd "q") #'quit-window))

    (gptel-request
        full-content
      :buffer buf
      :callback (lambda (response _info)
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert response)
                      (goto-char (point-min))
                      (setq fill-column 80)
                      (fill-region (point-min) (point-max))))))

    (with-current-buffer buf
      (local-set-key (kbd "q") #'quit-window))))

(defun my/gptel-auto-format-code (&rest _args)
  "Auto-fill paragraphs inside code blocks in a gptel chat buffer."
  (when (derived-mode-p 'gptel-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^```\\([^ \n]*\\)?" nil t)
        (let ((block-start (match-end 0)))
          (when (re-search-forward "^```" nil t)
            (let ((block-end (match-beginning 0)))
              (save-restriction
                (narrow-to-region block-start block-end)
                (goto-char (point-min))
                (while (not (eobp))
                  (fill-paragraph)
                  (forward-paragraph))))))))))

(add-hook 'gptel-post-response-hook #'my/gptel-auto-format-code)

(defun my/gptel-rewrite-with-format ()
  "Rewrite the region with gptel, auto-format it, and accept the output."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((buf (current-buffer))
         (beg (region-beginning))
         (end (region-end))
         (should-format (derived-mode-p 'org-mode 'markdown-mode 'markdown-ts-mode)))

    (defun my/format-and-accept-once ()
      "Format and accept the rewritten content."
      (when should-format
        (with-current-buffer buf
          (save-excursion
            (goto-char beg)
            (let ((fill-column 80))
              (dotimes (_ 20)
                (when (< (point) (point-max))
                  (fill-paragraph)
                  (forward-paragraph)))))))
      ;; Automatically accept the changes
      (with-current-buffer buf
        (goto-char beg)
        (when (looking-at "<<<<<<< original")
          (let ((end-of-conflict (search-forward ">>>>>>>")))
            (delete-region (line-beginning-position) end-of-conflict))))
      (remove-hook 'gptel-post-stream-hook #'my/format-and-accept-once))

    (add-hook 'gptel-post-stream-hook #'my/format-and-accept-once)

    (let ((current-prefix-arg 1))
      (call-interactively #'gptel-rewrite))))

;;; ============================================================================
;;; CUSTOM HELPER FUNCTIONS
;;; ============================================================================

(defun my/open-workspace-project ()
  "Select a folder under ~/workspace and open it."
  (interactive)
  (let ((projects (directory-files "~/workspace" t "^[^.].*")))
    (helm :sources (helm-build-sync-source "Workspace Projects"
                     :candidates projects
                     :fuzzy-match t
                     :action (lambda (dir) (dired dir)))
          :buffer "*helm workspace projects*")))

(defun my/helm-projectile-open-files ()
  "Use Helm to show all open files in the current Projectile project."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (helm :sources
          (helm-build-sync-source "Open Project Files"
            :candidates (lambda ()
                          (cl-remove-if-not
                           (lambda (buffer)
                             (with-current-buffer buffer
                               (and buffer-file-name
                                    (string-prefix-p project-root (file-truename buffer-file-name)))))
                           (buffer-list)))
            :action '(("Switch to buffer" . switch-to-buffer)))
          :buffer "*helm project open files*")))

(defun my/toggle-visible-windows ()
  "Move cursor to the next visible window, including side windows."
  (interactive)
  (let ((next (next-window (selected-window) nil 'visible)))
    (select-window next)))

(defun my/deno-test-at-point ()
  "Run the Deno test under the cursor in the compile buffer."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         test-name)
    (save-excursion
      (when (re-search-backward "Deno\\.test\\([\"']\\)\\([^\"']+\\)\\1" nil t)
        (setq test-name (match-string 2))))
    (let ((command (if test-name
                       (format "/opt/homebrew/bin/deno test --filter \"%s\"" test-name)
                     "/opt/homebrew/bin/deno test")))
      (compile command))))

;;; ============================================================================
;;; KEYBINDINGS
;;; ============================================================================

(defun my/projectile-run-eshell-split ()
  "Run `projectile-run-eshell' in a split window."
  (interactive)
  ;; Select a window splitting function as per your preference, e.g., horizontal split
  (split-window-below) 
  ;; Switch to the newly created window
  (other-window 1) 
  ;; Run projectile-run-eshell
  (projectile-run-eshell))


;; Mode-specific format-on-save and Evil Leader bindings
(dolist (mode '(rustic-mode rust-mode rust-ts-mode go-mode go-ts-mode 
                typescript-ts-mode emacs-lisp-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda ()
              (add-hook 'before-save-hook
                        #'my/eglot-format-buffer-if-active
                        nil t)))
  
  (evil-leader/set-key-for-mode mode
    "gg" 'xref-find-definitions
    "gl" 'xref-find-references
    "gp" 'pop-tag-mark
    "gr" 'eglot-rename
    "t"  'eldoc-box-help-at-point
    "gu" 'eglot-reconnect))

;; Org-mode keybindings
(evil-leader/set-key-for-mode 'org-mode
  "rr" (lambda ()
         (interactive)
         (let ((current-prefix-arg nil))
           (org-babel-execute-src-block t)))
  "u" 'org-todo
  "S" (lambda ()
        (interactive)
        (org-sort-entries nil ?o))
  "i" (lambda ()
        (interactive)
        (insert "#+BEGIN_SRC R :session :results none\n\n#+END_SRC\n")
        (forward-line 1)))

;; Global leader key bindings
(evil-leader/set-key
  ;; Config
  "co" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "cl" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))

  ;; Spell check
  "ss" 'ispell
  "sr" 'ispell-region

  ;; Project
  "vu" 'projectile-compile-project
  "vv" 'my/projectile-run-eshell-split
  "mp" 'my/open-workspace-project
  "mt" 'my/deno-test-at-point

  ;; Buffer management
  "nk" (lambda () (interactive) 
         (mapc 'kill-buffer (buffer-list)) 
         (switch-to-buffer "*scratch*"))
  "nn" 'next-buffer
  "np" 'previous-buffer
  "no" 'delete-other-windows
  "nd" 'kill-buffer-and-window
  "nb" 'helm-mini
  "nr" (lambda () (interactive) (switch-to-buffer "*scratch*"))
  "na" (lambda () (interactive) (find-file "~/workspace/org/notes.org"))
  "nm" (lambda () (interactive) (helm-find-files-1 "~/workspace/org/"))
  "nf" 'helm-projectile-find-file
  "ng" 'helm-projectile-rg
  "nw" 'my/toggle-visible-windows
  "nt" (lambda () (interactive) (dired-sidebar-toggle-sidebar))
  "nl" 'helm-buffers-list

  ;; Window splits
  "_" 'split-window-vertically
  "|" 'split-window-horizontally

  ;; Scrolling
  "j" 'evil-scroll-down
  "k" 'evil-scroll-up

  ;; GPTel / Magit / Git
  "mv" 'my/gptel-send-with-prompt
  "ma" 'my/gptel-ephemeral-popup-once
  "mr" 'my/gptel-rewrite-with-format
  "mg" 'my/agent-shell-send-region
  "ms" 'magit
  "mb" 'magit-blame-addition
  "mf" 'magit-log-buffer-file
  "mm" 'my/github-link-to-line-or-region
  "ml" 'elfeed
  "md" 'diff-hl-mode
  "me" (lambda () (interactive) (eww-browse-url (read-string "url: ")))
  "mu" 'my/magit-review-pr
  "mt" 'my/toggle-solarized-theme

  ;; Font size
  "=" (lambda () (interactive) (my/global-font-size 20))
  "-" (lambda () (interactive) (my/global-font-size -20)))

;; Enable evil-leader in various modes
(dolist (mode-hook '(rust-ts-mode-hook rustic-mode-hook typescript-ts-mode-hook
                     emacs-lisp-mode-hook org-mode-hook go-ts-mode-hook
                     go-mode-hook yaml-ts-mode json-ts-mode special-mode-hook))
  (add-hook mode-hook #'evil-leader-mode))

(setq evil-disable-insert-state-bindings t)
(evil-leader-mode t)

;;; ============================================================================
;;; TRANSIENT (Required by Magit)
;;; ============================================================================

(use-package transient
  :straight t)

;;; init.el ends here
