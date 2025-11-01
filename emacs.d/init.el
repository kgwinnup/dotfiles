;; -*- lexical-binding: t; -*-

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(setq org-src-block-faces nil)

;; Ensure Homebrew GCC + libgccjit are visible to native compiler
(setenv "PATH" (concat "/opt/homebrew/opt/gcc/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(let ((homebrew-bin "/opt/homebrew/bin"))
  (setenv "PATH" (concat homebrew-bin ":" (getenv "PATH")))
  (setq exec-path (cons homebrew-bin exec-path)))
(setenv "LIBRARY_PATH"
        (concat "/opt/homebrew/opt/libgccjit/lib/gcc/current:"
                (getenv "LIBRARY_PATH")))
(setenv "DYLD_LIBRARY_PATH"
        (concat "/opt/homebrew/opt/libgccjit/lib/gcc/current:"
                (getenv "DYLD_LIBRARY_PATH")))

(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

;;; ----------------------------
;;; straight.el bootstrap
;;; ----------------------------
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

;;; ----------------------------
;;; Global defaults
;;; ----------------------------
(setq-default ring-bell-function 'ignore
              require-final-newline t
              compilation-scroll-output t
              scroll-step 1
              gc-cons-threshold (* 384 1024 1024)
              gc-cons-percentage 0.6
              read-process-output-max (* 3 1024 1024)
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
              shell-file-name "bash"
              initial-major-mode 'org-mode
              compilation-environment '("TERM=xterm-256color")
              backup-directory-alist '(("" . "~/.emacs.d/backup"))
              auto-mode-case-fold nil
              fill-column 80)


;;; ----------------------------
;;; Packages
;;; ----------------------------
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(straight-use-package 'rg)

(use-package vterm
  :straight t
  :commands (vterm))

;;; ----------------------------
;;; Enable ANSI colors in compile buffer
;;; ----------------------------
(use-package ansi-color
  :straight t
  :config
  ;; Filter ANSI escape codes in compilation buffer
  (defun my/colorize-compilation-buffer ()
    "Apply ANSI color codes in compilation buffer."
    (ansi-color-apply-on-region compilation-filter-start (point)))
  
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

;; Spell checking
(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell"))

(setq ispell-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" 
         "[A-Za-z]" "[^A-Za-z]" 
         "[']" nil ("-d" "en_US") nil utf-8)))

;; On-the-fly spell check
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; ----------------------------
;;; TypeScript / Deno support
;;; ----------------------------

;; typescript-mode
(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-mode)
  :mode ("\\.tsx\\'" . typescript-mode)  ;; Ensure tsx also uses typescript-mode
  :hook (typescript-mode . eglot-ensure))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)        ;; Cycle through candidates
  (corfu-auto t)         ;; Show popup automatically
  (corfu-auto-delay 0.1)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  :init
  (global-corfu-mode))

;; Optional: keybindings for cycling with Tab/Shift-Tab
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "<tab>") #'corfu-next)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "<return>") #'corfu-insert))


(add-hook 'prog-mode-hook 'eldoc-mode)
;; Optional: use eldoc-box for better popup
(use-package eldoc-box
  :straight t
  :after eldoc)

;; If you previously enabled global eldoc-box mode:
;; (eldoc-box-enable) → comment out or remove

(defun my/eglot-format-buffer-on-save ()
  "Add `eglot-format-buffer` to `before-save-hook` in the current buffer."
  (add-hook 'before-save-hook
            (lambda ()
              (when (bound-and-true-p eglot--managed-mode)
                (eglot-format-buffer)))
            nil t)) ;; local hook


(use-package rustic
  :straight t
  :hook (rustic-mode . eglot-ensure)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t))

(setq eglot-connect-timeout 120)

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 4)
  (setq js-indent-level 4))

(use-package yaml-mode
  :straight t
  :mode (("\\.ya?ml\\'" . yaml-mode)) ;; matches .yaml and .yml
  )

;; Install and configure dockerfile-mode
(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Eglot LSP setup
(use-package eglot
  :straight t
  :commands eglot-ensure
  :config
  ;; Tell Eglot to use Deno LSP for TypeScript
  (add-to-list 'eglot-server-programs
               `(typescript-mode . ("/opt/homebrew/bin/deno" "lsp")))
  
  ;; Tell Eglot to use rust-analyzer for Rust
  (add-to-list 'eglot-server-programs
               `(rust-mode . ("rust-analyzer")))

  ;; Configure Deno only when deno.json/deno.jsonc is found
  (defun my/eglot-deno-setup ()
    (let ((config (or (locate-dominating-file default-directory "deno.json")
                      (locate-dominating-file default-directory "deno.jsonc"))))
      (when config
        (setq-local eglot-workspace-configuration
                    `(:deno (:enable t
                                     :lint t
                                     :config ,(expand-file-name
                                               "deno.json"
                                               config))))
        (when (fboundp 'projectile-reset-project-root)
          (projectile-reset-project-root config)))))

  ;; Apply setup + format hook in TS buffers
  (add-hook 'typescript-mode-hook
            (lambda ()
              (my/eglot-deno-setup)
              (add-hook 'before-save-hook #'my/eglot-format-on-save nil t))))

(use-package ess
  :straight t
  :init
  ;; Associate .R and .Rmd files with ESS R mode
  (add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . R-mode))
  :config
  (setq ess-ask-for-ess-directory nil)) ;; optional: don't ask for working dir

;; Markdown syntax highlighting
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc") ;; optional, for preview/export
  :config
  ;; Enable syntax highlighting for code blocks
  (setq markdown-fontify-code-blocks-natively t))

;; Tell Evil we don’t want its default keybindings for other packages
(setq evil-want-keybinding nil)

;; Evil
(use-package evil
  :straight t
  :config
  ;; Make SPC a leader key in Evil
  (setq evil-leader/leader "SPC")
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; Transient
(use-package transient
  :straight t)

;; Keybinding for "mp" to select a folder under ~/workspace
(defun my/open-workspace-project ()
  "Select a folder under ~/workspace and open it."
  (interactive)
  (let ((projects (directory-files "~/workspace" t "^[^.].*"))) ;; skip . and ..
    (helm :sources (helm-build-sync-source "Workspace Projects"
                                           :candidates projects
                                           :fuzzy-match t
                                           :action (lambda (dir)
                                                     (dired dir)))
          :buffer "*helm workspace projects*")))

;; Helm
(use-package helm
  :straight t
  :after bookmark
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  :config
  (helm-mode 1))

;; ------------------------------
;; Org-mode setup
;; ------------------------------
(use-package org
  :straight t
  :defer t
  :init
  ;; Enable Babel languages (add Mermaid)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (shell . t)
     (ruby . t)))  ;; enable Mermaid blocks

  ;; Redisplay inline images after executing code blocks
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; Org startup settings
  (setq org-startup-folded t
        org-src-preserve-indentation t
        org-startup-with-inline-images t
        org-image-actual-width nil)

  ;; Org-mode hooks
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              (local-set-key (kbd "<tab>") 'org-cycle)
              (local-set-key (kbd "TAB") 'org-cycle)))

  :config
  ;; Custom faces for TODO keywords
  (setq org-todo-keyword-faces
        '(("IN-PROGRESS" . "orange")
          ("TODO" . "dodger blue")
          ("DONE" . "systemGrayColor")
          ("NOTES" . "coral")
          ("1-1" . "systemRedColor")))

  (setq org-todo-keywords
        '((sequence "IN-PROGRESS" "TODO" "DONE" "NOTES" "1-1"))))

;; Ensure projectile is installed
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  ;; Set the projects root directory
  (setq projectile-project-search-path '("~/workspace/")))

;;; ----------------------------
;;; Use ripgrep for Projectile (with Helm)
;;; ----------------------------
(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :config
  (helm-projectile-on)

  ;; Make Projectile use ripgrep for finding files in projects
  (setq projectile-generic-command
        "rg --files --hidden --glob '!.git/*'"))


(defun my/helm-projectile-ripgrep ()
  "Use ripgrep within the current projectile project with Helm."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (helm :sources (helm-build-in-buffer-source "Ripgrep Project Search"
                                                :data (split-string
                                                       (shell-command-to-string
                                                        "rg --files --hidden --glob '!.git/*'")
                                                       "\n" t)
                                                :fuzzy-match t
                                                :action (lambda (file) (find-file file)))
          :buffer "*helm ripgrep project*")))

(use-package magit
  :straight t
  :commands (magit-status magit-blame-addition magit-log-buffer-file)
  :config
  ;; Optional: show full diffs by default
  (setq magit-diff-refine-hunk t)
  (with-eval-after-load 'magit
    (define-key magit-blame-read-only-mode-map (kbd "q") #'magit-blame-quit)))

(setq browse-url-browser-function 'eww-browse-url)
;; eww-mode browser keybinding
(setq shr-use-fonts nil
      shr-use-fonts nil
      shr-use-colors nil
      shr-external-rendering-functions nil
      shr-indentation 2
      eww-search-prefix "https://ddg.gg/html?q="
      shr-width 80)

(use-package elfeed
  :straight t
  :commands (elfeed)
  :config
  ;; Optional: set your feed URLs
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" hackernews)
          ("https://lobste.rs/rss" lobsters)
          ("https://lwn.net/headlines/rss" lwn)
          ("https://mcyoung.xyz/feed" mcyoung)
          ("https://drewdevault.com/blog/index.xml" devault)
          ("https://danluu.com/atom.xml" danluu))))

(defun my/gptel-send-with-prompt ()
  "Send the current region or buffer to GPTel, with an additional user prompt."
  (interactive)
  (let* ((user-prompt (read-string "Extra prompt: "))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-string)))
         (full-content (concat user-prompt "\n\n" content)))
    ;; Temporarily override gptel input with combined content
    (let ((gptel-buffer-text full-content))
      (gptel-send))))

(defun my/gptel-ephemeral-popup-once ()
  "Send region or buffer text to GPTel once and show response in a markdown popup."
  (interactive)
  (let* ((user-prompt (read-string "Extra prompt: "))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-string)))
         (full-content (concat user-prompt "\n\n" content))
         (buf (get-buffer-create "*GPTel Output*")))
    
    ;; Prepare buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    
    ;; Display first so tokens stream live
    (display-buffer buf '(display-buffer-pop-up-window))

    ;; Enable markdown + disable spellcheck + q to close
    (with-current-buffer buf
      (markdown-mode)
      (when (bound-and-true-p flyspell-mode)
        (flyspell-mode -1))
      (local-set-key (kbd "q") #'quit-window))

    ;; GPTel one-shot request
    (gptel-request
        full-content
      :buffer buf
      :callback (lambda (response _info)
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert response)
                      (goto-char (point-min))
                      ;; Reformat line widths automatically
                      (setq fill-column 80)
                      (fill-region (point-min) (point-max))))))

    ;; Make q close
    (with-current-buffer buf
      (local-set-key (kbd "q") #'quit-window))))

(defun my/gptel-rewrite-with-format ()
  "Rewrite the region with gptel, then auto-format it in org/markdown modes.
This installs a one-shot hook that formats the rewritten text after gptel
finishes and the replacement is accepted."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((beg (region-beginning))
         (end (region-end))
         ;; markers survive buffer edits; the second arg t makes end-marker
         ;; advance when text is inserted at its position.
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end t))
         (should-format (derived-mode-p 'org-mode 'markdown-mode))
         ;; one-shot hook: runs with two args (response-beg response-end)
         (formatter
          (lambda (_resp-beg _resp-end)
            (when should-format
              ;; run formatting in the original buffer where markers live
              (with-current-buffer (current-buffer)
                (save-excursion
                  (goto-char beg-marker)
                  (while (< (point) end-marker)
                    (fill-paragraph)
                    (forward-paragraph))))))))
    ;; add the one-shot hook
    (add-hook 'gptel-post-rewrite-functions formatter)
    (unwind-protect
        ;; call interactively to avoid passing the wrong arity/args
        (call-interactively #'gptel-rewrite)
      ;; ensure we always remove our hook even on error / user cancel
      (remove-hook 'gptel-post-rewrite-functions formatter))))


;(defun my/read-api-key ()
;  (with-temp-buffer
;    (insert-file-contents "~/.emacs.d/openai.txt")
;    (string-trim (buffer-string))))

;(use-package gptel
;  :straight t
;  :after (evil transient)
;  :config
;  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
;  (setq gptel-api-key (my/read-api-key) 
;        gptel-model 'gpt-4o
;        gptel-default-mode 'org-mode
;        gptel-playback t))

(defun my/gptel-auto-format-code (&rest _args)
  "Auto-fill paragraphs inside code blocks in a gptel chat buffer.
Runs after gptel inserts a response."
  (when (derived-mode-p 'gptel-mode)
    (save-excursion
      (goto-char (point-min))
      ;; find ```lang blocks
      (while (re-search-forward "^```\\([^ \n]*\\)?" nil t)
        (let ((block-start (match-end 0)))
          ;; find closing ```
          (when (re-search-forward "^```" nil t)
            (let ((block-end (match-beginning 0)))
              (save-restriction
                (narrow-to-region block-start block-end)
                (goto-char (point-min))
                ;; format paragraph by paragraph
                (while (not (eobp))
                  (fill-paragraph)
                  (forward-paragraph))))))))))

(defun my/gptel-auto-format-and-preview (&rest _args)
  "Auto-format code blocks and preview LaTeX fragments in a gptel chat buffer.
Runs after gptel inserts a response."
  (when (derived-mode-p 'gptel-mode)
    (save-excursion
      ;; -----------------------
      ;; 1️⃣ Format code blocks
      ;; -----------------------
      (goto-char (point-min))
      (while (re-search-forward "^```\\([^ \n]*\\)?" nil t)
        (let ((block-start (match-end 0)))
          ;; find closing ```
          (when (re-search-forward "^```" nil t)
            (let ((block-end (match-beginning 0)))
              (save-restriction
                (narrow-to-region block-start block-end)
                (goto-char (point-min))
                ;; format paragraph by paragraph
                (while (not (eobp))
                  (fill-paragraph)
                  (forward-paragraph))))))))
    (org-preview-latex-fragment)))

(add-hook 'gptel-post-response-hook #'my/gptel-auto-format-code)

(defun my/toggle-visible-windows ()
  "Move cursor to the next visible window."
  (interactive)
  (other-window 1))

;;; ----------------------------
;;; Run Deno test at point (anywhere in function)
;;; ----------------------------
(defun my/deno-test-at-point ()
  "Run the Deno test under the cursor in the compile buffer.
If the cursor is inside a Deno.test function, it runs that test; otherwise runs all tests."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (line (thing-at-point 'line t))
         test-name)
    ;; Search backwards for the nearest Deno.test line
    (save-excursion
      (when (re-search-backward "Deno\\.test\\([\"']\\)\\([^\"']+\\)\\1" nil t)
        (setq test-name (match-string 2))))
    ;; Build the command
    (let ((command (if test-name
                       (format "/opt/homebrew/bin/deno test --filter \"%s\"" test-name)
                     "/opt/homebrew/bin/deno test")))
      ;; Run in compile buffer
      (compile command))))

(defun my/eglot-format-buffer-if-active ()
  "Format buffer with Eglot if it is active."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-format-buffer))
    (eglot-format-buffer)))

(defun my/global-font-size (delta)
  "Change the global font size by DELTA."
  (let* ((current-height (face-attribute 'default :height))
         (new-height (+ current-height delta)))
    (set-face-attribute 'default nil :height new-height)))

(use-package solarized-theme
  :straight t
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  :config
  (load-theme 'solarized-dark t))

(defvar my/solarized-current-theme 'light
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
    (load-theme 'solarized-light t)
    (setq my/solarized-current-theme 'light)))

;;; ----------------------------
;;; Evil leader key setup
;;; ----------------------------
;; set these before setting the other keybindings
(use-package evil-leader
  :straight t
  :after evil
  :init
  ;; Enable global leader mode
  (global-evil-leader-mode)
  (evil-mode 1)

  (dolist (mode '(rustic-mode go-mode go-ts-mode typescript-mode emacs-lisp-mode))
    ;; Format buffer on save
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (add-hook 'before-save-hook
                          #'my/eglot-format-buffer-if-active
                          nil t)))

    ;; Evil leader keybindings
    (evil-leader/set-key-for-mode mode
      "gg" 'xref-find-definitions
      "gl" 'xref-find-references
      "gp" 'pop-tag-mark
      "gr" 'eglot-rename
      "gh" 'e;; Disable automatic eldoc-box
      "t"  'eldoc-box-help-at-point
      "gu" 'eglot-reconnect))

  
  ;; Set leader key to SPC
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key-for-mode 'elfeed-show-mode
    ;; n: add a note for the current entry
    "n" 'my/elfeed-add-note)

  (evil-leader/set-key-for-mode 'org-mode
    "rr" (lambda ()
           (interactive)
           (let ((current-prefix-arg nil)) ;; no prefix needed
             (org-babel-execute-src-block t)))

    "u" 'org-todo

    "S" (lambda ()
          (interactive)
          (org-sort-entries nil ?o))

    "i" (lambda ()
          (interactive)
          (let ((lang "R"))
            (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC\n" lang))
            (forward-line 1))))

  ;; Leader key bindings
  (evil-leader/set-key
    ;; Config files
    "co" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "cl" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))

    ;; Spell
    "ss" 'ispell
    "sr" 'ispell-region

    ;; CLI / project
    "vu" 'projectile-compile-project
    "mp" 'my/open-workspace-project
    "mt" 'my/deno-test-at-point

    ;; Buffer management
    "nk" (lambda () (interactive) (mapc 'kill-buffer (buffer-list)) (switch-to-buffer "*scratch*"))
    "nn" 'next-buffer
    "np" 'previous-buffer
    "no" 'delete-other-windows
    "nd" 'kill-buffer-and-window
    "nb" 'helm-mini
    "nr" (lambda () (interactive) (switch-to-buffer "*scratch*"))
    "na" (lambda () (interactive) (find-file "~/workspace/org/notes.org"))
    "nm" (lambda () (interactive) (helm-find-files-1 "~/workspace/org/"))
    "nf" 'my/helm-projectile-ripgrep
    "ng" 'projectile-ripgrep
    "nw" 'my/toggle-visible-windows
    "nt" 'treemacs

    ;; Window splits
    "_" 'split-window-vertically
    "|" 'split-window-horizontally

    ;; Scrolling
    "j" 'evil-scroll-down
    "k" 'evil-scroll-up

    ;; gptel / Magit / git
    "mg" 'gptel
    "mv" 'my/gptel-send-with-prompt
    "ma" 'my/gptel-ephemeral-popup-once
    "mr" 'my/gptel-rewrite-with-format
    "ms" 'magit
    "mb" 'magit-blame-addition
    "mf" 'magit-log-buffer-file
    "ml" 'elfeed
    "me" (lambda () (interactive) (eww-browse-url (read-string "url: ")))
    "mc" 'my/open-vterm-with-cursor-agent

    ;; Font / view
    "=" (lambda () (interactive) (my/global-font-size 20))
    "-" (lambda () (interactive) (my/global-font-size -20))
    "mt" 'my/toggle-solarized-theme
    ))

;;; ----------------------------
;;; Set default font with fallbacks
;;; ---------------------------
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
(evil-leader-mode t)
