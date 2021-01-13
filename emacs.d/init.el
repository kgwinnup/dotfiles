;;
;; Packages and General stuff
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


;; brew isntall w3m
;; git clone mu for latest build
(add-to-list 'load-path "~/.emacs.d/mu/mu4e")
(load-file "~/.emacs.d/email.el")
(require 'mu4e)
(setq mue4e-headers-skip-duplicates  t
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-compose-format-flowed nil
      mu4e-date-format "%y/%m/%d"
      mu4e-headers-date-format "%Y/%m/%d"
      mu4e-change-filenames-when-moving t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t
      smtpmail-queue-mail t
      smtpmail-queue-dir (expand-file-name "~/Maildir/queue/cur")
      smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'smtpmail-send-it
      mu4e-update-interval 300
      ;mu4e-html2text-command "/usr/bin/w3m -dump -T text/html"
      mu4e-compose-signature-auto-include nil
      mu4e-attachments-dir "~/Downloads"
      ;; top-level Maildir
      mu4e-maildir       "~/Maildir/gmail"
      ;; note that these folders below must start with /
      ;; the paths are relative to maildir root
      mu4e-refile-folder "/archive"
      mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash"
      ;; this setting allows to re-sync and re-index mail
      ;; by pressing U
      mu4e-get-mail-command  "mbsync -a"
      mu4e-bookmarks `(("maildir:/gmail/inbox" "Inbox" ?i)
                       ("flag:unread AND NOT flag:trashed AND NOT maildir:/queue" "Unread messages" ?u)
                       ("maildir:/queue AND from:*@gmail.com" "Outbox" ?o)
                       ("date:today..now" "Today's messages" ?t)
                       ("date:7d..now" "Last 7 days" ?w)))
(add-hook 'message-mode-hook 'turn-on-orgtbl)

(setq-default ring-bell-function 'ignore
              mac-allow-anti-aliasing nil
              scroll-step 1
              scroll-conservatively  10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse 't
              indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4
              initial-scratch-message nil
              inhibit-startup-screen t
              auto-save-default nil
              make-backup-files nil
              shr-width 80
              backup-directory-alist '(("" . "~/.emacs.d/backup"))
              default-directory "~/workspace/"
              custom-file "~/.emacs.d/custom.el")

(shell-command "touch ~/.emacs.d/custom.el")
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/go/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))
(setenv "GTAGSLIBPATH" "~/.gtags")
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(load custom-file)

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
  "toggles the shell window, this function also keeps the cursor in
the editor buffer when bringing the terminal back into the visible
frame"
  (interactive)
  (if (not (member "*shell*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (setq w1 (selected-window))
        (setq w2 (split-window-horizontally))
        (shell)
        (display-line-numbers-mode -1)
        (set-window-buffer w2 "*shell*")
        (select-window w1))
    (if (and (get-buffer-window "*shell*"))
        (delete-other-windows)
      (progn (let ((w1 (selected-window))
                   (w2 (split-window-horizontally)))
               (set-window-buffer w2 "*shell*")
               (selecte-window w1))))))

(defun my-clear-shell ()
  "clears the shell buffer"
  (interactive)
  (if (get-buffer-window "*shell*")
      (comint-clear-buffer)))

(defun my-send-to-shell (cmd)
  "sends a command to the buffer containing an active shell"
  (interactive)
  (let ((proc (get-process "shell"))
        (curbuf (current-buffer)))
    (setq command (concat cmd "\n"))
    (process-send-string proc command)
    (setq last-shell-cmd cmd)
    (switch-to-buffer "shell")
    (goto-char (point-max))
    (switch-to-buffer curbuf)))

(eval-after-load "comint"
  '(progn
      (setq comint-move-point-for-output 'others)))

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

(setq my-font-size 150)
(defun my-global-font-size (size)
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ size my-font-size))
  (setq my-font-size (+ size my-font-size)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t))

(use-package helm
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  :config
  (setq helm-boring-buffer-regexp-list
      (quote
       (  "\\Minibuf.+\\*"
          "\\` "
          "\\magit"
          "\\*.+\\*"))))

(use-package helm-gtags
  :ensure t
  :init
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'rust-mode-hook 'helm-gtags-mode)
  (add-hook 'helm-gtags-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC g g") 'helm-gtags-find-tag-from-here)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'helm-gtags-pop-stack)
              (define-key evil-normal-state-local-map (kbd "SPC g f") 'helm-gtags-select)
              (define-key evil-normal-state-local-map (kbd "SPC g l") 'helm-gtags-parse-file)
              (define-key evil-normal-state-local-map (kbd "SPC g u") 'helm-gtags-update-tags))))

(use-package yaml-mode
  :ensure t)

(use-package helm-themes
  :ensure t)

(use-package acme-theme
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

(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds '(("https://www.lobste.rs/rss" lobsters)
                       ("http://www.reddit.com/r/reverseengineering/.rss" reddit-re)
                       ("http://www.reddit.com/r/bsd/.rss" reddit-bsd)
                       ("http://www.reddit.com/r/emacs/.rss" reddit-emacs)
                       ("http://rss.slashdot.org/Slashdot/slashdotMain" slashdot)))
  (setq-default elfeed-search-filter "@2-days-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(use-package magit
  :ensure t
  :init
  (use-package evil-magit
    :ensure t))

(require 'ox-latex)
(use-package org
  :ensure t
  :init
  (use-package ox-gfm
    :ensure t)
  (use-package org-present
    :ensure t)
    (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode)
              ;(add-hook 'after-save-hook 'org-preview-latex-fragment)
              (define-key evil-normal-state-local-map (kbd "SPC r r") 'org-preview-latex-fragment)
              (define-key evil-normal-state-local-map (kbd "SPC E") 'org-gfm-export-to-markdown)
              (define-key evil-normal-state-local-map (kbd "SPC F") 'org-table-toggle-coordinate-overlays)
              (define-key evil-normal-state-local-map (kbd "SPC P") 'org-present)
              (define-key evil-normal-state-local-map (kbd "SPC R") 'my-org-refresh)
              (define-key evil-normal-state-local-map (kbd "SPC T") 'my-org-timestamp)
              (define-key evil-normal-state-local-map (kbd "SPC p") 'org-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC g p") 'org-global-cycle)
              (define-key evil-normal-state-local-map (kbd "SPC s n") 'my-start-code-block)
              (define-key evil-normal-state-local-map (kbd "SPC s o") 'org-edit-src-code)
              (define-key evil-normal-state-local-map (kbd "SPC u") 'org-todo)
              (define-key evil-normal-state-local-map (kbd "SPC o") 'org-toggle-checkbox)))
    :config
    (setq org-todo-keywords
          '((sequence "PROJECT" "TODO" "IN-PROGRESS" "BACKLOG" "|" "DONE")))
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
                     (org-present-read-write)))))
    '(org-preview-latex-process-alist
      (quote
       ((dvipng :programs ("lualatex" "dvipng")
                :description "dvi > png"
                :message "you need to install the programs: latex and dvipng."
                :image-input-type "dvi"
                :image-output-type "png"
                :image-size-adjust (1.0 . 1.0)
                :latex-compiler ("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
                :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
        (dvisvgm :programs ("latex" "dvisvgm")
                 :description "dvi > svg"
                 :message "you need to install the programs: latex and dvisvgm."
                 :use-xcolor t
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs ("latex" "convert")
                     :description "pdf > png"
                     :message "you need to install the programs: latex and imagemagick."
                     :use-xcolor t
                     :image-input-type "pdf"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))))
    (add-to-list 'org-latex-classes
                 '("koma-article2" "\\documentclass[times,11pt,letterpaper,twopage,parskip=half-,headings=small,booktabs,longtable,DIV=15]{scrartcl}
                    \\usepackage[utf8]{inputenc}
                    \\usepackage[T1]{fontenc}
                    \\usepackage[margin=1in]{geometry}
                    \\usepackage{longtable}
                    \\usepackage{wrapfig}
                    \\usepackage{rotating}
                    \\usepackage[normalem]{ulem}
                    \\usepackage{amsmath}
                    \\usepackage{textcomp}
                    \\usepackage{amssymb}
                    \\usepackage{capt-of}
                    \\usepackage[style=authortitle-ibid,sortcites=true,sorting=nyt,backend=biber]{biblatex}
                    \\usepackage{xurl}
                    \\usepackage[colorlinks=true,urlcolor=blue,citecolor=blue,breaklinks=true]{hyperref}
                    [NO-DEFAULT-PACKAGES]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

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

(use-package lsp-ui
  :ensure t
  :after lsp-mode)

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
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+R-mode))
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq scroll-down-aggressively 0.01)
  (setq ess-fancy-comments nil)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (get-process "R"))
    ;(if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (progn
          (setq w1 (selected-window))
          (setq w2 (split-window-horizontally))
          (R)
          (display-line-numbers-mode -1)
          (set-window-buffer w2 "*R*")
          (select-window w1))
      (if (and (get-buffer-window "*R*"))
          (delete-other-windows)
        (progn (let ((w1 (selected-window))
                     (w2 (split-window-horizontally)))
                 (set-window-buffer w2 "*R*")
                 (selecte-window w1))))))
  (add-hook 'ess-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "SPC r s") 'my-ess-start-R)
              (define-key evil-normal-state-local-map (kbd "SPC r r") (lambda () (interactive) (ess-eval-function-or-paragraph-and-step))))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-lsp-cache-candidates t)
  (setq company-lsp-async t)
  (add-to-list 'company-backends 'company-gtags)
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
               "t c" 'my-clear-shell
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
               "n a" '(lambda () (interactive) (find-file "~/workspace/notes.org"))
               "n f" 'make-frame
               "j" 'evil-scroll-down
               "k" 'evil-scroll-up
               ;; magit
               "m s" 'magit
               "m e" 'elfeed
               "m m" 'mu4e
               ;; view
               "d t" (lambda () (interactive) (progn (disable-theme 'gruvbox-dark-medium) (disable-theme 'acme) (load-theme 'tsdh-light) (set-face-background 'mode-line "gold")))
               "d g" (lambda () (interactive) (load-theme 'gruvbox-dark-medium))
               "d a" (lambda () (interactive) (load-theme 'acme))
               "d f" (lambda () (interactive) (toggle-frame-fullscreen))
               "=" (lambda () (interactive) (my-global-font-size 10))
               "-" (lambda () (interactive) (my-global-font-size -10)))))

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
;(load-theme 'tsdh-light)
;(set-face-background 'mode-line "gold")
(load-theme 'gruvbox-dark-medium)

(set-face-attribute 'default nil
                    :family "mononoki"
                    :height my-font-size
                    :weight 'medium)

(global-display-line-numbers-mode)
(global-hl-line-mode)

