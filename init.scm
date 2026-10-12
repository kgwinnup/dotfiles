(set-option "font-family" "JetBrains Mono")
(set-option "auto-revert" #t)

;; Make C-c C-t cycle through ONLY these labels (drop the built-in TODO vocab),
;; so the rotation is: nothing -> BUG -> FEATURE -> IDEA -> nothing.
(set! *org-todo-keywords* '())
(set! *org-custom-keywords* (list "TODO" "BUG" "FEATURE" "IN-PROGRESS" "DONE"))

;; Optional: give some/all a color (omitted labels inherit the title color).
(set! *org-custom-keyword-colors*
  (list (cons "TODO"    "#fe8019")
        (cons "BUG"     "#fb4934")
        (cons "FEATURE" "#83a598")
        (cons "IN-PROGRESS"    "#fabd2f")
        (cons "DONE"    "#b8bb26")))
(org--register-custom-keyword-faces)

;;; ============================================================================
;;; EVIL MODE + SPC LEADER (ported from my old Emacs/evil-leader config)
;;; ============================================================================
;; Opt in to Vim modal editing. SPC is used as a Doom/Spacemacs-style leader by
;; binding "space …" chords in the evil-normal keymap (global) and in the
;; per-major-mode evil keymaps (evil-define-key, the analogue of
;; evil-leader/set-key-for-mode).
;;
;; Skipped from the original (no macros equivalent / per request): gptel (mv ma
;; mr mg), theme toggles (mt), eww (me), elfeed (ml), diff-hl (md), ispell
;; (ss sr), github-link (mm), PR review (mu), and the custom toggle-windows (nw)
;; / org-sort (S) helpers.

 (evil-mode)
 
 ;; --- helper commands the leader binds to ------------------------------------
 (define (my/scratch) (switch-to-buffer "*scratch*"))
 (define (my/notes)   (open-file "~/repos/org/notes.org"))
 
 ;; Helm picker over my org directory (was helm-find-files-1 "~/repos/org/").
 (define (my/org-files)
   (helm-from "~/repos/org/"
              (list (list "Files" 'find-file-candidates 'open-file))))
 
 ;; Kill every buffer, then land on *scratch* (was the `nk` lambda).
 (define (my/kill-all-buffers)
   (for-each (lambda (b) (kill-buffer-named b)) (buffer-names))
   (switch-to-buffer "*scratch*"))
 
 ;; Open this config (was `co` -> ~/.emacs.d/init.el).
 (define (my/edit-config)
   (open-file "~/.config/macros/init.scm"))
 
 ;; Font size +/- (was my/global-font-size). font-size is a string option.
 (define (my/font-size-num)
   (let ((v (get-option "font-size")))
     (cond ((number? v) v)
           ((string? v) (or (string->number v) 17))
           (else 17))))
 (define (my/font-inc) (set-option "font-size" (number->string (+ (my/font-size-num) 1))))
 (define (my/font-dec) (set-option "font-size" (number->string (- (my/font-size-num) 1))))
 
 ;; Insert an R source block and drop the cursor on the empty body line (was `i`).
 (define (my/org-insert-src-r)
   (insert "#+BEGIN_SRC R :session :results none\n\n#+END_SRC\n")
   (previous-line) (previous-line))
 
 ;; Leader key — SPC by default. Change it here (before the bindings below) and
 ;; every leader chord moves with it, e.g. (evil-set-leader "comma").
 (evil-set-leader "space")
 
 ;; --- global leader bindings (was evil-leader/set-key) -----------------------
 ;; Config
 (evil-leader-set "c o" 'my/edit-config)

 ;; Project
 (evil-leader-set "v u" 'compile)              ; projectile-compile-project

 ;; Buffer / window management
 (evil-leader-set "n k" 'my/kill-all-buffers)
 (evil-leader-set "n w" 'other-window)
 (evil-leader-set "n n" 'next-buffer)
 (evil-leader-set "n p" 'previous-buffer)
 (evil-leader-set "n o" 'delete-other-windows)
 (evil-leader-set "n d" 'kill-buffer-and-window)
 (evil-leader-set "n b" 'helm-mini)
 (evil-leader-set "n r" 'my/scratch)
 (evil-leader-set "n a" 'my/notes)
 (evil-leader-set "n m" 'my/org-files)
 (evil-leader-set "n f" 'projectile-find-file)
 (evil-leader-set "n g" 'projectile-grep)
 (evil-leader-set "n t" 'dired-sidebar)             ; dired-sidebar-toggle
 (evil-leader-set "n l" 'helm-buffers-list)

 ;; Window splits (emacs vertical = stacked = below; horizontal = side-by-side)
 (evil-leader-set "_" 'split-window-below)
 (evil-leader-set "|" 'split-window-right)

 ;; Scrolling
 (evil-leader-set "j" 'evil-scroll-down)
 (evil-leader-set "k" 'evil-scroll-up)

 ;; Git / Magit
 (evil-leader-set "m s" 'magit)               ; magit status
 (evil-leader-set "m b" 'magit-blame-toggle)  ; magit-blame-addition
 (evil-leader-set "m f" 'magit-log-file)      ; magit-log-buffer-file

 ;; Font size
 (evil-leader-set "=" 'my/font-inc)
 (evil-leader-set "-" 'my/font-dec)
 
 ;; --- per-major-mode leader: LSP (was the eglot/xref dolist) -----------------
 ;; Built-in LSP replaces eglot. Applied to the code modes the original covered.
 (evil-leader-set-for-modes
   '("rust" "go" "typescript" "steel" "ocaml")
   (list (cons "g g" 'lsp-find-definition)     ; xref-find-definitions
         (cons "g l" 'lsp-find-references)      ; xref-find-references
         (cons "g p" 'xref-pop-marker)          ; pop-tag-mark
         (cons "g r" 'lsp-rename)               ; eglot-rename
         (cons "t"   'lsp-hover)                ; eldoc-box-help-at-point
         (cons "g u" 'enable-lsp)))             ; eglot-reconnect

 ;; --- grid buffers: open CSV/TSV (and friends) as a spreadsheet ---------------
 ;; This is the built-in default, pinned here. `grid-extensions` REPLACES the
 ;; list, so keep every extension that should open as a grid.
 (grid-extensions '("csv" "tsv" "tab" "psv" "xlsx"))

 ;; --- per-major-mode leader: grid buffers (CSV/TSV/.xlsx) --------------------
 ;; On the cursor cell, or every cell of a selection (v / V / C-v).
 (evil-leader-set-for-modes
   '("grid-mode")
   (list (cons "g g" 'grid-toggle-bold)          ; bold ⇄ normal
         (cons "g p" 'grid-cycle-text-color)     ; red → orange → … → purple → none
         (cons "g k" 'grid-clear-format)))       ; bold and colors off
 
 ;; --- language servers --------------------------------------------------------
 ;; OCaml — ocaml-lsp-server from opam. Full absolute path because the GUI
 ;; launch doesn't inherit the opam shell PATH and the LSP spawn does no ~
 ;; expansion. .mli files need the "ocaml.interface" languageId on the wire.
 ;; format-on-save goes through the server (ocamlformat; needs a .ocamlformat
 ;; file in the project or formatting is a no-op).
 ;; The server itself shells out to `dune` (and ocamlformat), and child
 ;; processes inherit macros' env — so the opam bin dir must be on PATH too.
 (setenv "PATH" (string-append "/Users/kylegwinnup/.opam/5.5.0/bin:" (getenv "PATH")))
 (lsp-language "ocaml"
   'extensions     '("ml" "mli")
   'language-ids   '(("mli" . "ocaml.interface"))
   'command        "~/.opam/5.5.0/bin/ocaml_lsp"
   'format-on-save 'lsp)

 ;; Rust — rust-analyzer from rustup (~/.cargo/bin). Same PATH story as opam:
 ;; the GUI launch doesn't inherit the shell PATH, and rust-analyzer shells out
 ;; to `cargo` (cargo check diagnostics), so its bin dir goes on PATH too.
 ;; format-on-save goes through the server (rustfmt).
 (setenv "PATH" (string-append "/Users/kylegwinnup/.cargo/bin:" (getenv "PATH")))
 (lsp-language "rust"
   'extensions     '("rs")
   'command        "~/.cargo/bin/rust-analyzer"
   'format-on-save 'lsp)

 ;; --- per-major-mode leader: Org --------------------------------------------
 (evil-leader-set-for-modes
   '("org-mode")
   (list (cons "r r" 'org-babel-execute)        ; execute src block
         (cons "u"   'org-todo)                 ; cycle TODO
         (cons "i"   'my/org-insert-src-r)))    ; insert R src block

;;; ============================================================================
;;; k8s-dashboard — Kubernetes dashboard
;;; ============================================================================
;; A magit-style status buffer for kubectl: foldable, color-coded sections for
;; nodes / deployments / pods / services / warnings. Point-and-key actions give
;; describe, logs, shell, yaml, scale, and delete. Requires kubectl on PATH.
;; ALL runtime state lives in one `k8s-state` hash.
;; `'config` runs after the package loads. Use /bin/sh (not the /bin/bash
;; default) when shelling into a pod with `s`. (Works because `set-option` is a
;; core builtin — a config thunk that called a package-defined function would
;; fail to resolve at load time.)
(package-install-local "~/repos/k8s-dashboard"
  'config (lambda () (set-option "k8s-shell" "/bin/sh")))

;; Launch with M-x k8s-dashboard (no global key is bound). In *k8s-dashboard*:
;; r=refresh TAB=fold d=describe l=logs s=shell/scale y=yaml x=delete
;; N=namespace A=all-ns C=context a=auto-refresh ?=more q=quit.

;;; ============================================================================
;;; macros-feeds — RSS/Atom feed reader (an elfeed clone)
;;; ============================================================================
;; Search buffer of entries from all feeds (newest first, elfeed filter
;; language: +tag -tag @2-weeks-ago #100 regex !regex), a show buffer for one
;; entry, and a persistent database (~/.macros-feeds-db) of read/unread state
;; and tags. Fetches with the async `http-request` primitive — no curl needed.
;; ALL runtime state lives in one `*feeds-state*` hash.
;;
;; Config is options (the set-option channel, like k8s above):
;;   "feeds"         — the feed list: whitespace-separated `url` or `url|tag,tag`
;;   "feeds-filter"  — the filter the search buffer starts with
;;   "feeds-db-file" — database path (default ~/.macros-feeds-db; `~/` ok)
(package-install-local "~/repos/feeds"
  'config (lambda ()
            (set-option "feeds"
                        (string-append
                         "https://lobste.rs/rss|dev "
                         "https://hnrss.org/frontpage|hn "))
            (set-option "feeds-filter" "@6-months-ago +unread")))

;; SPC m f → feeds (the package also binds C-c f globally). In *feeds-search*:
;; enter=show b=browser r/u=read/unread +/-=tag s=filter g=refresh G=fetch
;; (evil: gr=refresh G/R=fetch) q=quit. In *feeds-entry*: n/p=next/prev b=browser.
(evil-leader-set "m f" 'feeds)

;;; ============================================================================
;;; psql-dashboard — PostgreSQL dashboard
;;; ============================================================================
;; A magit-style dashboard for psql: every schema as a foldable section listing
;; its relations with row estimates and sizes, plus live backend activity.
;; RET/g drills into a schema, then a table (columns/indexes/constraints/
;; stats); TAB folds; ^ goes back up. `s` opens a half-window sql-mode query pane —
;; C-c C-c runs it async, C-c C-g cancels, results land in *psql-results*.
;; psql binary + connection are options (set below). ALL runtime state lives
;; in one `psql-state` hash.
;;
;; Connection is the "psql-url" option (anything `psql -d` accepts). Left unset,
;; psql falls back to the PG* environment / ~/.pgpass. Change it live with U in
;; the dashboard or M-x psql-set-url.
(package-install-local "~/repos/psql-dashboard"
  'config (lambda ()
            (set-option "psql-command" "/opt/homebrew/opt/libpq/bin/psql")
            (set-option "psql-url" (secret-get "prod-read-only" ""))
            #f))

;; Launch with M-x psql-dashboard (no global key is bound). In the dashboard:
;; r=refresh RET/g=drill-in TAB=fold ^=up s=query U=url Y=copy ?=menu q=quit.

(mcp-server-start)
;;; ============================================================================
;;; macros-analytics — Cloudflare hits on macros.dev/latest.json
;;; ============================================================================
;; Fetches day-by-day request analytics from Cloudflare's GraphQL API, from the
;; day after the last complete saved day through today, and keeps them in
;; ~/.config/macros/macros-analytics/ (Cloudflare's free plan only holds a few
;; days, so this is the long-term record). The API token (Zone → Analytics →
;; Read) is asked for on the first fetch and saved in authinfo.gpg as
;; machine api.cloudflare.com login macros-analytics; the zone ID is asked for
;; once and remembered.
(package-install-local "~/workspace/macros-analytics"
  'config (lambda ()
            (set-option "analytics-zone-id" "b71ca97ddcf710fc18687f15c991ae4c")
            (set-option "analytics-host" "macros.dev")
            (set-option "analytics-path" "/latest.json")))

;; SPC m c → analytics dashboard. In *analytics*: r=fetch w=window(7/30/90/all)
;; enter=filter by IP c=clear y=copy IP o=ipinfo e=CSV tab=fold ?=menu q=quit.
(evil-leader-set "m c" 'analytics)
