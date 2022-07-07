;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "John Hammond"
      user-mail-address "jmhammond@gmail.com"

      doom-font (font-spec :family "Source Code Pro" :size 16)
      doom-variable-pitch-font (font-spec :family "Source Serif 4" )
      doom-serif-font (font-spec :family "Source Serif 4" )
      doom-theme 'modus-vivendi
      ;doom-theme 'modus-operandi
      ;; doom-font (font-spec :family "Fira Code" :size 16)
      ; doom-variable-pitch-font (font-spec :family "Inconsolata" :size 19)
      ; doom-serif-font (font-spec :family "Inconsolata" :height 45)
      ;; doom-font (font-spec :family "Input Mono" :size 14)
      ;; doom-variable-pitch-font (font-spec :family "Source Code Variable" :size 14)
      +doom-dashboard-banner-file (expand-file-name "coffeesquirrel.png" doom-private-dir)

      initial-frame-alist '((top . 1) (left . 1) (width . 100) (height . 40))

;      display-line-numbers-type 'visual
      ; let f, s, etc, find on visual lines
      evil-cross-lines t

      evil-snipe-scope 'buffer

      ;; make *Scratch* act like org-mode
      doom-scratch-buffer-major-mode 'org-mode

      )

(setq emacs-everywhere-major-mode-function #'org-mode
      emacs-everywhere-paste-p nil
      )

(when (equal system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  ;; For macos auctex building
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
  (setq exec-path (append exec-path '("/Library/TeX/texbin/")))

  ;; I want scrollbars on macos
  (setq scroll-bar-mode 'right)
  (scroll-bar-mode)


  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ; it is a bug in emacs that external keyboards plugged in treat ALL modifier keys has right-modifiers... so make right modifer option to meta to get the standard alt behavior! https://github.com/hlissner/doom-emacs/issues/4178
        mac-right-option-modifier 'meta
        ns-right-option-modifier  'meta)
)

; The following makes emacs follow (correctly!) the links setup in Obsidian and Logseq
(setq markdown-enable-wiki-links t
      markdown-wiki-link-search-type '(parent-directories sub-directories)
      markdown-enable-math t
      markdown-wiki-link-fontify-missing t
      )

; mixed pitch fonts; proportional AND fixed in the same buffer
; from https://github.com/tecosaur/emacs-config/blob/master/config.org
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  ;(setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font doom-variable-pitch-font)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

;;;;;; copied from tecosaur

; split windows, be asked what to load:
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; treemacs
;;
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(map! :g "M-0" #'treemacs-select-window)
(map! :g "M-o" #'treemacs-select-window)
(after! treemacs
  (treemacs-follow-mode t)
  (treemacs-indent-guide-mode t)
  (setq treemacs-resize-icons 44
        treemacs-width 30
        treemacs-width-is-initially-locked nil
        treemacs-indent-guide-style 'line
        treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux"
          "ptc"
          "fdb_latexmk"
          "fls"
          "synctex.gz"
          "toc"
          ;; LaTeX - glossary
          "glg"
          "glo"
          "gls"
          "glsdefs"
          "ist"
          "acn"
          "acr"
          "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"
          )
        treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex")))

(after! evil
  (setq evil-ex-substitute-global t
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode; this is truly game changing!
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring


;;
;; hopefully don't let doom upgrade compile natively every package
(setq native-comp-deferred-compilation t)

(setq
 doom-modeline-icon (display-graphic-p)
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
                                        ;doom-vibrant-brighter-modeline t
 doom-modeline-height 1
 doom-modeline-buffer-state-icon t)
(setq all-the-icons-scale-factor 1.0)
(custom-set-faces!
  '(mode-line :family "Fira Code" :height 1.0)
  '(mode-line-inactive :family "Fira Code" :height 1.0))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook! 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


;; which-key popups are good.
(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; ignore unneccessary tex files and such in find-file
(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^#\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)\\|\\(aux\\)\\|\\(fdb_latexmk\\)\\|\\(fls\\)\\|\\(out\\)\\|\\(synctex\\)\\|\\(pdf\\)\\|\\(log\\)"))

;; automatically update buffers if the file has changed on the disk;
(global-auto-revert-mode t)

;; soft wrap everywhere
;; (note I also needed something in init.el)
(global-visual-line-mode +1)
;;(+global-word-wrap-mode +1)
(setq +word-wrap-extra-indent 2)
                                        ; this is responsible for hard wrapping.
(remove-hook 'text-mode-hook #'auto-fill-mode)


;; smartparens specific configurations
;; disable all smart-parens completions; I kind of hate it.
;; autoclose created too many > characters
(sp-local-pair 'nxml-mode "<" ">" :post-handlers '(("[d1]" "/")))
(setq nxml-slash-auto-complete-flag t)

;; spell checking
;; (setf (alist-get 'nxml-mode +spell-excluded-faces-alist)
;;       '(nxml-namespace-attribute-xmlns
;;         nxml-attribute-value
;;         nxml-element-local-name)
;; )
;;


;; Math-preview; this package works even in nxml mode!
(setq math-preview-tex-macros
   '(("ddx" "\\frac{d#2}{d#1}" 2 "t")
     ("and" . "\\mbox{ and }"))
   math-preview-tex-marks
   '(("\\begin{equation}" "\\end{equation}")
     ("\\begin{equation*}" "\\end{equation*}")
     ("\\[" "\\]")
     ("$$" "$$")
     ("<m>" "</m>")
     ("<me>" "</me>")
     ("<mrow>" "</mrow>")))



;; compile PreText documents via `pretext build'
(defun my-make-compile ()
  (setq compile-command "pretext build html")
  (local-set-key (kbd "C-c C-c") 'recompile))
(add-hook 'nxml-mode-hook 'my-make-compile)

;; apparently this helps somehow
(setq undo-tree-enable-undo-in-region nil)

(map! :ne "M-/" #'comment-or-uncomment-region)


(defun +evil-embrace-dollars-h ()
  (embrace-add-pair ?$ "$" "$"))
(add-hook 'org-mode-hook #'+evil-embrace-dollars-h)
(add-hook 'nxml-mode-hook #'+evil-embrace-dollars-h) ; <-- why in xml mode? there it's <m>...

;; First, dump smartparens in AucTex, then use Auctex's own electric bracket and math closures
                                        ;(add-hook 'LaTeX-mode-hook #'turn-off-smartparens-mode)
(setq TeX-electric-sub-and-superscript nil)
;; (setq LaTeX-electric-left-right-brace 't)
;; (setq TeX-electric-math (cons "$" "$"))

;; (global-evil-motion-trainer-mode 1)
;; (setq evil-motion-trainer-threshold 4)
                                        ; (setq evil-motion-trainer-super-annoying-mode t)


;; disable the company idle popup
;; work-around for org-tables and TAB
(setq company-idle-delay nil)

;; unmap tab from company and yas-snippets in insert mode:
(map! :map company-keymap "TAB" nil)
(map! :map yas-keymap "TAB" nil)

;; popups
(set-popup-rules!
  '(
    ("^\\*Warnings" :select t)
    ("^\\*compilation" :select t)
    ("^\\*Completions" :slot -1 :ttl 0)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
    ("^\\*Help" :slot -1 :size 0.4 :select t)
    ("^\\*doom:"
     :size 0.35 :select t :modeline t :quit t :ttl t)))

; With no error, get rid of the compile window
(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                (progn
                  (run-at-time
                   "0.3 sec" nil 'delete-windows-on buf)
                  (message "No Compilation Errors.")))))


(add-to-list 'auto-mode-alist '("~/TheArchive/.*\\.md\\'" . org-mode))


(map! :leader
      ;; prefer the unshifted semicolon for Ex commands
      ";" 'execute-extended-command
      ":" 'eval-expression)
(map! :i
      ;; use caps(ctrl) + ; to trigger the M-x command list
      "C-;" 'execute-extended-command)
;(map! :mode org-mode :n "S-TAB" 'org-cycle)


;; this keymap is similar to the evil default "z i" for org-toggle-inline-images
; (map! :mode org-mode :n "S- p" 'org-toggle-latex-fragment)

; (use-package! org-ql :after org)

;; autosave org files
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (when (file-in-directory-p (buffer-file-name) "~/org")
;;               (salv-mode))))
;; this is via https://www.reddit.com/r/emacs/comments/vqk41j/comment/iestdam/?utm_source=reddit&utm_medium=web2x&context=3
(auto-save-visited-mode 1)  ;; auto save everything

;; (setq-default auto-save-visited-mode nil)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (when (file-in-directory-p (buffer-file-name) "~/org")
;;               (setq-local auto-save-visited-mode t))))

(after! org

  ;; Don't delete hidden subtrees:
  (setq org-ctrl-k-protect-subtree t)

  ;; for ~org-cite~
  (setq! org-cite-csl-styles-dir "~/Zotero/styles")

  (require 'org-mouse) ;; allows clicking headlines to open/close

  ;; obsidan link handling for obsidian:// links
  (defun org-obsidian-link-open (slash-message-id)
    "Handler for org-link-set-parameters that opens a obsidian:// link in obsidian"
    ;; remove any / at the start of slash-message-id to create real note-id
    (let ((message-id
           (replace-regexp-in-string (rx bos (* "/"))
                                     ""
                                     slash-message-id)))
      (do-applescript
       (concat "tell application \"Obsidian\" to open location \"obsidian://"
               message-id
               "\" activate"))))
  ;; on obsdian://aoeu link, this will call handler with //aoeu
  (org-link-set-parameters "obsidian" :follow #'org-obsidian-link-open)

  ;; Email link handlink for message:// links
  (defun org-message-mail-open (slash-message-id)
    "Handler for org-link-set-parameters that opens a message:// link in apple mail"
    ;; remove any / at the start of slash-message-id to create real message-id
    (let ((message-id
           (replace-regexp-in-string (rx bos (* "/"))
                                     ""
                                     slash-message-id)))
      (do-applescript
       (concat "tell application \"mail\" to open location \"message://"
               message-id
               "\" activate"))))
  ;; on message://aoeu link, this will call handler with //aoeu
  (org-link-set-parameters "message" :follow #'org-message-mail-open)


  ;; UPDATE THIS FOR TODO LIST STUFF!!!!!!
  ;; Org for GTD
  (setq org-directory "~/org")
  (setq org-agenda-files
        (mapcar 'file-truename
                (file-expand-wildcards "~/org")))

                                        ;doom emacs has its own capture templates and refile targets that overwrite any that I make; my own have to be inside an !after block to overwrite the overwriters.
                                        ;
  ;; capture templates
  (setq org-capture-templates
        `(("i" "Inbox" entry (file "inbox.org")
           "* TODO %?\n %l")))
  (setq org-refile-targets '((nil :maxlevel . 5) ; current file
                             ("gtd.org" :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(h)" "SOMEDAY(s)" "PROJ(p)" "|" "DONE(d)")))


  ;; autosave on refile
;;   (defun gtd-save-org-buffers ()
;;     "Save `org-agenda-files' buffers without user confirmation.
;; See also `org-save-all-org-buffers'"
;;     (interactive)
;;     (message "Saving org-agenda-files buffers...")
;;     (save-some-buffers t (lambda ()
;;                            (when (member (buffer-file-name) org-agenda-files)
;;                              t)))
;;     (message "Saving org-agenda-files buffers... done"))
;;   (advice-add 'org-refile :after
;;               (lambda (&rest _)
;;                 (gtd-save-org-buffers)))
  ;; refile faster with <spc> m r
  ;(map! :leader :desc "org-refile" "m r" #'org-refile)

  (setq org-archive-location "~/org/archive/%s_archive::")

  (setq org-log-done 'time)

  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; in the agenda, hide the todo state
  ; (setq org-agenda-todo-keyword-format "")
  ;; ;; hide done tasks
  (setq org-agenda-skip-scheduled-if-done "")

  (setq org-agenda-custom-commands
        '(("Q" . "Custom queries") ;; gives label to "Q"
           ("Qa" "Archive search" search ""
            ((org-agenda-files (file-expand-wildcards "~/org/archive/*.org_archive")
                               ((org-agenda-overriding-header "From the Archives"))
                               )))
           ("Qb" "Projects and Archive" search ""
            ((org-agenda-text-search-extra-files
              (file-expand-wildcards "~/archive/*.org_archive")
              ((org-agenda-overriding-header "From the Archives"))
              )))
           ;; searches both projects and archive directories
           ("QA" "Archive tags search" org-tags-view ""
            ((org-agenda-files (file-expand-wildcards "~/org/archive/*.org_archive")
                               ((org-agenda-overriding-header "From the Archives"))
                               )))
          ("i" "Inbox / To Refile"
           ((tags "refile-ignore" ;; note "-" means ignore
                  ((org-agenda-overriding-header "Items to Refile")))))
          ("s" "Shopping / Buy List"
           ((tags "shopping")))
          ("p" "Projects List"
           ((todo "PROJ"))
           ((org-agenda-overriding-header "Active Projects List\n")))
          ("g" "Today's Agenda"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))
            ))))


  ;; this is org-super-agenda
  ;; (setq org-agenda-skip-scheduled-if-done t
  ;;     org-agenda-skip-deadline-if-done t
  ;;     org-agenda-include-deadlines t
  ;;     org-agenda-block-separator nil
  ;;     org-agenda-compact-blocks t
  ;;     org-agenda-start-day nil ;; i.e. today
  ;;     org-agenda-span 1
  ;;     org-agenda-start-on-weekday nil)
  ;; (setq org-agenda-custom-commands
  ;;       '(("c" "Super view"
  ;;          ((agenda "" ((org-agenda-overriding-header "Calendar")
  ;;                       (org-super-agenda-groups
  ;;                        '((:name "Today"
  ;;                                 :time-grid t
  ;;                                 :date today
  ;;                                 :order 1)))))
  ;;           (alltodo "" ((org-agenda-overriding-header "All todos")
  ;;                        (org-super-agenda-groups
  ;;                         '((:auto-category t)
  ;;                           (:log t)
  ;;                           (:name "To refile"
  ;;                                  :file-path "inbox.org")
  ;;                           (:name "Next to do"
  ;;                                  :todo "NEXT"
  ;;                                  :order 1)
  ;;                           ;; (:name "Today's tasks"
  ;;                           ;;        :file-path "journal/")
  ;;                           (:name "Due Today"
  ;;                                  :deadline today
  ;;                                  :order 2)
  ;;                           (:name "Scheduled Soon"
  ;;                                  :scheduled future
  ;;                                  :order 8)
  ;;                           (:name "Overdue"
  ;;                                  :deadline past
  ;;                                  :order 7)
  ;;                           (:discard (:not (:todo "TODO")))))))))
  ;;         ("Z" "Testing"
  ;;          ((agenda "" ((org-super-agenda-groups
  ;;      '((:log t)  ; Automatically named "Log"
  ;;        (:name "Schedule"
  ;;               :time-grid t)
  ;;        (:name "Today"
  ;;               :scheduled today)
  ;;        (:habit t)
  ;;        (:name "Due today"
  ;;               :deadline today)
  ;;        (:name "Overdue"
  ;;               :deadline past)
  ;;        (:name "Due soon"
  ;;               :deadline future)
  ;;        (:name "Unimportant"
  ;;               :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
  ;;               :order 100)
  ;;        (:name "Waiting..."
  ;;               :todo "WAITING"
  ;;               :order 98)
  ;;        (:name "Scheduled earlier"
  ;;               :scheduled past))))
  ;; (org-agenda-list))))))


  (setq org-fold-catch-invisible-edits 'show-and-error)
) ;; end org-mode


(setq! orderless-matching-styles
       '(orderless-literal orderless-regexp orderless-flex)
       )


;; In case you forget, me, line-numbers are terrible for org files and emacs performance
;; ... but I love the extra space in the gutter
(defun jq-no-lines-but-gutter ()
  (doom-disable-line-numbers-h)
  (set-window-margins (selected-window) 3 3)
  )
(add-hook 'org-mode-hook 'jq-no-lines-but-gutter)

;; Make org files look prettier with modus themes
;; https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
(setq modus-themes-headings
      '((1 . (rainbow overline 1.2))
        (2 . (rainbow 1.1))
        (t . (rainbow  1.0))
        ))
        ;(t . (rainbow 1.0))))
(setq modus-themes-scale-headings t)

;; Calendar and such, hopefully🤞
;; (setq diary-file "~/emacsCalendar")
;; (load-file "~/src/org-mac-iCal/org-mac-iCal.el")
;; ;(setq org-agenda-include-diary t)
;; (require 'org-mac-iCal)

;; caching has been causing problems with big org files...
(setq org-element-use-cache nil)
