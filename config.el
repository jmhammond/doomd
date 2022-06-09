;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "John Hammond"
      user-mail-address "jmhammond@gmail.com"

      doom-font (font-spec :family "Source Code Pro" :size 16)
      doom-variable-pitch-font (font-spec :family "Source Serif 4" )
      doom-serif-font (font-spec :family "Source Serif 4" )
      doom-theme 'modus-vivendi
      ;; doom-font (font-spec :family "Fira Code" :size 16)
      ;; doom-variable-pitch-font (font-spec :family "Baskerville" :size 19)
      ;; doom-serif-font (font-spec :family "Baskerville" :height 45)
      ;; doom-font (font-spec :family "Input Mono" :size 14)
      ;; doom-variable-pitch-font (font-spec :family "Source Code Variable" :size 14)
      ;doom-theme 'doom-nord-light
      ;doom-theme 'doom-dark+
      ;doom-theme 'doom-zenburn
      ;doom-theme 'doom-vibrant
      ;doom-theme 'doom-dracula
      ;doom-theme 'doom-material
      ;;doom-theme 'doom-one-light
;     ; doom-theme 'doom-vibrant
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
;
; The following makes emacs follow (correctly!) the links setup in Obsidian
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
;;
;; (autoload #'mixed-pitch-serif-mode "mixed-pitch"
;;   "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

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
; calculator! I need to actually use it sometimes...
(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t)


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
        treemacs-width 35
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

;; (after! centaur-tabs
;;   (centaur-tabs-mode -1)
;;   (setq centaur-tabs-height 24
;;         centaur-tabs-set-icons t
;;         ;centaur-tabs-modified-marker "o"
;;         centaur-tabs-close-button "×"
;;         centaur-tabs-set-bar 'under
;;         centaur-tabs-plain-icons t
;;         centaur-tabs-style "rounded"
;;         centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-change-fonts "Baskerville" 180))
;; (setq x-underline-at-descent-line t)
;;

                                        ;(after! all-the-icons
                                        ;  (setcdr (assoc "ptx" all-the-icons-extension-icon-alist)
                                        ;          (cdr (assoc "xml" all-the-icons-extension-icon-alist))))
                                        ;(after! all-the-icons
                                        ;  (all-the-icons-wicon   "ptx"))
                                        ;(insert (all-the-icons-icon-for-file "foo.js"))
;; Inserts a javascript icon
;; #("js-icon" 0 1 (display (raise -0.24) face (:family "alltheicon" :height 1.08 :foreground "#FFD446")))
;;
;;
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

;; Math-preview

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


;; For macos auctex building
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))


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


;; 3-10-22: Note: this was back in ChromeOS crouton days. I think I'm okay to leave a window unsaved when jumping.
;; and when losing focus, enter normal mode go ahead and save
;; (add-hook! '(doom-switch-window-hook
;;              doom-switch-buffer-hook)
;;   ;; (evil-normal-state t) ;; this breaks company's popup window, so disable
;;   (save-some-buffers t))

(add-to-list 'auto-mode-alist '("~/TheArchive/.*\\.md\\'" . org-mode))


;; In case you forget, me, line-numbers are terrible for org files and emacs performance
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)

(map! :leader
      ;; prefer the unshifted semicolon for Ex commands
      ";" 'execute-extended-command
      ":" 'eval-expression)

;; this keymap is similar to the evil default "z i" for org-toggle-inline-images
(map! :mode org-mode :n "z p" 'org-toggle-latex-fragment)

(after! org

  ;; Don't delete hidden subtrees:
  (setq org-ctrl-k-protect-subtree t)

  (require 'org-ref)
  (require 'org-re-reveal-ref)


;;;  ; this super doesn't work. :-(
  (setq org-capture-templates
        '(("w"
           "Default template"
           entry
           (file+headline "~/org/capture.org" "Notes")
           "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1)
          ("l" "A link, for reading later." entry (file+headline "~/org/inbox.org" "Reading List") "* %:description\n%u\n\n%c\n\n%i" :empty-lines 1)
          ))


  ;; (defun org-roam-dailies-capture-today ()
  ;;   "Capture a note into the daily note for today."
  ;;   (interactive)
  ;;   (let ((org-roam-capture-templates org-roam-dailies-capture-templates)
  ;;         (org-roam-capture--info (list (cons 'time (current-time))))
  ;;         (org-roam-capture--context 'dailies))
  ;;     (org-roam--capture)))

  ;; (setq org-roam-dailies-capture-templates
  ;;       '(("d" "daily" plain (function org-roam-capture--get-point)
  ;;          "* %?"
  ;;          :file-name "%<%Y-%m-%d>"
  ;;          :head "#+TITLE: %<%Y-%m-%d>")))


  ;; (use-package! org-roam
  ;;   :after org
  ;;   :demand t
  ;;   :commands
  ;;   (org-roam-buffer
  ;;    org-roam-setup
  ;;    org-roam-capture
  ;;    org-roam-node-find)
  ;;   :config
  ;;   ;;(setq org-roam-mode-sections
  ;;   ;;      (list #'org-roam-backlinks-insert-section
  ;;   ;;            #'org-roam-reflinks-insert-section
  ;;   ;;            #'org-roam-unlinked-references-insert-section))
  ;;   (org-roam-setup))

  ;;
  ;;
  ;; My attempt at capturing to the daily note
  ;; (defun visit-the-daily-note()
  ;;   "Visit a new file named by the current timestamp"
  ;;   (interactive)
  ;;   (let* (
  ;;          (curr-date-stamp (format-time-string "%Y-%m-%d.org"))
  ;;          (file-name (expand-file-name curr-date-stamp "~/Documents/org/roam/")))
  ;;     (set-buffer (org-capture-target-buffer file-name))
  ;;     (goto-char (point-max))))

  ;; (setq org-capture-templates '(("n" "Note" entry (function visit-the-daily-note)
  ;;                                "* %?\n")))
  ;;


  ;; obsidan link handling for obsidian:// links
  (defun org-obidian-link-open (slash-message-id)
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
  (org-link-set-parameters "obsidian" :follow #'org-obidian-link-open)

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

  )
