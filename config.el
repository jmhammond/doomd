;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "John Hammond"
      user-mail-address "jmhammond@gmail.com"

      ;; make *Scratch* act like org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-font (font-spec :family "Source Code Pro" :size 16)
      ;doom-theme 'doom-nord-light
      ;doom-theme 'doom-dark+
      ;doom-theme 'doom-zenburn
      ;doom-theme 'doom-one
      doom-theme 'doom-dracula
      ;;doom-theme 'doom-one-light
      +doom-dashboard-banner-file (expand-file-name "coffeesquirrel.png" doom-private-dir)

      initial-frame-alist '((top . 1) (left . 1) (width . 100) (height . 40))

      display-line-numbers-type 'visual
      ; let f, s, etc, find on visual lines
      evil-cross-lines t

      evil-snipe-scope 'buffer

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

;; deft
(after! 'deft
  (setq deft-extensions '("txt" "md" "org"))
  (setq deft-recursive t)
  (add-to-list deft-recursive-ignore-dir-regexp
               "papers") ; ignore the papers subdirectory created by Zotero > mdnotes
  (define-key deft-mode-map (kbd "C-p") 'widget-backward)
  (define-key deft-mode-map (kbd "C-n") 'widget-forward)
  )

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
(add-hook 'nxml-mode-hook #'+evil-embrace-dollars-h)

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
 '(("^ \\*" :slot -1) ; fallback rule for special buffers
   ("^\\*" :select t)
   ("^\\*Warnings" :select t)
   ("^\\*compilation" :select t)
   ("^\\*Completions" :slot -1 :ttl 0)
   ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
   ("^\\*Help" :slot -1 :size 0.2 :select t)
   ("^\\*doom:"
    :size 0.35 :select t :modeline t :quit t :ttl t)))

;; (set-popup-rules!
;;   '(("^\\*Warnings" :size 0.2 :ttl 2))
;;   `(("^\\*compilation" :size=0.2 :ttl 0))) ;; doesn't kill *compilation*
;; .. but this does; the 'compilation-finish-function runs after 'compile

;; ;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
;; (setq compilation-finish-function
;; ;; (add-hook! 'compilation-finish-functions  <-- don't know why doom-emacs doesn't like this
;;   (lambda (buf str)
;;     (if (null (string-match ".*exited abnormally.*" str))
;;         ;;no errors, make the compilation window go away in a few seconds
;;         (progn
;;           (run-at-time
;;            "1 sec" nil 'delete-windows-on
;;            (get-buffer-create buf))
;;           (message "No Compilation Errors!")))))

;; ;;

;; and when losing focus, enter normal mode go ahead and save
(add-hook! '(doom-switch-window-hook
             doom-switch-buffer-hook)
  ;; (evil-normal-state t) ;; this breaks company's popup window, so disable
  (save-some-buffers t))

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
