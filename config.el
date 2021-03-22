;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "John Hammond"
      user-mail-address "jmhammond@gmail.com"

      ;; make *Scratch* act like org-mode
      doom-scratch-buffer-major-mode 'org-mode

      ;; ui
      ;; (cond
      ;;  ((string-equal system-name "penguin"))) ;;
        ;; if running in crouton/sommelier:
      doom-font (font-spec :family "Source Code Pro" :size 28)
      ;doom-theme 'doom-nord-light
      doom-theme 'doom-dark+
      ;;doom-theme 'doom-one-light
      +doom-dashboard-banner-file (expand-file-name "coffeesquirrel.png" doom-private-dir)

      ;; crostini doesn't let us do top/left, but the width/height works
      initial-frame-alist '((top . 1) (left . 1) (width . 100) (height . 40))

      ;; remove line numbers to speed up emacs
      display-line-numbers-type nil
      )

;; (after! evil-surround
;;   (defmacro define-and-bind-text-object (key start-regex end-regex)
;;     (let ((inner-name (make-symbol "inner-name"))
;;           (outer-name (make-symbol "outer-name")))
;;       `(progn
;;          (evil-define-text-object ,inner-name (count &optional beg end type)
;;            (evil-select-paren ,start-regex ,end-regex beg end type count nil))
;;          (evil-define-text-object ,outer-name (count &optional beg end type)
;;            (evil-select-paren ,start-regex ,end-regex beg end type count t))
;;          (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
;;          (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
;;                                         ; between dollar signs:
;;   (define-and-bind-text-object "$" "\\$" "\\$")
;;                                         ; between pipe characters:
;;   (define-and-bind-text-object "|" "|" "|"))

;; In crouton, I want to use the croutonurlhandler
;(setq browse-url-browser-function 'browse-url-generic
;     browse-url-generic-program "croutonurlhandler")

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

;; compile PreText documents via make
(defun my-make-compile ()
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

(global-evil-motion-trainer-mode 1)
(setq evil-motion-trainer-threshold 4)
; (setq evil-motion-trainer-super-annoying-mode t)


;; disable the company idle popup
;; work-around for org-tables and TAB
(setq company-idle-delay nil)

;; unmap tab from company and yas-snippets in insert mode:
(map! :map company-keymap "TAB" nil)
(map! :map yas-keymap "TAB" nil)

;; popups
(set-popup-rules!
  '(("^\\*Warnings" :size 0.2 :ttl 2))
  `(("^\\*compilation" :size=0.2 :ttl 0))) ;; doesn't kill *compilation*
;; .. but this does; the 'compilation-finish-function runs after 'compile

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
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

;; TODO change this back.
;; and when losing focus, enter normal mode go ahead and save
(add-hook! '(doom-switch-window-hook
             doom-switch-buffer-hook
             focus-out-hook) ; frames
  ;; (evil-normal-state t) ;; this breaks company's popup window, so disable
  (save-some-buffers t))

;; In case you forget, me, line-numbers are terrible for org files and emacs performance
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)


(map! :leader
;;      :desc "Org-roam capture to today"           "d"    #'org-roam-dailies-capture-today
      ;; prefer the unshifted semicolon for Ex commands
      ";" 'execute-extended-command
      ":" 'eval-expression)

;; this keymap is similar to the evil default "z i" for org-toggle-inline-images
(map! :mode org-mode :n "z p" 'org-toggle-latex-fragment)

(after! org

  ;; Don't delete hidden subtrees:
  (setq org-ctrl-k-protect-subtree t)

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

  ;; My attempt at capturing to the daily note
  (defun visit-the-daily-note()
    "Visit a new file named by the current timestamp"
    (interactive)
    (let* (
           (curr-date-stamp (format-time-string "%Y-%m-%d.org"))
           (file-name (expand-file-name curr-date-stamp "~/org/roam/")))
      (set-buffer (org-capture-target-buffer file-name))
      (goto-char (point-max))))

  (setq org-capture-templates '(("n" "Note" entry (function visit-the-daily-note)
                                 "* %?\n")))



  ;; Email link handlink for message:// links
  (setq thunderbird-program "/usr/bin/thunderbird")
  (defun org-message-thunderlink-open (slash-message-id)
    "Handler for org-link-set-parameters that converts a standard message:// link into
   a thunderlink and then invokes thunderbird."
    ;; remove any / at the start of slash-message-id to create real message-id
    (let ((message-id
           (replace-regexp-in-string (rx bos (* "/"))
                                     ""
                                     slash-message-id)))
      (start-process
       (concat "thunderlink: " message-id)
       nil
       thunderbird-program
       "-thunderlink"
       (concat "thunderlink://messageid=" message-id)
       )))
  ;; on message://aoeu link, this will call handler with //aoeu
  (org-link-set-parameters "message" :follow #'org-message-thunderlink-open)

  ;; here's the deal: I don't use org-agenda
  ;; ... I don't have capture templates that I want to use yet
  ;; ... let's dump all the copy-paste-from-the-internet
  ;; (setq org-capture-templates '(("t" "Todo [inbox]" entry
  ;;                              (file "~/org/inbox.org")
  ;;                              "* TODO %i%?" :prepend t)
  ;;                             ("l" "Todo [Linked to current line]" entry
  ;;                              (file+headline "~/org/inbox.org" "")
  ;;                              "* %i%? \n %a" :prepend t)
  ;;                             ("T" "Tickler" entry
  ;;                              (file+headline "~/org/tickler.org" "Tickler")
  ;;                              "* %i%? \n %U" :prepend t)
  ;;                             ("n" "Personal notes" entry
  ;;                              (file+headline "~/org/inbox.org" "")
  ;;                              "* %u %?\n%i\n%a" :prepend t)))
  ;; (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
  ;;                          ("~/org/someday.org" :level . 1)
  ;;                          ("~/org/tickler.org" :maxlevel . 2)))
  ;; (setq org-agenda-files '("~/org/inbox.org"
  ;;                          "~/org/gtd.org"
  ;;                          "~/org/tickler.org"))
  ;; (setq org-agenda-custom-commands
  ;;     '(("o" "At the office" tags-todo "@office"
  ;;        ((org-agenda-overriding-header "Office")))
  ;;     ("c" "At the computer" tags-todo "@computer"
  ;;        ((org-agenda-overriding-header "Computer")))
  ;;     ("h" "At home" tags-todo "@home"
  ;;        ((org-agenda-overriding-header "Home")))
  ;;     ("e" "Errands / Buy List" tags-todo "@errands"
  ;;        ((org-agenda-overriding-header "Errands")))
  ;;     ("w" "Waiting For" ((todo "WAITING"))
  ;;        ((org-agenda-overriding-header "Waiting For")))
  ;;     ))

 )


;; here are the defaults from doom:
;; (("t" "Personal todo" entry
;;   (file+headline +org-capture-todo-file "Inbox")
;;   "* [ ] %?\n%i\n%a" :prepend t)
;;  ("n" "Personal notes" entry
;;   (file+headline +org-capture-notes-file "Inbox")
;;   "* %u %?\n%i\n%a" :prepend t)
;;  ("j" "Journal" entry
;;   (file+olp+datetree +org-capture-journal-file)
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("p" "Templates for projects")
;;  ("pt" "Project-local todo" entry
;;   (file+headline +org-capture-project-todo-file "Inbox")
;;   "* TODO %?\n%i\n%a" :prepend t)
;;  ("pn" "Project-local notes" entry
;;   (file+headline +org-capture-project-notes-file "Inbox")
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("pc" "Project-local changelog" entry
;;   (file+headline +org-capture-project-changelog-file "Unreleased")
;;   "* %U %?\n%i\n%a" :prepend t)
;;  ("o" "Centralized templates for projects")
;;  ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
;;  ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
;;  ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))

