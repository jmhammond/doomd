;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; ui
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 28))

(setq doom-theme 'doom-nord)

;; crostini doesn't let us do top/left, but the width/height works
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 100) (height . 40)))

;; coffee squirrrel for my emacs logo
(setq +doom-dashboard-banner-file (expand-file-name "coffeesquirrel.png" doom-private-dir))

;; soft wrap everywhere
(global-visual-line-mode 1)

;; nxml and smart parens
;; autoclose created too many > characters
(sp-local-pair 'nxml-mode "<" ">" :post-handlers '(("[d1]" "/")))

;; compile pretext documents via make
(defun my-make-compile ()
  (local-set-key (kbd "C-c C-c") 'recompile))
(add-hook 'nxml-mode-hook 'my-make-compile)

;; apparently this helps somehow
(setq undo-tree-enable-undo-in-region nil)

(map! :ne "M-/" #'comment-or-uncomment-region)

;; popups
(set-popup-rules!
  '(("^\\*Warnings" :size 0.2 :ttl 2))
  `(("^\\*compilation" :size=0.2 :ttl 0))) ;; doesn't kill *compilation*

;; .. but this does; the 'compilation-finish-function runs after 'compile
;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
(setq compilation-finish-function
;; (add-hook! 'compilation-finish-functions  <-- don't know why doom-emacs doesn't like this
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "1 sec" nil 'delete-windows-on
           (get-buffer-create buf))
          (message "No Compilation Errors!")))))
