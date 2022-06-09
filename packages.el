;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)


(package! math-preview :recipe (:host gitlab :repo "matsievskiysv/math-preview"))

;; (package! org-reveal :recipe (:host github :repo "yjwen/org-reveal"))
;; (package! ox-twsb :recipe (:host github :repo "marsmining/ox-twbs"))
;; (package! auctex)
;;
(package! ox-twbs)

;; (package! org-roam
;;    :recipe (:host github :repo "org-roam/org-roam" :branch "v2"))

;; (package! org-rifle
;;   :recipe (:host github :repo "alphapapa/org-rifle"))

;; (package! org-web-tools
;;   :recipe (:host github :repo "alphapapa/org-web-tools"))

;; (package! power-mode
;;   :recipe (:host github :repo "elizagamedev/power-mode.el"))
;;
;; (package! j-emacs-everywhere
;;   :recipe (:host github :repo "jmhammond/emacs-everywhere"))

(package! org-ref)
(package! org-re-reveal-ref)

;; ; I have never used  jk to ESCape in my life. I didn't know doom included it...
(package! evil-escape :disable t)

;(package! modus-themes :pin "59422c05e00d65582a005ccb06c3767622d14e03")
;; (package! alpha-org ;; /u/alphapapa's super org starter kit
;;   :recipe (:host github :repo "alphapapa/alpha-org"))
;(package! org-ref :recipe (:host github :repo "fuxialexander/org-ref" :files ("*")))

; (package! evil-motion-trainer :recipe (:host github :repo "martinbaillie/evil-motion-trainer"))
