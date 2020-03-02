;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)


;; (package! org-reveal :recipe (:host github :repo "yjwen/org-reveal"))
;; (package! ox-twsb :recipe (:host github :repo "marsmining/ox-twbs"))
;; (package! auctex)
;;
(package! ox-twbs)

;; Setup this package.
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam" :branch "develop"))
