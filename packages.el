;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el


; ... and while we're at it, let's unpin the ~markdown-mode~ package since it's updated more frequently I'm curious if it gets better each time...
; I might get rid of this part, given that the package is stable and has been out since 2007, but it's still updated and my focus is on markdown at the moment.
(unpin! (:lang markdown))


(package! org-ql)

(package! math-preview :recipe (:host gitlab :repo "matsievskiysv/math-preview"))


;; ; I have never used  jk to ESCape in my life. I didn't know doom included it...
(package! evil-escape :disable t)

