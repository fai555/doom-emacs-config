;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! ob-mermaid)
(package! org-bullets)
(package! org-superstar)

;; https://www.reddit.com/r/emacs/comments/k4zavc/powerline_doom_emacs/
(package! powerline)
(package! airline-themes)

;; https://ruivieira.dev/doom-emacs.html#2QrAfe_5b_:0.H7O31EP5n:1~sesesseesesssesesesesesseessesesesesessesessessesesesssesessssessesesessss~m~n
(package! beacon)
(package! focus)

;; org roam stuff
;; https://github.com/org-roam/org-roam-ui#doom

(unpin! org-roam)
(package! org-roam-ui)


(package! mu4e-dashboard
  :recipe (:host github
           :repo "rougier/mu4e-dashboard"))

(package! mu4e-thread-folding
  :recipe (:host github
           :repo "rougier/mu4e-thread-folding"))

(package! doom-snippets
  :recipe (:host github
           :repo "doomemacs/snippets" :files ("*.el" "*")))

(package! awqat
  :recipe (:host github
           :repo "zkry/awqat"))


(package! vlf :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c")


(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly")
  :pin "e47b370faace9ca081db0b87ae3bcfd73212c56d")

(package! lsp-grammarly
  :disable (or (not (modulep! :tools lsp)) (modulep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/lsp-grammarly")
  :pin "eab5292037478c32e7d658fb5cba8b8fb6d72a7c")


(package! ranger)
(package! dired-narrow)
(package! dired-open)
(package! verb)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! org-modern)

(package! rainbow-delimiters)
(package! centered-cursor-mode)

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")

(package! org-download)
