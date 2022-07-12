(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Volumes/base/cerebro/Dropbox/Notes/Orgzly")

(after! org (setq org-hide-emphasis-markers t))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq magit-ediff-dwim-show-on-hunks t)


(setq! doom-unicode-font (font-spec :family "MesloLGS NF" ))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

(setq org-superstar-headline-bullets-list '("⦿" "⦿" "⦿" "⦿" "⦿"))

(use-package org-bullets
  :hook (( org-mode ) . org-bullets-mode))

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(use-package! evil-escape
  :init
  (setq evil-escape-key-sequence "jj")
  (setq-default evil-escape-delay 0.2)
)

(require 'ob-async)

;; these are the defaults (before I changed them)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3);;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
  (set-face-attribute face nil :weight 'light :height 1.0)))

(add-hook 'org-mode-hook #'my/org-mode-hook)

;; https://emacstil.com/til/2021/09/19/org-mermaid/
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

;; https://stackoverflow.com/questions/18582869/only-highlight-not-the-entire-heading-line-in-org-mode-emacs
;; (setq org-level-color-stars-only t)

;; https://orgmode.org/manual/Faces-for-TODO-keywords.html
;; (setq org-todo-keyword-faces '(("DONE" . (:foreground "green"))))


;; https://www.reddit.com/r/emacs/comments/evw0om/doom_emacs_newb_question/
(after! org
  (setq org-capture-templates
        '(("t" "Get Shit Done" entry
           (file "get-shit-done.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :kill-buffer t)
          ("n" "Tech Notes" entry
           (file "tech-notes.org")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :kill-buffer t)
          )))

;; https://www.youtube.com/watch?v=i-nGmSQ5fh0
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org"
      )
