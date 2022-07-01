(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Volumes/base/cerebro/notes")

(after! org (setq org-hide-emphasis-markers t))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq magit-ediff-dwim-show-on-hunks t)


(setq! doom-unicode-font (font-spec :family "MesloLGS NF" :size 12))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

(setq org-superstar-headline-bullets-list '("⦿" "⨠" "▶" "⁖" "✢" "❊"))

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
