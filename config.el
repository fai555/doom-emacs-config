;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.





(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (setq doom-theme 'doom-moonlight)

(after! treemacs
  (treemacs-follow-mode 1))




(when (eq system-type 'darwin)
  (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12 :weight 'light))
  (setq doom-big-font (font-spec :family "Iosevka Nerd Font Mono" :size 15 :weight 'light))
  (setq doom-big-font-increment 3)
  (setq doom-unicode-font (font-spec :family "Iosevka Nerd Font Mono" :size 12  :weight 'light))
  (setq doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font Mono" :size 12 :weight 'light))
  (font-put doom-font :weight 'regular))



(setq org-directory "/Volumes/base/cerebro/Dropbox/Notes/Orgzly")

(after! org (setq org-hide-emphasis-markers t))

;; https://emacstil.com/til/2021/09/19/org-mermaid/
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
  (set-face-attribute face nil :weight 'regular :height 1.0)))

(add-hook 'org-mode-hook #'my/org-mode-hook)


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
          ("f" "FAANG Prep" entry
           (file "faang-prep.org")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :kill-buffer t)
          ("c" "Command Archive" entry
           (file "command-archive.org")
           "* %?\n#+begin_src bash :result output\n\n#+end_src\n"
           :kill-buffer t)
          ("o" "Office Notes" entry
           (file "office-notes.org")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :kill-buffer t)
          )))

;; https://github.com/bastibe/org-journal#journal-file-content
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`yearly "#+TITLE: Yearly Journal\n"))))

(setq org-journal-file-header 'org-journal-file-header-func)

;; https://www.youtube.com/watch?v=i-nGmSQ5fh0
(setq org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "Journal_%Y.org"
      org-journal-file-type 'yearly)

(use-package! evil-escape
  :init
  (setq evil-escape-key-sequence "jj")
  (setq-default evil-escape-delay 0.2)
)


(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
;; ;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)


(use-package centered-cursor-mode
  :demand
  :hook (prog-mode . centered-cursor-mode)
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  ;; (global-centered-cursor-mode)
  (setq centered-cursor-mode t)
  )

;; https://github.com/doomemacs/doomemacs/issues/870#issuecomment-419455026
(setq display-line-numbers-type nil)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)



(use-package company
  :custom
  (company-idle-delay nil) ;; turn off auto-completion
  :general
  (:keymap 'company-mode-map
           "<tab>" 'company-complete) ;; keybinding to trigger company completion
  :hook
  (prog-mode . company-mode)
  :config
  ;; the following stops company from using the orderless completion style
  ;; makes company much more useful
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fun args))))



(add-hook 'shell-mode-hook (lambda () (goto-address-mode )))
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(use-package company-tabnine :ensure t)

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
)
;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)


(use-package jest
  :after (js2-mode)
  :hook (js2-mode . jest-minor-mode))

(setq org-superstar-headline-bullets-list '("❱" "❱" "❱" "❱" "❱" "❱"))
