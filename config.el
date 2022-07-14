(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)


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

;; https://github.com/seagle0128/doom-modeline/issues/189#issuecomment-507210875
;; (setq doom-modeline-height 1.5)
;; (set-face-attribute 'mode-line nil :height 200)
;; (set-face-attribute 'mode-line-inactive nil :height 200)

;; The maximum displayed length of the branch name of version control.
;; (setq doom-modeline-vcs-max-length 19)

;; Whether display the workspace name. Non-nil to display in the mode-line.
;; (setq doom-modeline-workspace-name t)

;; Whether display the environment version.
;; (setq doom-modeline-env-vercion t)

(setq all-the-icons-scale-factor 0.8)


;; https://www.reddit.com/r/emacs/comments/k4zavc/powerline_doom_emacs/
;; (require 'powerline)



;; (diminish 'projectile-mode)
;; (require 'diminish)
;; (setq powerline-default-separator 'slant)




(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator 'arrow
        powerline-default-separator-dir (quote (left . right))
        powerline-height 38
        powerline-display-buffer-size nil
        powerline-display-hud nil
        powerline-display-mule-info nil
        powerline-gui-use-vcs-glyph t
        powerline-inactive1 '((t (:background "grey11" :foreground "#c5c8c6")))
        powerline-inactive2 '((t (:background "grey20" :foreground "#c5c8c6")))))




(require 'airline-themes)

(defun airline-themes-mode-line-format ()
  '(let* ((current-window-width (window-width))
          (active (powerline-selected-window-active))
          (separator-left (intern (format "powerline-%s-%s"
                                          (powerline-current-separator)
                                          (car powerline-default-separator-dir))))
          (separator-right (intern (format "powerline-%s-%s"
                                           (powerline-current-separator)
                                           (cdr powerline-default-separator-dir))))
          (mode-line-face (if active 'mode-line 'mode-line-inactive))
          (evil-mode-active (featurep 'evil))
          (visual-block (if evil-mode-active
                            (and (evil-visual-state-p)
                                 (eq evil-visual-selection 'block))
                          nil))
          (visual-line (if evil-mode-active
                           (and (evil-visual-state-p)
                                (eq evil-visual-selection 'line))
                         nil))
          (current-evil-state-string (if evil-mode-active
                                         (upcase (concat (symbol-name evil-state)
                                                         (cond (visual-block "-BLOCK")
                                                               (visual-line "-LINE"))))
                                       nil))
          ;; Shorten evil state to a single charater instead of the full word
          (current-evil-state-string (if (and current-evil-state-string
                                              (< current-window-width 80))
                                         (substring current-evil-state-string 0 1)
                                       current-evil-state-string))
          (outer-face
           (if active
               (if evil-mode-active
                   (cond ((eq evil-state (intern "normal"))  'airline-normal-outer)
                         ((eq evil-state (intern "insert"))  'airline-insert-outer)
                         ((eq evil-state (intern "visual"))  'airline-visual-outer)
                         ((eq evil-state (intern "replace")) 'airline-replace-outer)
                         ((eq evil-state (intern "emacs"))   'airline-emacs-outer)
                         (t                                  'airline-normal-outer))
                 'airline-normal-outer)
             'powerline-inactive1))

          (inner-face
           (if active
               (if evil-mode-active
                   (cond ((eq evil-state (intern "normal")) 'airline-normal-inner)
                         ((eq evil-state (intern "insert")) 'airline-insert-inner)
                         ((eq evil-state (intern "visual")) 'airline-visual-inner)
                         ((eq evil-state (intern "replace")) 'airline-replace-inner)
                         ((eq evil-state (intern "emacs"))   'airline-emacs-inner)
                         (t                                 'airline-normal-inner))
                 'airline-normal-inner)
             'powerline-inactive2))

          (center-face
           (if active
               (if evil-mode-active
                   (cond ((eq evil-state (intern "normal")) 'airline-normal-center)
                         ((eq evil-state (intern "insert")) 'airline-insert-center)
                         ((eq evil-state (intern "visual")) 'airline-visual-center)
                         ((eq evil-state (intern "replace")) 'airline-replace-center)
                         ((eq evil-state (intern "emacs"))   'airline-emacs-center)
                         (t                                 'airline-normal-center))
                 'airline-normal-center)
             'airline-inactive3))

          ;; Left Hand Side
          (lhs-mode (when (or (not airline-hide-state-on-inactive-buffers)
                              (and airline-hide-state-on-inactive-buffers active))
                      (if evil-mode-active
                          (list
                           ;; Evil Mode Name
                           (powerline-raw (concat " " current-evil-state-string " ") outer-face)
                           (funcall separator-left outer-face inner-face)
                           ;; Modified string
                           (powerline-raw "%*" inner-face 'l))
                        (list
                         ;; Modified string
                         (powerline-raw "%*" outer-face 'l)
                         ;; Separator >
                         (powerline-raw " " outer-face)
                         (funcall separator-left outer-face inner-face)))))

          (lhs-rest (list
                     ;; ;; Separator >
                     ;; (powerline-raw (char-to-string #x2b81) inner-face 'l)

                     ;; Eyebrowse current tab/window config
                     (if (and (or (not airline-hide-eyebrowse-on-inactive-buffers)
                                  (and airline-hide-eyebrowse-on-inactive-buffers active))
                              (featurep 'eyebrowse))
                         (powerline-raw (concat " " (eyebrowse-mode-line-indicator)) inner-face 'r))

                     ;; Git Branch
                     (if (and (or (not airline-hide-vc-branch-on-inactive-buffers)
                                  (and airline-hide-vc-branch-on-inactive-buffers active))
                              buffer-file-name vc-mode)
                         (powerline-raw (airline-get-vc) inner-face))

                     ;; Separator >
                     (powerline-raw " " inner-face)
                     (funcall separator-left inner-face outer-face )

                     ;; Directory
                     (cond
                      ((and buffer-file-name ;; if buffer has a filename
                            (eq airline-display-directory
                                'airline-directory-shortened))
                       (powerline-raw (airline-shorten-directory default-directory airline-shortened-directory-length) outer-face 'l))
                      ((and buffer-file-name ;; if buffer has a filename
                            (eq airline-display-directory
                                'airline-directory-full))
                       (powerline-raw default-directory outer-face 'l))
                      (t
                       (powerline-raw " " outer-face)))

                     ;; Buffer ID
                     ;; (powerline-buffer-id center-face)
                     (powerline-raw "%b" outer-face)

                     ;; Current Function (which-function-mode)
                     (when (and (boundp 'which-func-mode) which-func-mode)
                       ;; (powerline-raw which-func-format 'l nil))
                       (powerline-raw which-func-format center-face 'l))

                     ;; ;; Separator >
                     ;; (powerline-raw " " center-face)
                     ;; (funcall separator-left mode-line face1)

                     (when (boundp 'erc-modified-channels-object)
                       (powerline-raw erc-modified-channels-object center-face 'l))

                     ;; ;; Separator <
                     ;; (powerline-raw " " face1)
                     ;; (funcall separator-right face1 face2)
                     (funcall separator-left outer-face inner-face )
                     ))

          (lhs (append lhs-mode lhs-rest))

          ;; Right Hand Side
          (rhs (list (powerline-raw global-mode-string inner-face 'r)

                     ;; ;; Separator <
                     ;; (powerline-raw (char-to-string #x2b83) center-face 'l)

                     ;; Minor Modes
                     ;; (powerline-minor-modes center-face 'l)
                     ;; (powerline-narrow center-face 'l)

                     ;; Subseparator <
                     (funcall separator-right inner-face outer-face )

                     ;; Major Mode
                     (powerline-major-mode outer-face 'l)
                     (powerline-process outer-face)

                     ;; Separator <
                     (powerline-raw " " outer-face)
                     (funcall separator-right outer-face inner-face)

                     ;; ;; Buffer Size
                     ;; (when powerline-display-buffer-size
                     ;;   (powerline-buffer-size inner-face 'l))
                     ;; ;; Mule Info
                     ;; (when powerline-display-mule-info
                     ;;   (powerline-raw mode-line-mule-info inner-face 'l))
                     ;; (powerline-raw " " inner-face)
                     (powerline-raw (format " %s " buffer-file-coding-system) inner-face)

                     ;; Separator <
                     (funcall separator-right inner-face outer-face)

                     ;; % location in file
                     (powerline-raw "%3p" outer-face 'l)
                     ;; LN charachter
                     (powerline-raw (char-to-string airline-utf-glyph-linenumber) outer-face 'l)

                     ;; Current Line / File Size
                     ;; (powerline-raw "%l/%I" outer-face 'l)
                     ;; Current Line / Number of lines
                     (powerline-raw
                      (format "%%l/%d" (count-lines (point-min) (point-max))) outer-face 'l)

                     ;; (powerline-raw "ln :" outer-face 'l)

                     ;; Current Column
                     (powerline-raw "%3c " outer-face 'l)

                     ;; ;; position in file image
                     ;; (when powerline-display-hud
                     ;;   (powerline-hud inner-face outer-face))
                     )
               ))
     ;; Combine Left and Right Hand Sides
     (concat (powerline-render lhs)
             (powerline-fill inner-face (powerline-width rhs))
             (powerline-render rhs))))

(defun airline-themes-set-modeline ()
  "Set the airline mode-line-format"
  (interactive)
  (setq-default mode-line-format
                `("%e"
                  (:eval
                   ,(airline-themes-mode-line-format)
                   )))
  (powerline-reset)
  (kill-local-variable 'mode-line-format))

(airline-themes-set-modeline)

(setq doom-theme 'doom-one)
