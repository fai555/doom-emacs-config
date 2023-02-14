;; -*- lexical-binding: t -*-

(require 'awqat)
(setq calendar-latitude 52.520008
      calendar-longitude 13.404954)
(setq awqat-asr-hanafi nil)
(setq awqat-fajr-angle -18.0)
(setq awqat-isha-angle -16.0)



(use-package! awqat
  :commands (awqat-display-prayer-time-mode awqat-times-for-day)
  :config
  ;; Make sure `calendar-latitude' and `calendar-longitude' are set,
  ;; otherwise, set them here.
  (setq awqat-asr-hanafi nil
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) ")
  (awqat-set-preset-french-muslims))

(setq doom-theme 'doom-moonlight)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 )
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 ))

(setq! doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 ))

(set-fontset-font t 'arabic "Noto Naskh Arabic")

(setq org-superstar-headline-bullets-list '("❱" "❱" "❱" "❱" "❱" "❱"))
;; (setq org-superstar-headline-bullets-list '("⦿" "▶" "⦿" "▶" "⦿" "▶"))

(use-package org-bullets
  :hook (( org-mode ) . org-bullets-mode))


(setq org-directory "/Volumes/base/cerebro/Dropbox/Notes/Orgzly")

(after! org (setq org-hide-emphasis-markers t))

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
          ("f" "FAANG Prep" entry
           (file "faang-prep.org")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :kill-buffer t)
          ("c" "Command Archive" entry
           (file "command-archive.org")
           "* %?\n#+begin_src bash :result output\n\n#+end_src\n"
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
(setq
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "Journal_%Y.org"
      org-journal-file-type 'yearly
      )


(use-package! org
  :config
  (setq org-highlight-links
        '(bracket angle plain tag date footnote))
  ;; Setup custom links
  (+org-init-custom-links-h))


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Appearance
   org-modern-radio-target    '("❰" t "❱")
   org-modern-internal-target '("↪ " t "")
   org-modern-todo nil
   org-modern-tag nil
   org-ellipsis "↵"
   org-modern-timestamp t
   org-modern-star nil
   org-modern-statistics nil
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule "──────────"
   org-modern-keyword "▶"
   org-modern-list '((43 . "•")
                     (45 . "–")
                     (42 . "∘")))

  )



;; (use-package! svg-tag-mode
;;   :config
;;   (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
;;   (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
;;   (defconst day-re "[A-Za-z]\\{3\\}")
;;   (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

;;   (defun svg-progress-percent (value)
;;     (svg-image (svg-lib-concat
;;                 (svg-lib-progress-bar
;;                  (/ (string-to-number value) 100.0) nil
;;                  :height 0.8 :background (doom-color 'bg)
;;                  :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                 (svg-lib-tag (concat value "%") nil
;;                              :height 0.8 :background (doom-color 'bg)
;;                              :stroke 0 :margin 0)) :ascent 'center))

;;   (defun svg-progress-count (value)
;;     (let* ((seq (mapcar #'string-to-number (split-string value "/")))
;;            (count (float (car seq)))
;;            (total (float (cadr seq))))
;;       (svg-image (svg-lib-concat
;;                   (svg-lib-progress-bar (/ count total) nil
;;                                         :background (doom-color 'bg) :height 0.8
;;                                         :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                   (svg-lib-tag value nil
;;                                :background (doom-color 'bg)
;;                                :stroke 0 :margin 0 :height 0.8)) :ascent 'center)))

;;   (set-face-attribute 'svg-tag-default-face nil :family "Alegreya Sans")
;;   (setq svg-tag-tags
;;         `(;; Progress e.g. [63%] or [10/15]
;;           ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;;                                               (svg-progress-percent (substring tag 1 -2)))))
;;           ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;;                                             (svg-progress-count (substring tag 1 -1)))))
;;           ;; Task priority e.g. [#A], [#B], or [#C]
;;           ("\\[#A\\]" . ((lambda (tag) (svg-tag-make tag :face 'error :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
;;           ("\\[#B\\]" . ((lambda (tag) (svg-tag-make tag :face 'warning :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
;;           ("\\[#C\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
;;           ;; Keywords
;;           ("TODO" . ((lambda (tag) (svg-tag-make tag :inverse t :height .85 :face 'org-todo))))
;;           ("HOLD" . ((lambda (tag) (svg-tag-make tag :height .85 :face 'org-todo))))
;;           ("DONE\\|STOP" . ((lambda (tag) (svg-tag-make tag :inverse t :height .85 :face 'org-done))))
;;           ("NEXT\\|WAIT" . ((lambda (tag) (svg-tag-make tag :inverse t :height .85 :face '+org-todo-active))))
;;           ("REPEAT\\|EVENT\\|PROJ\\|IDEA" .
;;            ((lambda (tag) (svg-tag-make tag :inverse t :height .85 :face '+org-todo-project))))
;;           ("REVIEW" . ((lambda (tag) (svg-tag-make tag :inverse t :height .85 :face '+org-todo-onhold))))))

;;   :hook (org-mode . svg-tag-mode)
;;   )



(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks      t))

(use-package! evil-escape
  :init
  (setq evil-escape-key-sequence "jj")
  (setq-default evil-escape-delay 0.2)
)

;; (setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; ;; https://github.com/seagle0128/doom-modeline/issues/189#issuecomment-507210875
;; (setq doom-modeline-height 1.5)
;; (set-face-attribute 'mode-line nil :height 200)
;; (set-face-attribute 'mode-line-inactive nil :height 200)

;; ;; The maximum displayed length of the branch name of version control.
;; (setq doom-modeline-vcs-max-length 19)

(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
;; ;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)

;; ;; Whether display the environment version.
;; (setq doom-modeline-env-vercion t)

(require 'ob-async)

;; these are the defaults (before I changed them)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

(setq all-the-icons-scale-factor 0.8)

;; global beacon minor-mode
(use-package! beacon)
(after! beacon (beacon-mode 1))

(use-package! focus)

(use-package treemacs-projectile
  :after (treemacs projectile))

(after! (treemacs projectile)
  (treemacs-project-follow-mode 1))

(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))


(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; https://github.com/doomemacs/doomemacs/issues/870#issuecomment-419455026
(setq display-line-numbers-type nil)



(add-to-list 'default-frame-alist '(undecorated . t))

(use-package all-the-icons
  :ensure t)

(add-hook 'org-mode-hook 'org-appear-mode)

;; (add-to-list 'default-frame-alist '(alpha . 95))


;; https://hieuphay.com/doom-emacs-config/

;; Start Doom fullscreen
(add-to-list 'default-frame-alist '(width . 92))
(add-to-list 'default-frame-alist '(height . 35))

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))



(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.10))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))



(use-package! verb
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
;;
;; https://github.com/doomemacs/doomemacs/issues/2217
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

(setq which-key-idle-delay 0.5 ;; Default is 1.0
      which-key-idle-secondary-delay 0.05) ;; Default is nil



;; (setq which-key-allow-multiple-replacements t)

;; (after! which-key
;;   (pushnew! which-key-replacement-alist
;;             '((""       . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅔 \\1"))
;;             '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)")       . (nil . "Ⓔ
;;             \\1"))))



(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)


(setq company-global-modes
      '(not erc-mode
            circe-mode
            message-mode
            help-mode
            gud-mode
            vterm-mode))

(setq org-agenda-include-diary t)

(setq magit-ediff-dwim-show-on-hunks t)

(use-package! grammarly
  :config
  (grammarly-load-from-authinfo))

(use-package! lsp-grammarly
  :commands (+lsp-grammarly-load +lsp-grammarly-toggle)
  :init
  (defun +lsp-grammarly-load ()
    "Load Grammarly LSP server for LSP Mode."
    (interactive)
    (require 'lsp-grammarly)
    (lsp-deferred)) ;; or (lsp)

  (defun +lsp-grammarly-enabled-p ()
    (not (member 'grammarly-ls lsp-disabled-clients)))

  (defun +lsp-grammarly-enable ()
    "Enable Grammarly LSP."
    (interactive)
    (when (not (+lsp-grammarly-enabled-p))
      (setq lsp-disabled-clients (remove 'grammarly-ls lsp-disabled-clients))
      (message "Enabled grammarly-ls"))
    (+lsp-grammarly-load))

  (defun +lsp-grammarly-disable ()
    "Disable Grammarly LSP."
    (interactive)
    (when (+lsp-grammarly-enabled-p)
      (add-to-list 'lsp-disabled-clients 'grammarly-ls)
      (lsp-disconnect)
      (message "Disabled grammarly-ls")))

  (defun +lsp-grammarly-toggle ()
    "Enable/disable Grammarly LSP."
    (interactive)
    (if (+lsp-grammarly-enabled-p)
        (+lsp-grammarly-disable)
      (+lsp-grammarly-enable)))

  (after! lsp-mode
    ;; Disable by default
    (add-to-list 'lsp-disabled-clients 'grammarly-ls))

  :config
  (set-lsp-priority! 'grammarly-ls 1))


(setq org-export-headline-levels 5)


(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))

;; Powerline Config

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
        powerline-height 28
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

                     ;; Uncomment below line to bring back file encoding
                     ;; (powerline-raw (format " %s " buffer-file-coding-system) inner-face)

                     ;; Separator <
                     (funcall separator-right inner-face outer-face)

                     ;; ;; % location in file
                     ;; (powerline-raw "%3p" outer-face 'l)

                     ;; Current Line / File Size
                     (powerline-raw "%I" outer-face 'l)
                     ;; LN charachter
                     (powerline-raw (char-to-string airline-utf-glyph-linenumber) outer-face 'l)

                     ;; ;; Current Line / Number of lines
                     ;; (powerline-raw
                     ;;  (format "%%l/%d" (count-lines (point-min) (point-max))) outer-face 'l)

                     (powerline-raw "%l/%c " outer-face 'l)

                     ;; (powerline-raw "ln :" outer-face 'l)

                     ;; ;; Current Column
                     ;; (powerline-raw "%3c " outer-face 'l)

                     ;; ;; position in file image
                     ;; (when powerline-display-hud
                     ;;   (powerline-hud inner-face outer-face))
                     )
               ))
     ;; Combine Left and Right Hand Sides
     (concat (powerline-render lhs)
             (powerline-fill inner-face (powerline-width rhs))
             (powerline-render rhs))))

;; (defun airline-themes-set-modeline ()
;;   "Set the airline mode-line-format"
;;   (interactive)
;;   (setq-default mode-line-format
;;                 `("%e"
;;                   (:eval
;;                    ,(airline-themes-mode-line-format)
;;                    )))
;;   (powerline-reset)
;;   (kill-local-variable 'mode-line-format))

;; (airline-themes-set-modeline)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)
