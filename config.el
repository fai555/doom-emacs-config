;; -*- lexical-binding: t -*-

(require 'awqat)
(setq calendar-latitude 52.520008 calendar-longitude 13.404954)
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

(when (eq system-type 'darwin)
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 ))
  (setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 17))
  (setq doom-big-font-increment 3)
  (setq doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 ))
  (setq doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
  (font-put doom-font :weight 'regular))

(set-fontset-font t 'arabic "Noto Naskh Arabic")

(use-package! evil
  :init
  (setq evil-want-Y-yank-to-eol t))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(after! projectile
  (setq +workspaces-on-switch-project-behavior t)

  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
  (defun projectile-ignored-project-function (filepath)
      "Return t if FILEPATH is within any of `projectile-ignored-projects'"
      (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(after! dired

  (add-hook! 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook! 'dired-mode-hook 'hl-line-mode)

  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))

  (setq ls-lisp-dirs-first t)

  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (put 'dired-find-alternate-file 'disabled nil)

  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :after dired
  :config
    (map! :map dired-mode-map
      :n  "/" 'dired-narrow-fuzzy))

(use-package! dired-open
  :after dired
  :config
  (setq open-extensions
      '(("webm" . "mpv")
        ("avi" . "mpv")
        ("mp3" . "mpv")
        ("mp4" . "mpv")
        ("m4a" . "mpv")
        ("mkv" . "mpv")
        ("ogv" . "mpv")
        ("pdf" . "zathura")))
    (setq dired-open-extensions open-extensions))

(use-package! focus)

(use-package! markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.pmd$" . markdown-mode)
         ("\\.cbmd$" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(setq org-superstar-headline-bullets-list '("❱" "❱" "❱" "❱" "❱" "❱"))
;; (setq org-superstar-headline-bullets-list '("⦿" "▶" "⦿" "▶" "⦿" "▶"))

(use-package org-bullets
  :hook (( org-mode ) . org-bullets-mode))

;; (setq org-directory "/Volumes/base/cerebro/Dropbox/Notes/Orgzly")
(setq org-directory "~/Dropbox/Notes/Orgzly")

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
(setq org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "Journal_%Y.org"
      org-journal-file-type 'yearly)

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
   org-fold-catch-invisible-edits 'show-and-error
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

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

(require 'ob-async)

;; these are the defaults (before I changed them)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

(setq all-the-icons-scale-factor 0.8)


(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

(add-to-list 'default-frame-alist '(undecorated . t))

(use-package all-the-icons
  :ensure t)

(add-hook 'org-mode-hook 'org-appear-mode)

;; https://github.com/doomemacs/doomemacs/issues/2217
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


(setq which-key-idle-delay 0.5 ;; Default is 1.0
      which-key-idle-secondary-delay 0.05) ;; Default is nil

(setq company-global-modes
      '(not erc-mode
            circe-mode
            message-mode
            help-mode
            gud-mode
            vterm-mode))

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

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; https://github.com/doomemacs/doomemacs/issues/870#issuecomment-419455026
(setq display-line-numbers-type nil)

(add-to-list 'default-frame-alist '(alpha . 85))

;; https://hieuphay.com/doom-emacs-config/

;; Start Doom fullscreen
;; (add-to-list 'default-frame-alist '(width . 92))
;; (add-to-list 'default-frame-alist '(height . 35))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-hook 'window-setup-hook #'toogle-frame-maximized)

(use-package! verb
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (http . t)))

(setq org-confirm-babel-evaluate nil)

(defun org-babel-execute:json (body params)
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.20))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

;; Commented out since it was showing the below warning and
;; I don't have time to debug it

;; ~/.emacs.d/.local/straight/build-29.0.60/vlf/vlf-ediff.elc: Warning: Use
;;     ‘with-current-buffer’ rather than save-excursion+set-buffer
;; ~/.emacs.d/.local/straight/build-29.0.60/vlf/vlf-ediff.elc: Warning: Use
;;     ‘with-current-buffer’ rather than save-excursion+set-buffer

;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; global beacon minor-mode
(use-package! beacon)
(after! beacon (beacon-mode 1))

(use-package treemacs-projectile
  :after (treemacs projectile))

(after! (treemacs projectile)
  (treemacs-project-follow-mode 1))

(setenv "JAVA_HOME"
        "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home")
;; ;; switch java
;; ;;
;; (setq JAVA_BASE "/Users/fshourove/Library/Java/JavaVirtualMachines")

;; ;;
;; ;; This function returns the list of installed
;; ;;
;; (defun switch-java--versions ()
;;   "Return the list of installed JDK."
;;   (seq-remove
;;    (lambda (a) (or (equal a ".") (equal a "..")))
;;    (directory-files JAVA_BASE)))


;; (defun switch-java--save-env ()
;;   "Store original PATH and JAVA_HOME."
;;   (when (not (boundp 'SW_JAVA_PATH))
;;     (setq SW_JAVA_PATH (getenv "PATH")))
;;   (when (not (boundp 'SW_JAVA_HOME))
;;     (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))


;; (defun switch-java ()
;;   "List the installed JDKs and enable to switch the JDK in use."
;;   (interactive)
;;   ;; store original PATH and JAVA_HOME
;;   (switch-java--save-env)

;;   (let ((ver (completing-read
;;               "Which Java: "
;;               (seq-map-indexed
;;                (lambda (e i) (list e i)) (switch-java--versions))
;;               nil t "")))
;;     ;; switch java version
;;     (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver "/Contents/Home"))
;;     (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
;;                            ":" SW_JAVA_PATH)))
;;   ;; show version
;;   (switch-java-which-version?))


;; (defun switch-java-default ()
;;   "Restore the default Java version."
;;   (interactive)
;;   ;; store original PATH and JAVA_HOME
;;   (switch-java--save-env)

;;   ;; switch java version
;;   (setenv "JAVA_HOME" SW_JAVA_HOME)
;;   (setenv "PATH" SW_JAVA_PATH)
;;   ;; show version
;;   (switch-java-which-version?))


;; (defun switch-java-which-version? ()
;;   "Display the current version selected Java version."
;;   (interactive)
;;   ;; displays current java version
;;   (message (concat "Java HOME: " (getenv "JAVA_HOME"))))

;; (add-to-list 'exec-path "~/kotlin-language-server")
;; (add-to-list 'exec-path "~/kotlin-language-server/server/build/distributions/server-1.3.2")

;; (setenv "JAVA_HOME" "<java home path>")

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package! typescript-mode
  :mode ("\\.tsx\\'" . typescript-tsx-tree-sitter-mode)
  :config
  (setq typescript-indent-level 2)

  (define-derived-mode typescript-tsx-tree-sitter-mode typescript-mode "TypeScript TSX"
    (setq-local indent-line-function 'rjsx-indent-line))

  (add-hook! 'typescript-tsx-tree-sitter-mode-local-vars-hook
             #'+javascript-init-lsp-or-tide-maybe-h
             #'rjsx-minor-mode)
  (map! :map typescript-tsx-tree-sitter-mode-map
        "<" 'rjsx-electric-lt
        ">" 'rjsx-electric-gt))

(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-tree-sitter-mode . tsx)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)




(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)


;; if you use typescript-mode
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(setq highlight-indent-guides-delay 0)
(setq highlight-indent-guides-responsive 'top)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; keep the cursor centered to avoid sudden scroll jumps
(require 'centered-cursor-mode)

;; disable in terminal modes
;; http://stackoverflow.com/a/6849467/519736
;; also disable in Info mode, because it breaks going back with the backspace key
(define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'Info-mode 'vterm-mode 'term-mode 'eshell-mode 'shell-mode 'erc-mode)))
      (centered-cursor-mode))))
(my-global-centered-cursor-mode 1)
;; (use-package centered-cursor-mode
;;   :demand
;;   :config
;;   ;; Optional, enables centered-cursor-mode in all buffers.
;;   (global-centered-cursor-mode))

(global-set-key (kbd "M-o") 'ace-window)
