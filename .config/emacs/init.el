;;; package --- Summary ; -*- lexical-binding: t; -*-

;;; Commentary:
"Well its merely my emacs config"

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setopt
    ;; use-package-compute-statistics t
    package-quickstart t
    use-package-always-defer t
    use-package-always-ensure t
    use-package-verbose t
    )

(use-package consult
    :after (flycheck)
    :bind
    ("M-f" . consult-flycheck)
    ;;("M-i" . consult-flyspell)
    ("M-s" . consult-line)
    ("M-/" . consult-fd)
    ("M-\\" . consult-ripgrep))

(use-package editorconfig
    :init (editorconfig-mode))

(use-package emacs
    :custom-face
    (default ((t (:foreground "#FAF0E6" :background "#000000"))))
    (diff-added ((t (:foreground "#2E8B57" :background "#000000"))))
    (diff-file-header ((t (:foreground "#FAF0E6" :background "#161A1F" :bold t))))
    (diff-indicator-added ((t (:foreground "#2E8B57" :background "#000000"))))
    (diff-indicator-removed ((t (:foreground "#8B0000" :background "#000000"))))
    (diff-refine-added ((t (:foreground "#000000" :background"#2E8B57"))))
    (diff-refine-removed ((t (:foreground "#000000" :background "#8B0000"))))
    (diff-removed ((t (:foreground "#8B0000" :background "#000000"))))
    (font-lock-comment-face ((t (:foreground "#666699"))))
    (font-lock-string-face ((t (:foreground "#008080"))))
    (font-lock-variable-name-face ((t (:foreground "#8B008B"))))
    (fringe ((t (:background "#000000"))))
    (highlight ((t (:background "#0D0D0D"))))
    (isearch ((t (:foreground "#000000" :background "#FF00FF"))))
    (isearch-fail ((t (:foreground "#000000" :background "yellow"))))
    (lazy-highlight ((t (:background "#003641"))))
    (region ((t (:background "#191970"))))
    (secondary-selection ((t (:background "#330099"))))
    (trailing-whitespace ((t (:background "#330099"))))
    (window-divider ((t (:foreground "#444444"))))
    :custom
    (auto-save-default t)
    (auto-save-no-message nil)
    (auto-save-visited-file-name t)
    (backup-each-save-remote-files t)
    (completion-ignore-case t)
    (create-lockfiles t)
    (custom-file (make-temp-file "emacs-custom"))
    (enable-recursive-minibuffers t)
    (font-lock-maximum-decoration t)
    (font-lock-maximum-size '262144)
    (gc-cons-threshold 100000000)
    (inhibit-default-init t)
    (inhibit-startup-screen t)
    (initial-scratch-message "")
    (left-fringe-width  10)
    (lpr-add-switches nil)
    (lpr-command "lp")
    (make-backup-files nil)
    (mode-line-format nil)
    (mouse-wheel-follow-mouse nil)
    (mouse-wheel-progressive-speed nil)
    (mouse-wheel-scroll-amount '(3 ((shift) . 1)))
    (mouse-yank-at-point t)
    (pop-up-frames t)
    (read-buffer-completion-ignore-case t)
    (read-file-name-completion-ignore-case t)
    (right-fringe-width  0)
    (scroll-conservatively 0)
    (scroll-down-aggressively 0.01)
    (scroll-margin 5)
    (scroll-up-aggressively 0.01)
    (show-trailing-whitespace t)
    (sort-fold-case nil)
    (use-short-answers t)
    (vc-make-backup-files t)
    :config
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)
    (put 'erase-buffer 'disabled nil)
    (defun display-startup-echo-area-message ()
        (message ""))
    (defun toggle-transparency ()
        "Toggle frame transparency between 90% and 100%."
        (interactive)
        (let ((alpha (frame-parameter nil 'alpha)))
            (set-frame-parameter
                nil 'alpha
                (if (eql (cond ((numberp alpha) alpha)
                             ((numberp (cdr alpha)) (cdr alpha))
                             ((numberp (cadr alpha)) (cadr alpha)))
                        100)
                    '(90 . 90) '(100 . 100)))))
    ;; auto-save-everything-all-the-time
    (defvar backup-each-save-mirror-location "~/.backups")
    (defvar backup-each-save-remote-files t)
    (defvar backup-each-save-time-format "%y%m%d%H%M%S")
    (defvar backup-each-save-filter-function 'identity
        "Function which should return non-nil if the file should be backed up.")
    (defvar backup-each-save-size-limit 500000
        "Maximum size of a file (in bytes, nil disables) that should be copied at
each savepoint.")
    (defun backup-each-save ()
        "auto-save-everything-all-the-time: versioned backups are created as a
 dir tree in `~/.backups` whenever a file is saved or before auto-saved, hence
 auto-safe writes to the actual file (after the backup) by setting
 auto-save-visited-file-name."
        (let ((bfn (buffer-file-name)))
            (when (and (or backup-each-save-remote-files
		           (not (file-remote-p bfn)))
	              (funcall backup-each-save-filter-function bfn)
	              (or (not backup-each-save-size-limit)
		          (<= (buffer-size) backup-each-save-size-limit)))
                (copy-file bfn (backup-each-save-compute-location bfn) t t t))))
    (defun backup-each-save-compute-location (filename)
        (let* ((containing-dir (file-name-directory filename))
	          (basename (file-name-base filename))
                  (extension (file-name-extension filename))
	          (backup-container
	              (format "%s/%s"
		          backup-each-save-mirror-location
		          containing-dir)))
            (when (not (file-exists-p backup-container))
                (make-directory backup-container t))
            (format "%s/%s~%s~.%s" backup-container basename
	        (format-time-string backup-each-save-time-format) extension)))
    :hook
    (before-save . delete-trailing-whitespace)
    (after-save . backup-each-save)
    (auto-save . backup-each-save)
    :bind
    ("M-<mouse-8>" . previous-buffer)
    ("M-<mouse-9>" . next-buffer)
    ("C-<right>" . next-buffer)
    ("C-<left>" . previous-buffer)
    ("M-<wheel-down>" . scroll-up)
    ("M-<wheel-up>" . scroll-down)
    ("C-c t". toggle-transparency)
    ("C-c r" . (lambda() (interactive) (load-file "~/.config/emacs/init.el")))
    )

(use-package flycheck
    :init (global-flycheck-mode))

(use-package gptel
    :custom
    (gptel-default-mode 'org-mode)
    ;; (gptel-api-key
    ;;     (auth-source-pick-first-password
    ;;         :host "OpenWebUI"
    ;;         :user "credential"))
    (gptel-api-key (getenv "OPENWEBUI_API_KEY"))
    (gptel-model 'glm-4.6-355b)
    (gptel-backend (gptel-make-openai "OpenWebUI"
                       :host "chat.ai.datalab.tuwien.ac.at"
                       :protocol "https"
                       :key 'gptel-api-key
                       :endpoint "/api/chat/completions"
                       :stream t
                       :models '(glm-4.6-355b glm-4.5v-106b mistral-small-3.2-24b)))
    :bind
    ("M-l" . gptel)
    ("C-<return>" . gptel-send))


(use-package highlight-thing
    :custom-face
    (hi-yellow ((t (:foreground "#FAF0E6" :background "#1A004E"))))
    :init (global-highlight-thing-mode))

(use-package jinx
    :custom
    (jinx-languages "de_AT de_DE en_GB en_US")
    ;;:hook (emacs-startup . global-jinx-mode)
    :bind
    ("M-j" . jinx-correct)
    ("C-M-j" . jinx-languages))

(use-package magit
    :custom-face
    (magit-diff-added ((t (:foreground "#2E8B57" :background "#000000"))))
    (magit-diff-removed ((t (:foreground "#8B0000" :background "#000000"))))
    :custom
    (magit-diff-refine-hunk 'all)
    (magit-diff-highlight-hunk-body nil)
    :bind ("M-g" . magit))

(use-package marginalia
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

(use-package markdown-mode
  :custom-face (markdown-code-face ((t (:inherit org-block)))))

(use-package octave
    :mode ("\\.m$" . octave-mode)
    :bind ("C-c C-c" . octave-send-region))

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '(file (styles partial-completion)))
    (completion-category-defaults nil)
    (completion-pcm-leading-wildcard t))

(use-package org
    :mode ("\\.org$" . org-mode)
    :custom
    (org-confirm-babel-evaluate nil)
    (org-ellipsis " ▼")
    (org-fontify-quote-and-verse-blocks t)
    (org-src-fontify-natively t)
    (org-startup-indented t)
    (org-table-header-line-p t)
    :config
    (electric-pair-mode)
    (org-babel-do-load-languages
        'org-babel-load-languages
        '((C . t)
             (emacs-lisp . t)
             (fortran . t)
             (gnuplot . t)
             (lua . t)
             (makefile . t)
             (org . t)
             (python . t)
             (shell . t)))
    (set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)
    (set-face-attribute 'org-code nil :background "#0b0d0f")
    (set-face-attribute 'org-level-1 nil :bold t :foreground "#FAF0E6" :background "#4682B4")
    (set-face-attribute 'org-level-2 nil :bold t)
    (set-face-attribute 'org-level-3 nil :bold t)
    (set-face-attribute 'org-level-4 nil :bold t)
    (set-face-attribute 'org-level-5 nil :bold t)
    (set-face-attribute 'org-level-6 nil :bold t)
    (set-face-attribute 'org-level-7 nil :bold t)
    (set-face-attribute 'org-level-8 nil :bold t)
    (set-face-attribute 'org-meta-line nil :bold t)
    (set-face-attribute 'org-quote nil :italic t)
    (set-face-attribute 'org-table nil :background "#0b0d0f")
    (set-face-attribute 'org-table-header nil :foreground "#787787" :background "#161a1f" :bold t)
    (set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
    (set-face-attribute 'org-block nil :background "#0b0d0f")
    (defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=) (?\_ . ?\_) (?~ . ?~) (?+ . ?+))
        "electric-pair-mode for org-mode: mark region, then press `*` or `/`.")
    (defun org-add-electric-pairs ()
        "Enable electric-pair-pairs automatically in `org-mode'."
        (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs)
	    electric-pair-text-pairs electric-pair-pairs))
    (defun only-if-use-region (func &rest args)
        "Only insert electric pairs (FUNC ARGS) when selecting something, not on
  single key press."
        (if (use-region-p)
            (apply func args)))
    (advice-add 'electric-pair-post-self-insert-function :around 'only-if-use-region)
    :hook
    (org-mode . org-add-electric-pairs)
    (org-mode . (lambda () (require 'org-tempo)))
    :bind ("M-o" . org-view-mode)
    )

(use-package org-appear
    :after org
    :hook (org-mode . org-appear-mode)
    :custom
    (org-appear-autoemphasis t)
    (org-hide-emphasis-markers t)
    (org-appear-autolinks t)
    (org-appear-autoentities t)
    (org-appear-autosubmarkers t)
    (run-at-time nil nil #'org-appear--set-elements))

(use-package ranger
    :custom
    (ranger-width-preview 0.5)
    (ranger-preview-file t)
    (ranger-max-preview-size 10)
    (ranger-dont-show-binary t)
    :bind ("M-d" . ranger))

(use-package treemacs
    :bind ("M-t" . treemacs))

(use-package treemacs-magit
    :after (treemacs magit)
    :ensure t)

(use-package unfill
    :bind ("M-q" . unfill-toggle))

(use-package vertico
    :init
    (vertico-mode)
    (vertico-mouse-mode)
    (vertico-multiform-mode)
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    (vertico-cycle t)
    ;; configure the display per command.  use a buffer with indices for
    ;; imenu and a flat (Ido-like) menu for M-x.
    ;; (setq vertico-multiform-commands
    ;;       '((consult-imenu buffer indexed)
    ;;         (execute-extended-command unobtrusive)))
    (vertico-multiform-commands  '((consult-imenu buffer indexed)))
    ;; Configure the display per completion category.  Use the grid
    ;; display for files and a buffer for the consult-grep commands.
    ;; (setq vertico-multiform-categories
    ;;       '((file grid)
    ;;         (consult-grep buffer)))
    (vertico-multiform-categories '((consult-grep buffer)))
    :config
    (defun my-vertico-next (&optional n)
        "Cycle without returning to the prompt line."
        (interactive "p")
        (let ((index (+ vertico--index (or n 1))))
            (vertico--goto
                (if (= vertico--total 0) -1 (mod index vertico--total)))))
    :bind
    (:map vertico-map
        ("TAB" . my-vertico-next)
        ("<backtab>" . vertico-previous))
    )

(use-package vundo
    :custom
    (vundo-glyph-alist vundo-unicode-symbols)
    :bind ("M-v" . vundo))

;; (use-package auth-source-1password
;;     :config
;;     (setq auth-source-1password-executable "op")
;;     (setq auth-source-1password-vault "Employee")
;;     (auth-source-1password-enable))

;; (use-package embark-consult
;;   :hook
;;     (embark-collect-mode . consult-preview-at-point-mode))

;; (with-eval-after-load "latex"
;;     (define-key LaTeX-mode-map (kbd "C-c C-a")
;;         (lambda ()
;; 	    (interactive)
;; 	    (TeX-command-sequence '("Arara" "Extex") t))))
;; ;;              (TeX-command-sequence '("Arara" "View") t))))

;; ;; latex mode:
;; (with-eval-after-load "tex"
;;     (add-to-list 'TeX-command-list
;;         `("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara") t)
;;     (add-to-list 'TeX-command-list
;;         `("Extex" "lualatex -synctex=1 -interaction=nonstopmode --shell-escape %s" TeX-run-TeX nil t :help "LuaLatex + SyncTex + ShellEscape + NonstopMode (no halt-on-error)") t)
;;     (tex-source-correlate-mode t) )

;; todo: this crashes instantly!
;; (use-package embark
;;     :bind ("M-a" . embark-act))

;;; init.el ends here

;; Local Variables:
;; jinx-languages: "de_AT de_DE en_GB en_US"
;; End:
