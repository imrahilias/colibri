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
    (;; C-c bindings (mode-specific-map)
        ("C-c M-x" . consult-mode-command)
        ("C-c h" . consult-history)
        ("C-c k" . consult-kmacro)
        ("C-c m" . consult-man)
        ("C-c i" . consult-info)
        ([remap Info-search] . consult-info)
        ;; C-x bindings (ctl-x-map)
        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
        ;; Custom M-# bindings for fast register access
        ("M-#" . consult-register-load)
        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
        ("C-M-#" . consult-register)
        ;; Other custom bindings
        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
        ;; M-g bindings (goto-map)
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
        ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)
        ;; M-s bindings (search-map)
        ("M-s d" . consult-find)
        ("M-s D" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)
        ("M-/" . consult-fd)
        ("M-\\" . consult-ripgrep)
        ;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history))                ;; orig. previous-matching-history-element
    )

(use-package editorconfig
    :init (editorconfig-mode))

(use-package emacs
    :custom-face
    ;; those cant be set by .Xresources:
    (fringe ((t (:background "#FFFFFF"))))
    (highlight ((t (:background "#F0F0F0"))))
    (lazy-highlight ((t (:background "#003641"))))
    (region ((t (:background "#ADD8E6"))))
    (secondary-selection ((t (:background "#CCCCFF"))))
    (trailing-whitespace ((t (:background "#CCCCFF"))))
    :custom
    (auto-save-default t)
    (auto-save-no-message nil)
    (auto-save-visited-file-name 0)
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
    (load-theme 'dichromacy t)
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
                    '(80 . 80) '(100 . 100)))))
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
    ;; (hi-yellow ((t (:foreground "#FAF0E6" :background "#1A004E")))) ; dark mode
    (hi-yellow ((t (:foreground "#000000" :background "#F0F0F0")))) ; light mode
    :init (global-highlight-thing-mode))

(use-package jinx
    :custom
    (jinx-languages "de_AT de_DE en_GB en_US")
    ;;:hook (emacs-startup . global-jinx-mode)
    :bind
    ("M-j" . jinx-correct)
    ("C-M-j" . jinx-languages))

(use-package json-mode
    :config
    ;; set tab size to 4
    (setq json-encoding-default-indentation "    ")
    )

(use-package lsp-mode
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook
    (python-mode . lsp-deferred)
    (lua-mode . lsp-deferred)
    (toml-mode . lsp-deferred)
    (lisp-mode . lsp-deferred)
    (lsp-mode . lsp-enable-which-key-integration)
    :commands (lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

;;Elsa currently supports lsp-mode, but it is not yet built-in to lsp-mode
;;itself because it (Elsa LSP) is not stable enough.
;;(elsa-lsp-register)

(use-package lua-mode)

(use-package magit
    :custom-face
    ;; dark mode
    ;; (magit-diff-added ((t (:foreground "#2E8B57" :background "#000000"))))
    ;; (magit-diff-removed ((t (:foreground "#8B0000" :background "#000000"))))
    ;; light mode
    (magit-diff-added ((t (:foreground "#2E8B57" :background "#FFFFFF"))))
    (magit-diff-removed ((t (:foreground "#8B0000" :background "#FFFFFF"))))
    :custom
    (magit-diff-refine-hunk 'all)
    (magit-diff-highlight-hunk-body nil)
    :bind ("C-c g" . magit))

(use-package marginalia
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

(use-package markdown-mode
    :custom-face (markdown-code-face ((t (:inherit org-block)))))

(use-package multiple-cursors
    :bind(("C-c l" . mc/edit-lines)
             ("C-c >" . mc/mark-next-like-this)
             ("C-c <" . mc/mark-previous-like-this)
             ("C-c =" . mc/mark-all-like-this)
             ("C-c <mouse-1>" . mc/add-cursor-on-click)
             ("C-c RET" . set-rectangular-region-anchor)
             ))

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
        'org-babel-load-languages '(
                                       (C . t)
                                       (F90 . t)
                                       (emacs-lisp . t)
                                       (gnuplot . t)
                                       (js . t)
                                       (latex . t)
                                       (lua . t)
                                       (makefile . t)
                                       (matlab . t)
                                       (octave . t)
                                       (org . t)
                                       (python . t)
                                       (shell . t)
                                       ))
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
    ;; dark mode
    ;; (set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)
    ;; (set-face-attribute 'org-code nil :background "#0b0d0f")
    ;; (set-face-attribute 'org-table nil :background "#0b0d0f")
    ;; (set-face-attribute 'org-table-header nil :foreground "#787787" :background "#161a1f" :bold t)
    ;; (set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
    ;; (set-face-attribute 'org-block nil :background "#0b0d0f")
    ;; light mode
    (set-face-attribute 'org-block-begin-line nil :background "#F0F0F0" :bold t)
    (set-face-attribute 'org-code nil :background "#F5F5F5")
    (set-face-attribute 'org-table nil :foreground "#000000" :background "#F5F5F5")
    (set-face-attribute 'org-table-header nil :background "#F0F0F0" :bold t)
    (set-face-attribute 'org-verbatim nil :background "#F5F5F5")
    (set-face-attribute 'org-block nil :background "#F5F5F5")
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
    :bind
    ("M-o" . org-view-mode)
    (:map org-mode-map ("C-<return>" . nil))
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
    :after (treemacs magit))

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
        ("M-TAB" . my-vertico-next)
        ("M-<iso-lefttab>" . vertico-previous) ;;M-S-TAB
        ("M-RET" . vertico-exit-input)
        )
    )

(use-package vundo
    :custom
    (vundo-glyph-alist vundo-unicode-symbols)
    :bind ("M-v" . vundo))

(use-package which-key
    :init
    (which-key-mode))

(use-package yaml-mode)

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
