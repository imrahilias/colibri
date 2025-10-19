;;; package --- Summary ; -*- lexical-binding: t; -*-

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                            ("gnu"   . "https://elpa.gnu.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Install use-package if it's not already there
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; Load use-package
(require 'use-package)
;; This is a crucial line that makes `:ensure t` work everywhere
(setq use-package-always-ensure t)

;; All persistent customisation is here in init.el
(setq custom-file (make-temp-file "emacs-custom"))

;; Install all these with 'package-install-selected-packages'
(setq package-selected-packages
    '(all-the-icons all-the-icons-completion all-the-icons-dired
	 all-the-icons-nerd-fonts auto-complete auto-correct blacken
	 color-identifiers-mode consult consult-flycheck consult-flyspell
	 csv-mode cuda-mode eat embark embark-consult flycheck
	 flycheck-bashate flycheck-yamllint fuzzy highlight
	 highlight-blocks highlight-defined highlight-escape-sequences
	 highlight-function-calls highlight-numbers highlight-operators
	 highlight-parentheses highlight-quoted highlight-symbol
	 highlight-thing highlight-unique-symbol hl-block-mode json-mode
	 live-py-mode lua-mode luarocks magit magit-gitlab marginalia
	 markdown-mode mmm-mode orderless org-appear org-auto-tangle org-faces org-modern
	 org-view-mode poly-mode poly-markdown rainbow rainbow-identifiers
	 rainbow-mode ranger ripgrep string-inflection vertico wgrep yaml-mode yaml-pro))

;;; Commentary:
"well its merely my emacs config"

;;; Code:

;; ;; markdown/org translator:
;; (add-to-list 'load-path "~/.emacs.d/others/org-pandoc-import")
;; (require 'org-pandoc-import)
;; (require 'org-pandoc-import-transient)

(use-package emacs
    :custom-face
    (default ((t (:foreground "#FAF0E6" :background "#000000"))))
    (font-lock-string-face ((t (:foreground "#008080"))))
    (font-lock-comment-face ((t (:foreground "#666699"))))
    (font-lock-variable-name-face ((t (:foreground "#8B008B"))))
    (region ((t (:background "#191970"))))
    (secondary-selection ((t (:background "#330099"))))
    (window-divider ((t (:foreground "#444444"))))
    (trailing-whitespace ((t (:background "#330099"))))
    (lazy-highlight ((t (:background "#003641"))))
    (isearch ((t (:foreground "#000000" :background "#FF00FF"))))
    (isearch-fail ((t (:foreground "#000000" :background "yellow"))))
    (highlight ((t (:background "#0D0D0D"))))

    :init
    ;; (set-face-attribute 'default nil :family "DejaVu Nerd Font Mono")
    (set-face-attribute 'default nil :family "BitstromWera Nerd Font")
    (set-face-attribute 'default nil :height (* 12 10))
    (set-face-attribute 'fringe nil :background "#000000")
    (set-cursor-color "#FF00FF")
    (set-foreground-color "#FAF0E6")
    (set-background-color "#000000")

    (global-font-lock-mode)
    (global-color-identifiers-mode)
    (global-hi-lock-mode)
    (global-highlight-parentheses-mode)
    (global-hl-line-mode)
    (global-auto-revert-mode)

    (flyspell-mode 0)
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (blink-cursor-mode 0)
    (indent-tabs-mode 0)

    (setopt
        pop-up-frames t
        sort-fold-case nil
        inhibit-startup-message t
        initial-scratch-message ""
        scroll-margin 5
        scroll-conservatively 0
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        mouse-wheel-scroll-amount '(3 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse nil
        mouse-yank-at-point t
        show-trailing-whitespace t
        ;;enable-recursive-minibuffers t
        use-short-answers t
        font-lock-maximum-decoration t
        font-lock-maximum-size '262144
        left-fringe-width  10
        right-fringe-width  0
        lpr-command "lp"
        lpr-add-switches nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        )

    ;; opacity
    (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
    (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

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

    ;; disable up/downcase region warning message:
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)

    ;; `window-divider-mode' gives us finer control over the border
    ;; between windows. The native border "consumes" a pixel of the left
    ;; fringe on righter-most splits (in Yamamoto's emacs-mac at least),
    ;; window-divider does not. NOTE Only available on Emacs 25.1+:
    ;; (when (boundp 'window-divider-mode)
    ;;     (setq window-divider-default-places t
    ;;         window-divider-default-bottom-width 1
    ;;         window-divider-default-right-width 1)
    ;;     (window-divider-mode 1))

    (defvar-local hidden-mode-line-mode nil)
    (define-minor-mode hidden-mode-line-mode
        "Minor mode to hide the mode-line in the current buffer."
        :init-value nil
        :global t
        :variable hidden-mode-line-mode
        :group 'editing-basics
        (if hidden-mode-line-mode
            (setq hide-mode-line mode-line-format
                mode-line-format nil)
            (setq mode-line-format hide-mode-line
                hide-mode-line nil))
        (force-mode-line-update)
        ;; Apparently force-mode-line-update is not always enough to
        ;; redisplay the mode-line
        (redraw-display)
        (when (and (called-interactively-p 'interactive)
                  hidden-mode-line-mode)
            (run-with-idle-timer
                0 nil 'message
                (concat "hidden mode line mode enabled.  "
                    "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

    (defun insert-current-date()
        "Insert current date."
        (interactive)
        (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %k:%M')")))

    (defun make-backup-file-name (filename)
        "Change the backup-file naming convention to FILENAME.  The following
example redefines 'make-backup-file-name' to prepend a `.` in addition
to appending a tilde `~`."
        (expand-file-name
            (concat "." (file-name-nondirectory filename) "~")
            (file-name-directory filename)))

    (defun auto-save-file-name-p (filename)
        "Change the naming convention for auto-save files.  If you redefine it,
be sure to redefine the function 'make-auto-save-file-name'
correspondingly.  Return non-nil if FILENAME can be yielded by FILENAME."
        (string-match "^\.*~$" filename))

    (defun make-auto-save-file-name ()
        "This exists as a separate function so that you can redefine it to
customize the naming convention for auto-save files.  Be sure to change
'auto-save-file-name-p' in a corresponding way.  Return file name to use
for auto-saves of current buffer."
        (if buffer-file-name
            (concat
                (file-name-directory buffer-file-name)
                "."
                (file-name-nondirectory buffer-file-name)
                "~")
            (expand-file-name
                (concat "." (buffer-name) "~"))))

    (defun my-outline-set-global-ellipsis (ellipsis)
        "Apply the ellipsis ELLIPSIS to outline mode globally."
        (let* ((face-offset (* (face-id 'shadow) (ash 1 22)))
                  (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
            (set-display-table-slot standard-display-table 'selective-display value)))
    (my-outline-set-global-ellipsis " ▼")

    (defun fill-or-unfill ()
        "Reformat current paragraph or region to `fill-column', like
`fill-paragraph' or “unfill”.  When there is a text selection, act on
the selection, else, act on a text block separated by blank lines.  URL
`http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2017-01-08."
        (interactive)
        ;; This command symbol has a property “'compact-p”, the possible
        ;; values are t and nil. This property is used to easily determine
        ;; whether to compact or uncompact, when this command is called
        ;; again.
        (let ( ($compact-p
                   (if (eq last-command this-command)
                       (get this-command 'compact-p)
                       (> (- (line-end-position) (line-beginning-position)) fill-column)))
                 (deactivate-mark nil)
                 ($blanks-regex "\n[ \t]*\n")
                 $p1 $p2
                 )
            (if (use-region-p)
                (progn (setq $p1 (region-beginning))
                    (setq $p2 (region-end)))
                (save-excursion
                    (if (re-search-backward $blanks-regex nil "NOERROR")
                        (progn (re-search-forward $blanks-regex)
                            (setq $p1 (point)))
                        (setq $p1 (point)))
                    (if (re-search-forward $blanks-regex nil "NOERROR")
                        (progn (re-search-backward $blanks-regex)
                            (setq $p2 (point)))
                        (setq $p2 (point)))))
            (if $compact-p
                (fill-region $p1 $p2)
                (let ((fill-column most-positive-fixnum ))
                    (fill-region $p1 $p2)))
            (put this-command 'compact-p (not $compact-p))))

    :hook
    (after-change-major-mode . hidden-mode-line-mode)
    (before-save . delete-trailing-whitespace)
    ;;(text-mode . (auto-complete-mode auto-correct-mode))

    :bind
    ("M-<mouse-8>" . previous-buffer)
    ("M-<mouse-9>" . next-buffer)
    ("C-<right>" . next-buffer)
    ("C-<left>" . previous-buffer)
    ("M-<wheel-down>" . scroll-up)
    ("M-<wheel-up>" . scroll-down)
    ("C-c d" . insert-current-date)
    ("M-q" . fill-or-unfill)
    ("C-c t". toggle-transparency)
    )

(global-set-key (kbd "C-c r") (lambda() (interactive) (load-file "~/.config/emacs/init.el")))

(use-package org
    :config
    (setopt
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-startup-indented t
        org-confirm-babel-evaluate nil
        org-table-header-line-p t
        org-fontify-quote-and-verse-blocks t)

    (set-face-attribute 'org-level-1 nil :bold t :foreground "#FAF0E6" :background "#4682B4")
    (set-face-attribute 'org-level-2 nil :bold t)
    (set-face-attribute 'org-level-3 nil :bold t)
    (set-face-attribute 'org-level-4 nil :bold t)
    (set-face-attribute 'org-level-5 nil :bold t)
    (set-face-attribute 'org-level-6 nil :bold t)
    (set-face-attribute 'org-level-7 nil :bold t)
    (set-face-attribute 'org-level-8 nil :bold t)
    (set-face-attribute 'org-quote nil :italic t)
    (set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
    (set-face-attribute 'org-code nil :background "#0b0d0f")
    (set-face-attribute 'org-block nil :background "#0b0d0f")
    (set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)
    (set-face-attribute 'org-table nil :background "#0b0d0f")
    (set-face-attribute 'org-table-header nil :foreground "#787787" :background "#161a1f" :bold t)
    (set-face-attribute 'org-meta-line nil :bold t)

    ;; org mode emphasis styles, collides with 'org-appear'.
    ;; (setq org-emphasis-alist
    ;;     (quote(
    ;;               ("*" bold)
    ;;               ("/" italic)
    ;;               ("_" underline)
    ;;               ("-" overline)
    ;;               ("=" org-verbatim org-verbatim)
    ;;               ("~" org-code org-code)
    ;;               ("+" (:strike-through t)))))

    ;; '<s tab' creates a code block;
    (require 'org-tempo)

    (electric-pair-mode)
    (defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=) (?\_ . ?\_) (?~ . ?~) (?+ . ?+))
        "'electric-pair-mode' for 'org-mode': mark region, then press *.")
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

    :hook (org-mode . org-add-electric-pairs)

    :bind ("C-c v" . org-view-mode)
    )

(use-package org-appear
    :after org
    :hook (org-mode . org-appear-mode)
    :config
    (setopt org-appear-autoemphasis t
        org-hide-emphasis-markers t
        org-appear-autolinks t
        org-appear-autoentities t
        org-appear-autosubmarkers t
        )
    (run-at-time nil nil #'org-appear--set-elements))

(use-package highlight-thing
  :ensure t
  :custom-face
  (hi-yellow ((t (:foreground "#FAF0E6" :background "#1A004E"))))
  :init (global-highlight-thing-mode))

(use-package editorconfig
  :ensure t
    :init (editorconfig-mode))

(use-package rainbow-mode
   :ensure t
   :init
   (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
       (lambda () (rainbow-mode t)))
   (my-global-rainbow-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode)
  :config
  (setopt completion-in-region-function #'consult-completion-in-region
          vertico-cycle t)

  ;; configure the display per command.  use a buffer with indices for
  ;; imenu and a flat (Ido-like) menu for M-x.
  ;; (setq vertico-multiform-commands
  ;;       '((consult-imenu buffer indexed)
  ;;         (execute-extended-command unobtrusive)))
  (setopt vertico-multiform-commands
          '((consult-imenu buffer indexed)))

  ;; Configure the display per completion category.  Use the grid
  ;; display for files and a buffer for the consult-grep commands.
  ;; (setq vertico-multiform-categories
  ;;       '((file grid)
  ;;         (consult-grep buffer)))
  (setopt vertico-multiform-categories
          '((consult-grep buffer)))

  ;; If you don't want to go back to the prompt line by setting
  ;; vertico-cycle, define another function like this:
  (defun my-vertico-next (&optional n)
        "Circulate without returning to the prompt line"
        (interactive "p")
        (let ((index (+ vertico--index (or n 1))))
            (vertico--goto
                (if (= vertico--total 0) -1 (mod index vertico--total)))))

    :bind
    (:map vertico-map
        ("TAB" . my-vertico-next)
        ("<backtab>" . vertico-previous))
    )

(use-package orderless
    :ensure t
    :custom
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
    ;; (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles partial-completion))))
    (completion-category-defaults nil) ;; Disable defaults, use our settings
    (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
    :ensure t
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

(use-package all-the-icons
    :ensure t
    :init
    (all-the-icons-completion-mode t)
    (all-the-icons-dired-mode t))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(use-package consult
    :ensure t
    :bind
    ("M-f" . consult-flycheck)
    ;("M-i" . consult-flyspell)
    ("M-s" . consult-line)
    ("M-/" . consult-fd)
    ("M-\\" . consult-ripgrep)
    )

;; todo: this crashes instantly!
(use-package embark
    :bind ("M-a" . embark-act))

(use-package ranger
    :bind ("M-r" . ranger))

(use-package octave
    :bind ("C-c C-c" . octave-send-region))

;; ;; latex mode:
;; (with-eval-after-load "tex"
;;     (add-to-list 'TeX-command-list
;;         `("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara") t)
;;     (add-to-list 'TeX-command-list
;;         `("Extex" "lualatex -synctex=1 -interaction=nonstopmode --shell-escape %s" TeX-run-TeX nil t :help "LuaLatex + SyncTex + ShellEscape + NonstopMode (no halt-on-error)") t)
;;     (tex-source-correlate-mode t) )

;; (with-eval-after-load "latex"
;;     (define-key LaTeX-mode-map (kbd "C-c C-a")
;;         (lambda ()
;; 	    (interactive)
;; 	    (TeX-command-sequence '("Arara" "Extex") t))))
;; ;;              (TeX-command-sequence '("Arara" "View") t))))

(use-package ispell
    ;;:custom
    :init
    (setopt ispell-program-name "hunspell"
        ispell-dictionary "de_DE,en_GB,en_US"
        ispell-personal-dictionary "~/.hunspell_personal")
    ;; ispell-set-spellchecker-params has to be called before
    ;; ispell-hunspell-add-multi-dic will work
    ;;:config
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "de_AT,de_DE,en_GB,en_US"))

(use-package jinx
    :ensure t
    :config
    (jinx-languages "de_AT de_DE en_GB en_US")
    :hook (emacs-startup . global-jinx-mode)
    :bind
    ("M-j" . jinx-correct)
    ("C-M-j" . jinx-languages))

;;; init.el ends here

;; End:
