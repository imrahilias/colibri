;; -*- lexical-binding: t; -*-

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

(setq package-selected-packages
    '(all-the-icons all-the-icons-completion all-the-icons-dired
         all-the-icons-nerd-fonts auto-complete auto-correct blacken
         color-identifiers-mode consult consult-flycheck consult-flyspell
         csv-mode cuda-mode eat embark embark-consult flycheck
         flycheck-bashate flycheck-yamllint fuzzy ghnuplot glab highlight
         highlight-blocks highlight-defined highlight-escape-sequences
         highlight-function-calls highlight-numbers highlight-operators
         highlight-parentheses highlight-quoted highlight-symbol
         highlight-thing highlight-unique-symbol hl-block-mode json-mode
         live-py-mode lua-mode luarocks magit magit-gitlab marginalia
         markdown-mode mmm-mode orderless org-auto-tangle org-faces org-modern
         org-view-mode poly-markdown poly-org rainbow rainbow-identifiers
         rainbow-mode ranger ripgrep spinner vertico wgrep yaml-mode yaml-pro))

;; Commentary:
"well its merely my emacs config"

;;; Code:

;;       |          _|  _|
;;   __| __| |   | |   |
;; \__ \ |   |   | __| __|
;; ____/\__|\__,_|_|  _|
;;

;; ;; markdown/org translator:
;; (add-to-list 'load-path "~/.emacs.d/others/org-pandoc-import")
;; (require 'org-pandoc-import)
;; (require 'org-pandoc-import-transient)

;; what are these?
;;string-inflection
;;ample-regexps

;; TODO: why is whitespace-mode not started on init?
(global-whitespace-mode 1)
(setq whitespace-style '(face tabs))

(use-package emacs

    ;;  |   |
    ;;  __| __ \   _ \ __ `__ \   _ \
    ;;  |   | | |  __/ |   |   |  __/
    ;; \__|_| |_|\___|_|  _|  _|\___|
    ;;

    :init

    ;; enable these:
    ;;(global-whitespace-mode)
    (global-font-lock-mode)
    (global-color-identifiers-mode)
    (global-hi-lock-mode)
    (global-highlight-parentheses-mode)
    (global-highlight-thing-mode)
    (global-hl-line-mode)
    (global-auto-revert-mode)

    ;; disable those:
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (blink-cursor-mode 0)
    (indent-tabs-mode 0)


    (set-face-attribute 'hi-yellow nil :foreground "#FAF0E6" :background "#1A004E")
    (set-face-background 'highlight "#0D0D0D")


    ;; ui and base colors:
    (setq
        theme-color-accent  "#4682B4"
        theme-color-level-1 "#1D1F21"
        theme-color-level-2 "#373B41"
        theme-color-level-3 "#C5C8C6"
        font-lock-maximum-decoration t
        font-lock-maximum-size '262144
        ;;whitespace-style '(face tabs)
        )



    ;; (set-face-attribute 'default nil :family "DejaVu Nerd Font Mono")
    ;; (set-face-attribute 'default nil :height (* 12 10))
    (set-face-attribute 'default nil :family "BitstromWera Nerd Font")
    (set-face-attribute 'default nil :height (* 12 10))
    (set-cursor-color "#FF00FF")
    (set-foreground-color "#FAF0E6")
    (set-background-color "#000000")
    (set-face-foreground 'default "#FAF0E6")
    (set-face-background 'default "#000000")
    (set-face-foreground 'font-lock-string-face "#008080")
    (set-face-foreground 'font-lock-comment-face "#666699")
    (set-face-foreground 'font-lock-variable-name-face "#8B008B")
    (set-face-background 'region "#191970")
    (set-face-background 'secondary-selection "#330099")
    (set-face-foreground 'window-divider "#444444")
    (set-face-background 'trailing-whitespace "#330099")
    (set-face-background 'lazy-highlight "#003641"); #004B5A
    (set-face-foreground 'isearch "#000000")
    (set-face-background 'isearch "#FF00FF")
    (set-face-foreground 'isearch-fail "#000000")
    (set-face-background 'isearch-fail "yellow")
    (set-face-background 'whitespace-tab "#ff6000")


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
    (global-set-key (kbd "C-c t") 'toggle-transparency)

    ;; Window dividers (for Emacs 25.1+)
    (when (boundp 'window-divider-mode)
        (setq window-divider-default-places t
            window-divider-default-bottom-width 1
            window-divider-default-right-width 1)
        (window-divider-mode 1))

    ;; the fringe:
    ;; the fringe is like a vertical ruler that runs from the top to the
    ;; bottom of the buffer -- the left fringe is sandwiched between the
    ;; line numbers and the text. it can be invisible if it is the same
    ;; color as the default background of the user, or it can be a
    ;; different color.
    (setq-default left-fringe-width  10)
    (setq-default right-fringe-width  0)
    (set-face-attribute 'fringe nil :background "#000000")

    ;; disable up/downcase region warning message:
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)

    ;; `window-divider-mode' gives us finer control over the border
    ;; between windows. The native border "consumes" a pixel of the left
    ;; fringe on righter-most splits (in Yamamoto's emacs-mac at least),
    ;; window-divider does not. NOTE Only available on Emacs 25.1+:
    (when (boundp 'window-divider-mode)
        (setq window-divider-default-places t
            window-divider-default-bottom-width 1
            window-divider-default-right-width 1)
        (window-divider-mode +1))

    (setq sort-fold-case nil
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
        enable-recursive-minibuffers t)


    ;; hide modeline:
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

    ;; hide the mode-line in every buffer by default:
    (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    )

;;
;;   _ \   __| _` |
;;  (   | |   (   |
;; \___/ _|  \__, |
;;           |___/

(use-package org
    :custom

    (org-emphasis-alist
        '(("*" bold)
             ("/" italic)
             ("_" underline)
             ("-" overline)
             ("=" org-verbatim org-verbatim)
             ("~" org-code org-code)
             ("+" (:strike-through t))))
    (org-src-fontify-natively t)
    (org-fontify-quote-and-verse-blocks t)
    (org-startup-indented t)
    (org-confirm-babel-evaluate nil)
    (org-table-header-line-p t)

    :config
    ;; --- Make all org-level faces bold ---
    (dolist (face '(org-level-1 org-level-2 org-level-3
                       org-level-4 org-level-5 org-level-6
                       org-level-7 org-level-8))
        (set-face-attribute face nil :bold t))
    (set-face-attribute 'org-quote nil :italic t)
    (set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
    (set-face-attribute 'org-code nil :background "#0b0d0f")
    (set-face-attribute 'org-block nil :background "#0b0d0f")
    (set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)
    (set-face-attribute 'org-table nil :background "#0b0d0f")
    (set-face-attribute 'org-table-header nil :foreground "#787787" :background "#161a1f" :bold t)
    (set-face-attribute 'org-meta-line nil :bold t)

    ;; org tempo expands snippets to structures defined in
    ;; org-structure-template-alist and org-tempo-keywords-alist. for
    ;; example, < s tab creates a code block:
    (require 'org-tempo)

    ;; Electric Pairs for Org
    (defun org-add-electric-pairs ()
        "Enable electric-pair-pairs automatically in `org-mode'."
        (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs)
            electric-pair-text-pairs electric-pair-pairs))
    (add-hook 'org-mode-hook 'org-add-electric-pairs)

    (defun only-if-use-region (func &rest args)
        "Only insert electric pairs (FUNC ARGS) when selecting something, not on
single key press."
        (if (use-region-p)
            (apply func args)))
    (advice-add 'electric-pair-post-self-insert-function :around 'only-if-use-region)

    ;; org-view-mode gets rid of the markup, but is read only:
    :bind ("C-c v" . 'org-view-mode)
    )

(use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))


(use-package rainbow-mode
    :ensure t
    :init
    (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
        (lambda () (rainbow-mode t)))
    (my-global-rainbow-mode t))

;; Enable Vertico completion UI:
(use-package vertico
    :init
    (vertico-mode)
    (vertico-mouse-mode)
    (vertico-multiform-mode)
    :config
    (setq completion-in-region-function #'consult-completion-in-region
        vertico-cycle t)
    (keymap-set vertico-map "TAB" 'vertico-next)
    (keymap-set vertico-map "<backtab>" 'vertico-previous)

    ;; configure the display per command.  use a buffer with indices for
    ;; imenu and a flat (Ido-like) menu for M-x.
    ;; (setq vertico-multiform-commands
    ;;       '((consult-imenu buffer indexed)
    ;;         (execute-extended-command unobtrusive)))
    (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)))

    ;; Configure the display per completion category.  Use the grid
    ;; display for files and a buffer for the consult-grep commands.
    ;; (setq vertico-multiform-categories
    ;;       '((file grid)
    ;;         (consult-grep buffer)))
    (setq vertico-multiform-categories
        '((consult-grep buffer)))

    ;; If you don't want to go back to the prompt line by setting
    ;; vertico-cycle, define another function like this:
    ;; (defun my-vertico-next (&optional n)
    ;;   "Circulate without returning to the prompt line"
    ;;   (interactive "p")
    ;;   (let ((index (+ vertico--index (or n 1))))
    ;;     (vertico--goto
    ;;      (if (= vertico--total 0) -1 (mod index vertico--total)))))

    ;; (define-key vertico-map (kbd "TAB") 'my-vertico-next)
    )




;; Optionally use the `orderless' completion style.
(use-package orderless
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
    ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
    ;; available in the *Completions* buffer, add it to the
    ;; `completion-list-mode-map'.
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

(setq all-the-icons-completion-mode t)
(setq all-the-icons-dired-mode t)

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

;; electric pairs:
(electric-pair-mode)

;; --- Consult & Embark ---
(global-set-key (kbd "M-f") 'consult-flycheck)
(global-set-key (kbd "M-i") 'consult-flyspell)
(global-set-key (kbd "M-s") 'consult-line)
(global-set-key (kbd "M-/") 'consult-fd)
(global-set-key (kbd "M-\\") 'consult-ripgrep)
(global-set-key (kbd "M-a") 'embark-act)

;; eranger
(define-key global-map (kbd "M-r") 'ranger)

;; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; eval .emacs:
(global-set-key (kbd "C-c r") (lambda() (interactive) (load-file "~/.config/emacs/init.el")))

;; open various configs:
(global-set-key (kbd "C-c a") (lambda() (interactive) (find-file "~/.config/awesome/rc.lua")))
(global-set-key (kbd "C-c e") (lambda() (interactive) (find-file "~/.config/emacs/init.el")))
;; (global-set-key (kbd "C-c l") (lambda() (interactive) (find-file "~/.config/lf/lfrc")))
;; (global-set-key (kbd "C-c R") (lambda() (interactive) (find-file "~/.config/ranger/rifle.conf")))
(global-set-key (kbd "C-c x") (lambda() (interactive) (find-file "~/.xinitrc")))
(global-set-key (kbd "C-c X") (lambda() (interactive) (find-file "~/.Xresources")))
(global-set-key (kbd "C-c y") (lambda() (interactive) (find-file "~/.config/yazi/keymap.toml")))
(global-set-key (kbd "C-c z") (lambda() (interactive) (find-file "~/.zshrc")))

;; printer command:
(setq lpr-command "lp")
(setq lpr-add-switches nil)

;; switch window:
;; (global-set-key [C-tab] 'other-window)
;; (global-set-key [C-C-tab] (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-<mouse-8>") 'next-buffer)
(global-set-key (kbd "M-<mouse-9>") 'previous-buffer)
(global-set-key (kbd "C-<right>") 'next-buffer)
(global-set-key (kbd "C-<left>") 'previous-buffer)

;; page down/up:
(global-set-key (kbd "M-<wheel-down>") (kbd "C-v"))
(global-set-key (kbd "M-<wheel-up>") (kbd "M-v"))

;; You can change the backup-file naming convention by redefining this
;; function. The following example redefines make-backup-file-name to
;; prepend a ‘.’ in addition to appending a tilde:
(defun make-backup-file-name (filename)
    "Change the backup-file naming convention to FILENAME.  The following
example redefines 'make-backup-file-name' to prepend a '.' in addition
to appending a tilde '~'."
    (expand-file-name
        (concat "." (file-name-nondirectory filename) "~")
        (file-name-directory filename)))

;; This function exists so that you can customize it if you wish to
;; change the naming convention for auto-save files. If you redefine
;; it, be sure to redefine the function make-auto-save-file-name
;; correspondingly:
(defun auto-save-file-name-p (filename)
    "Return non-nil if FILENAME can be yielded by..."
    (string-match "^\.*~$" filename))

;; This exists as a separate function so that you can redefine it to
;; customize the naming convention for auto-save files. Be sure to
;; change auto-save-file-name-p in a corresponding way:
(defun make-auto-save-file-name ()
    "Return file name to use for auto-saves \
of current buffer.
..."
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

;; (defun scroll-down-keep-cursor ()
;;   ;; Scroll the text one line down while keeping the cursor
;;   (interactive)
;;   (scroll-down 1)
;;   )
;; (global-set-key (kbd "C-,") 'scroll-down-keep-cursor)


;; (defun scroll-up-keep-cursor ()
;;   ;; Scroll the text one line up while keeping the cursor
;;   (interactive)
;;   (scroll-up 1)
;;   )
;; (global-set-key (kbd "C-.") 'scroll-up-keep-cursor)

(defun insert-current-date()
    "Insert current date."
    (interactive)
    (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %k:%M')") )
    )
(global-set-key (kbd "C-c d") 'insert-current-date)

;; un/compact block:
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
(global-set-key (kbd "M-q") 'fill-or-unfill)

(defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=) (?\_ . ?\_) (?~ . ?~) (?+ . ?+))
    "'electric-pair-mode' for 'org-mode': mark region, then press *.")


;;  |
;;  |  _` | __ \   _` |
;;  | (   | |   | (   |
;; _|\__,_|_|  _|\__, |
;;               |___/

;; Start auto-complete-mode:
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (auto-complete-mode 1))))

;; highlight parentheses when the cursor is next to them:
(require 'paren)
(show-paren-mode t)

;; c mode customizations:
(cwarn-mode t)
(setq c-default-style "linux")
(which-function-mode t)
(setq c-basic-offset 2)
(global-set-key (kbd "C-c p") 'compile)

;; octave mode:
;; (global-set-key (kbd "C-c C-c") 'octave-send-region)

;; latex mode:
(with-eval-after-load "tex"
    (add-to-list 'TeX-command-list
        `("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara") t)
    (add-to-list 'TeX-command-list
        `("Extex" "lualatex -synctex=1 -interaction=nonstopmode --shell-escape %s" TeX-run-TeX nil t :help "LuaLatex + SyncTex + ShellEscape + NonstopMode (no halt-on-error)") t)
    (tex-source-correlate-mode t) )

(with-eval-after-load "latex"
    (define-key LaTeX-mode-map (kbd "C-c C-a")
        (lambda ()
            (interactive)
            (TeX-command-sequence '("Arara" "Extex") t))))
;;              (TeX-command-sequence '("Arara" "View") t))))

;;                  | |
;;   __| __ \   _ \ | |  __|
;; \__ \ |   |  __/ | |\__ \
;; ____/ .__/ \___|_|_|____/
;;      _|

;; flyspell mode:
(dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

;; ispell:
(with-eval-after-load "ispell"
    ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
    ;; dictionary' even though multiple dictionaries will be configured
    ;; in next line.
    ;;(setenv "LANG" "en_GB")
    (setq ispell-program-name "hunspell")
    ;; Configure German, Swiss German, and two variants of English.
    (setq ispell-dictionary "de_DE,en_GB,en_US")
    ;; ispell-set-spellchecker-params has to be called
    ;; before ispell-hunspell-add-multi-dic will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US")
    ;; For saving words to the personal dictionary, don't infer it from
    ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
    (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; the personal dictionary file has to exist, otherwise hunspell will
;; silently not use it:
;; (unless (file-exists-p ispell-personal-dictionary)
;; (write-region "" nil ispell-personal-dictionary nil 0))

(setenv "LANG" "en_US.UTF-8")

                                        ;(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
      [default default default italic underline success warning error])
 '(ansi-color-names-vector
      ["#000000" "#8B0000" "#2E8B57" "#DAA520" "#4682B4" "#8B008B" "#20B2AA" "#FFEBCD"])
 '(custom-safe-themes
      '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" default))
 '(org-babel-load-languages
      '((C . t) (R . t) (emacs-lisp . t) (fortran . t) (gnuplot . t) (haskell . t)
           (lua . t) (makefile . t) (octave . t) (org . t) (python . t)
           (shell . t) (sql . t) (sqlite . t)))
 '(package-selected-packages
      '(all-the-icons all-the-icons-completion all-the-icons-dired
           all-the-icons-nerd-fonts auto-complete auto-correct blacken
           color-identifiers-mode consult consult-flycheck consult-flyspell
           csv-mode cuda-mode eat embark embark-consult flycheck
           flycheck-bashate flycheck-yamllint ghnuplot glab highlight
           highlight-blocks highlight-defined highlight-escape-sequences
           highlight-function-calls highlight-numbers highlight-operators
           highlight-parentheses highlight-quoted highlight-symbol
           highlight-thing highlight-unique-symbol hl-block-mode json-mode
           live-py-mode lua-mode luarocks magit magit-gitlab marginalia
           markdown-mode mmm-mode orderless org-auto-tangle org-faces org-modern
           org-view-mode poly-markdown poly-org rainbow rainbow-identifiers
           rainbow-mode ranger ripgrep spinner vertico wgrep yaml-mode yaml-pro))
 '(warning-suppress-log-types '((auto-save))))
