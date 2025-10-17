;; -- lexical-binding: t;--

;;; Commentary:
"well its merely my emacs config"

;;; Code:

;;       |          _|  _|
;;   __| __| |   | |   |
;; \__ \ |   |   | __| __|
;; ____/\__|\__,_|_|  _|
;;

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf"
    "#eeeeec"])
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b"
     default))
 '(org-babel-load-languages
   '((C . t) (R . t) (emacs-lisp . t) (fortran . t) (gnuplot . t)
     (haskell . t) (lisp . t) (lua . t) (makefile . t) (octave . t)
     (org . t) (python . t) (sed . t) (shell . t) (sql . t)
     (sqlite . t)))
 '(package-selected-packages
   '(all-the-icons all-the-icons-completion all-the-icons-dired
                   all-the-icons-nerd-fonts ample-regexps
                   atom-one-dark-theme auto-complete auto-correct
                   bash-completion blacken color-identifiers-mode
                   company consult consult-flycheck consult-flyspell
                   csv-mode cuda-mode dired-subtree eat embark
                   embark-consult evil flycheck fuzzy glab gnuplot
                   highlight highlight-blocks highlight-defined
                   highlight-escape-sequences highlight-function-calls
                   highlight-numbers highlight-operators
                   highlight-parentheses highlight-quoted
                   highlight-symbol highlight-thing
                   highlight-unique-symbol hl-block-mode json-mode
                   live-py-mode lua-mode luarocks magit magit-gitlab
                   marginalia markdown-mode matlab-mode mmm-mode
                   orderless org-auto-tangle org-modern org-view-mode
                   php-mode poly-ansible poly-markdown polymode
                   rainbow-identifiers rainbow-mode ranger ripgrep
                   rust-mode spinner string-inflection
                   timu-caribbean-theme vertico wgrep yaml-pro))
 '(warning-suppress-log-types '((auto-save))))

;; ;; markdown/org translator:
;; (add-to-list 'load-path "~/.emacs.d/others/org-pandoc-import")
;; (require 'org-pandoc-import)
;; (require 'org-pandoc-import-transient)

;; highlight tabs:
(setq whitespace-style '(face tabs))
(global-whitespace-mode 1)

;;  |   |
;;  __| __ \   _ \ __ `__ \   _ \
;;  |   | | |  __/ |   |   |  __/
;; \__|_| |_|\___|_|  _|  _|\___|
;;

;; readymade themes:
;; (load-theme 'timu-caribbean t)
;; (customize-set-variable 'timu-caribbean-org-intense-colors t)
;; (load-theme 'atom-one-dark t)

;; fonts:
;; (set-face-attribute 'default nil :family "DejaVu Nerd Font Mono")
;; (set-face-attribute 'default nil :height (* 12 10))
(set-face-attribute 'default nil :family "BitstromWera Nerd Font")
(set-face-attribute 'default nil :height (* 12 10))

;; darkest night mode:
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

;; ui and base colors:
;; (setq theme-color-accent  "#ff6000")
(setq theme-color-accent  "#4682B4")
(setq theme-color-level-1 "#1D1F21")
(setq theme-color-level-2 "#373B41")
(setq theme-color-level-3 "#C5C8C6")

;; common colors:
;; (setq theme-color-red     "maroon")
;; (setq theme-color-green   "SeaGreen")
;; (setq theme-color-yellow  "goldenrod")
;; (setq theme-color-blue    "SteelBlue")
;; (setq theme-color-magenta "violet")
;; (setq theme-color-cyan    "DarkCyan")
;; (setq theme-color-gray    "grey")

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size '262144)

;; rainbow style syntax highlighting:
(global-color-identifiers-mode t)

;; toggle selective highlighting of patterns (hi-lock-mode):
(global-hi-lock-mode t)

;; (highlight-operators-mode t)
(global-highlight-parentheses-mode t)
(set-face-attribute 'hi-yellow nil :foreground "#FAF0E6" :background "#1A004E")

;; minor mode that highlights things at point:
(global-highlight-thing-mode t)

;; make ugly glyphs from greek letters?
;;(global-prettify-symbols-mode t)

;; highlight line:
(global-hl-line-mode t)
(set-face-background 'highlight "#0D0D0D")

;; opacity:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; you can use the following snippet after you’ve set the alpha as
;; above to assign a toggle to “c-c t”:
(defun toggle-transparency ()
  "Toggle transparency."
  (interactive "Toggle transparency.")
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; a general transparency function:
(defun transparency (value)
  "Set the transparency of the frame window.  VALUE 0=transparent/100=opaque."
  (interactive "Transparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; the fringe:
;; the fringe is like a vertical ruler that runs from the top to the
;; bottom of the buffer -- the left fringe is sandwiched between the
;; line numbers and the text. it can be invisible if it is the same
;; color as the default background of the user, or it can be a
;; different color.
(setq-default left-fringe-width  10)
(setq-default right-fringe-width  0)
(set-face-attribute 'fringe nil :background "#000000")

;; fullscreen: this is bound to f11 in Emacs 24.4::
;;(toggle-frame-fullscreen)

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

;; none of these please:
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)

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

;; show date and time in mode-line:
;; (setq display-time-day-and-date t )
;; (setq display-time-24hr-format t)
;; (display-time)

;;
;;   _ \   __| _` |
;;  (   | |   (   |
;; \___/ _|  \__, |
;;           |___/

(require 'org-faces)
;; load faces first, then customise:
(with-eval-after-load 'org-faces
  ;; org headers become bold:
  (set-face-attribute 'org-level-1 nil :bold t)
  (set-face-attribute 'org-level-2 nil :bold t)
  (set-face-attribute 'org-level-3 nil :bold t)
  (set-face-attribute 'org-level-4 nil :bold t)
  (set-face-attribute 'org-level-5 nil :bold t)
  (set-face-attribute 'org-level-6 nil :bold t)
  (set-face-attribute 'org-level-7 nil :bold t)
  (set-face-attribute 'org-level-8 nil :bold t)
  (set-face-attribute 'org-level-1 nil :foreground "#FAF0E6" :background "#4682B4")
  (set-face-attribute 'org-level-1 nil :background "#4682B4")

  ;; org mode colours:
  (setq org-fontify-quote-and-verse-blocks t)
  (set-face-attribute 'org-quote nil :italic t)
  (set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
  (set-face-attribute 'org-code nil :background "#0b0d0f")
  (set-face-attribute 'org-block nil :background "#0b0d0f")
  (set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)

  ;; table (header) is same as block (begin line):
  (set-face-attribute 'org-table nil :background "#0b0d0f")
  (set-face-attribute 'org-table-header nil :foreground "#787787" :background "#161a1f" :bold t)

  ;; magic comments face:
  (set-face-attribute 'org-meta-line nil :bold t)
  )

;; org mode emphasis styles:
(setq org-emphasis-alist
      (quote
       (
        ("*" bold)
        ("/" italic)
        ("_" underline)
        ("-" overline)
        ("=" org-verbatim org-verbatim)
        ("~" org-code org-code)
        ("+" (:strike-through t))
        )))


;;            |   |  _)
;;   __|  _ \ __| __| | __ \   _` |  __|
;; \__ \  __/ |   |   | |   | (   |\__ \
;; ____/\___|\__|\__|_|_|  _|\__, |____/
;;                           |___/

;; ranger



;; https://www.gnu.org/software/emacs/manual/html_node/emacs/EditorConfig-support.html
(editorconfig-mode t)

;; flycheck for shellchecking
(global-flycheck-mode t)

;; sort case insensitively:
(setq sort-fold-case nil)

;; revert buffers when the underlying file has changed
(global-auto-revert-mode t)

;; org mode header styles:
(setq org-startup-indented t)

;; code block execution (ask always=t, ask never=nil):
(setq org-confirm-babel-evaluate nil)

;; code block sytax highlighting in org mode:
(setq org-src-fontify-natively t)

;; Turn on the display of the first data row of the table at point in
;; the window header line when this first row is not visible anymore
;; in the buffer:
(setq org-table-header-line-p t)

;; org tempo expands snippets to structures defined in
;; org-structure-template-alist and org-tempo-keywords-alist. for
;; example, < s tab creates a code block:
(require 'org-tempo)

;; BROKEN
;; collapse clutter in org-view-mode (not working):
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (if org-mode
;;                 (org-view-mode +1)
;;               (org-view-mode -1))) nil t)

;; BROKEN
;; collapse clutter in org-view-mode (not working):
;; (add-hook 'org-mode-hook (lambda ()
;;                            ('org-view-mode 1)
;;                            (org-view-edit-on 1)))

;; no welcome message please:
(setq inhibit-startup-message t)

;; inital scratch text:
(setq initial-scratch-message "")

;; save and restore my buffers every time:
;; (desktop-save-mode 1)

;; scrolling
(setq scroll-margin 5 scroll-conservatively 0 )
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01 )

;; scrolling mouse:
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; scroll three lines at a time per default
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse nil) ;; scroll window under mouse

;; interactively do things mode, this is done with vertico now.
;; (require 'ido)
;; (ido-mode t)

;; emacs paste on line curser (not mouse):
(setq mouse-yank-at-point t)


;; Always show color as color
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode t)))
(my-global-rainbow-mode t)

;; subword mode (camelCase mode):
;; (global-subword-mode t)

;; i hate tabs!
(setq-default indent-tabs-mode nil)

;; refresh buffers on change:
(global-auto-revert-mode t)

;; auto break lines in paragraphs:
;; add-hook 'text-mode-hook 'turn-on-auto-fill)

;; get rid of whitespaces:
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ecil mode:
;; (evil-mode t)

;; all completion styles, including vertico:
(setq completion-styles '(orderless substring partial-completion flex))

;; vertico ripgrep embark: its over 9000:
(vertico-mode t)
(vertico-mouse-mode t)

;; In case you want to use Vertico to show the completion candidates
;; of completion-at-point and completion-in-region, you can use the
;; function consult-completion-in-region provided by the Consult
;; package.
(setq completion-in-region-function #'consult-completion-in-region)



;; always use buffers or enable vertico-multiform and select for each type:
;; (vertico-buffer-mode)
(vertico-multiform-mode)

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

;; Enable cycling for 'vertico-next/previous':
(setq vertico-cycle t)

(setq all-the-icons-completion-mode t)
(setq all-the-icons-dired-mode t)

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

;; electric pairs:
(electric-pair-mode)

;;  |
;;  |  /  _ \ |   |  __|
;;    <   __/ |   |\__ \
;; _|\_\\___|\__, |____/
;;           ____/

;; cycle inside the list of completions (like vanilla emacs and zsh),
;; cycle back with 'S-Tab' which is '<backtab>' whyever:
(keymap-set vertico-map "TAB" 'vertico-next)
(keymap-set vertico-map "<backtab>" 'vertico-previous)

;; consult, and its over 9000 modes:
(define-key global-map (kbd "M-f") 'consult-flycheck)
(define-key global-map (kbd "M-m") 'consult-flymake)
(define-key global-map (kbd "M-i") 'consult-flyspell)
(define-key global-map (kbd "M-s") 'consult-line)

(define-key global-map (kbd "M-/") 'consult-fd)
(define-key global-map (kbd "M-\\") 'consult-ripgrep)

;; possible actions with shortkeys
(define-key global-map (kbd "M-a") 'embark-act)

;; eranger
(define-key global-map (kbd "M-r") 'ranger)

;; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; custom keyboard shortcuts:
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-C w") 'delete-trailing-whitespace)

;; eval .emacs:
(global-set-key (kbd "C-c r") (lambda() (interactive) (load-file "~/.emacs")))

;; open various configs:
(global-set-key (kbd "C-c a") (lambda() (interactive) (find-file "~/.config/awesome/rc.lua")))
(global-set-key (kbd "C-c e") (lambda() (interactive) (find-file "~/.emacs")))
                                        ;(global-set-key (kbd "C-c l") (lambda() (interactive) (find-file "~/.config/lf/lfrc")))
                                        ;(global-set-key (kbd "C-c R") (lambda() (interactive) (find-file "~/.config/ranger/rifle.conf")))
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

;; this does not work, since <mouse-8> and <mouse-9> are intercepted
;; by xbindkeys and mapped to "alt-left" / "alt-right" to work with
;; lf!
;; (global-set-key (kbd "<mouse-8>") (kbd "C-v"))
;; (global-set-key (kbd "<mouse-9>") (kbd "M-v"))

;; ?
;; (define-key (current-local-map) (kbd "<mouse-8>")
;;             (lookup-key (current-local-map) (kbd "C-v")))

;;   _|                  |  _)
;;  |   |   | __ \   __| __| |  _ \  __ \   __|
;;  __| |   | |   | (    |   | (   | |   |\__ \
;; _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/
;;

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

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

(defun org-add-electric-pairs ()
  "Enable electric-pair-pairs automatically in 'org-mode'."
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'org-mode-hook 'org-add-electric-pairs)

(defun only-if-use-region (func &rest args)
  "Only insert electric pairs (FUNC ARGS) when selecting something, not on single key press."
  (if (use-region-p)
      (apply func args)))
(advice-add 'electric-pair-post-self-insert-function :around 'only-if-use-region)

;; org-view-mode gets rid of the markup, but is read only:
(global-set-key (kbd "C-c v") 'org-view-mode)

;;  |
;;  |  _` | __ \   _` |
;;  | (   | |   | (   |
;; _|\__,_|_|  _|\__, |
;;               |___/

;; Start auto-complete-mode:
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (auto-complete-mode 1))))

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

;; If you’re using a Mac, you may need to add the following Elisp code
;; to your config file as well in order for Flyspell to pick up the
;; two-finger clicks (right-clicks):
;; (eval-after-load "flyspell"
;;   '(progn
;;      (define-key flyspell-mouse-map [mouse-3] #'flyspell-correct-word)))


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

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
