;;       |          _|  _|
;;   __| __| |   | |   |
;; \__ \ |   |   | __| __|
;; ____/\__|\__,_|_|  _|

;; fun:
;;(Global-set-key (kbd "C-x C-s") 'spook)

;; melpa
(require 'package)
(package-initialize)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" default))
 '(org-babel-load-languages
   '((C . t)
     (R . t)
     (emacs-lisp . t)
     (fortran . t)
     (gnuplot . t)
     (haskell . t)
     (latex . t)
     (lisp . t)
     (lua . t)
     (makefile . t)
     (octave . t)
     (org . t)
     (python . t)
     (sed . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))
 '(package-selected-packages
   '(yaml-pro org-view-mode org-modern hl-block-mode atom-one-dark-theme timu-caribbean-theme php-mode org-auto-tangle gnuplot blacken poly-ansible poly-markdown polymode mmm-mode cuda-mode csv-mode spinner string-inflection json-mode ample-regexps fuzzy auto-complete-auctex luarocks highlight-unique-symbol highlight-defined highlight-function-calls highlight-thing highlight-symbol highlight-parentheses highlight-operators highlight highlight-blocks highlight-escape-sequences highlight-quoted highlight-numbers color-identifiers-mode lua-mode flycheck markdown-mode company auto-complete auctex matlab-mode live-py-mode rainbow-identifiers rainbow-mode ess auto-correct))
 '(warning-suppress-log-types '((auto-save))))

;; markdown/org translator:
(add-to-list 'load-path "~/.emacs.d/others/org-pandoc-import")
(require 'org-pandoc-import)
(require 'org-pandoc-import-transient)


;;  |   |
;;  __| __ \   _ \ __ `__ \   _ \
;;  |   | | |  __/ |   |   |  __/
;; \__|_| |_|\___|_|  _|  _|\___|
;;

;; readymade themes:
;;(load-theme 'timu-caribbean t)
;;(customize-set-variable 'timu-caribbean-org-intense-colors t)
;;(load-theme 'atom-one-dark t)

;; fonts:
;;(set-face-attribute 'default nil :family "DejaVu Nerd Font Mono")
;;(set-face-attribute 'default nil :height (* 12 10))
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

;; ui and base colors:
;;(setq theme-color-accent  "#ff6000")
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

;; syntax highlighting:
(global-color-identifiers-mode t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size '262144)
(global-hi-lock-mode t)
(global-highlight-operators-mode t)
(global-highlight-parentheses-mode t)
(global-highlight-thing-mode t)
(set-face-attribute 'hi-yellow nil :foreground "#FAF0E6" :background "#1A004E")

;; make ugly glyphs from greek letters?
;;global-prettify-symbols-mode t)

;; highlight line:
;(global-hl-line-mode t)
(set-face-background 'highlight "#0D0D0D")

;; opacity:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; you can use the following snippet after you’ve set the alpha as
;; above to assign a toggle to “c-c t”:
(defun toggle-transparency ()
  (interactive)
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
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
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
  "minor mode to hide the mode-line in the current buffer."
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
;;(setq display-time-day-and-date t )
;;(setq display-time-24hr-format t)
;;(display-time)

;; org headers become bold:
(set-face-attribute 'org-level-1 nil :bold t)
(set-face-attribute 'org-level-2 nil :bold t)
(set-face-attribute 'org-level-3 nil :bold t)
(set-face-attribute 'org-level-4 nil :bold t)
(set-face-attribute 'org-level-5 nil :bold t)
(set-face-attribute 'org-level-6 nil :bold t)
(set-face-attribute 'org-level-7 nil :bold t)
(set-face-attribute 'org-level-8 nil :bold t)
;; (set-face-attribute 'org-level-1 nil :foreground "#FAF0E6" :background "#4682B4")
;; (set-face-attribute 'org-level-1 nil :background "#4682B4")

;; org mode colours:
(setq org-fontify-quote-and-verse-blocks t)
(set-face-attribute 'org-quote nil :italic t)
(set-face-attribute 'org-verbatim nil :foreground "#000000" :background "#787787")
(set-face-attribute 'org-code nil :background "#0b0d0f")
(set-face-attribute 'org-block nil :background "#0b0d0f")
(set-face-attribute 'org-block-begin-line nil :background "#161a1f" :bold t)

;; table (header) is same as block (begin line):
(set-face-attribute 'org-table nil :background "#0b0d0f")
(set-face-attribute 'org-table-header nil
:foreground "#787787"
:background "#161a1f"
:bold t
)

;; magic comments face:
(set-face-attribute 'org-meta-line nil :bold t)

;; org mode emphasis styles:
(setq org-emphasis-alist
      (quote (
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

;; revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

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

;; collapse clutter in org-view-mode (not working):
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (if org-mode
;;                 (org-view-mode +1)
;;               (org-view-mode -1))) nil t)

;; collapse clutter in org-view-mode (not working):
;; (add-hook 'org-mode-hook (lambda ()
;;                            ('org-view-mode 1)
;;                            (org-view-edit-on 1)))

;; no welcome message please:
(setq inhibit-startup-message t)

;; inital scratch text:
(setq initial-scratch-message "")

;; save and restore my buffers every time:
;;(desktop-save-mode 1)

;; scrolling
(setq scroll-margin 5 scroll-conservatively 0 )
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01 )

;; scrolling mouse:
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; scroll three lines at a time per default
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse nil) ;; scroll window under mouse

;; interactively do things mode:
(require 'ido)
(ido-mode t)

;; emacs paste on line curser (not mouse):
(setq mouse-yank-at-point t)

;; always show color as color:
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode t)))
(my-global-rainbow-mode t)

;; subword mode (camelcase mode):
(global-subword-mode t)

;; i hate tabs!
(setq-default indent-tabs-mode nil)

;; refresh buffers on change:
(global-auto-revert-mode t)

;; auto break lines in paragraphs:
;; add-hook 'text-mode-hook 'turn-on-auto-fill)

;; get rid of whitespaces:
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;  |
;;  |  /  _ \ |   |  __|
;;    <   __/ |   |\__ \
;; _|\_\\___|\__, |____/
;;           ____/

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
(global-set-key (kbd "C-c l") (lambda() (interactive) (find-file "~/.config/lf/lfrc")))
(global-set-key (kbd "C-c R") (lambda() (interactive) (find-file "~/.config/ranger/rifle.conf")))
(global-set-key (kbd "C-c x") (lambda() (interactive) (find-file "~/.xinitrc")))
(global-set-key (kbd "C-c X") (lambda() (interactive) (find-file "~/.Xresources")))
(global-set-key (kbd "C-c z") (lambda() (interactive) (find-file "~/.zshrc")))

;; printer command:
(setq lpr-command "lp")
(setq lpr-add-switches nil)

;; switch window:
;;(global-set-key [C-tab] 'other-window)
;;(global-set-key [C-C-tab] (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-<mouse-8>") 'next-buffer)
(global-set-key (kbd "M-<mouse-9>") 'previous-buffer)

;; page down/up:
(global-set-key (kbd "M-<wheel-down>") (kbd "C-v"))
(global-set-key (kbd "M-<wheel-up>") (kbd "M-v"))

;; this does not work, since <mouse-8> and <mouse-9> are intercepted
;; by xbindkeys and mapped to "alt-left" / "alt-right" to work with
;; lf!
;;(global-set-key (kbd "<mouse-8>") (kbd "C-v"))
;;(global-set-key (kbd "<mouse-9>") (kbd "M-v"))

;; ?
;; (define-key (current-local-map) (kbd "<mouse-8>")
;;             (lookup-key (current-local-map) (kbd "C-v")))


;;   _|                  |  _)
;;  |   |   | __ \   __| __| |  _ \  __ \   __|
;;  __| |   | |   | (    |   | (   | |   |\__ \
;; _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/
;;

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

;; current date:
(defun insert-current-date() (interactive)
       (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %k:%M')") )
       )
(global-set-key (kbd "C-c d") 'insert-current-date)

;; un/compact block:
(defun fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like
`fill-paragraph' or “unfill”.  When there is a text selection, act on
the selection, else, act on a text block separated by blank lines.
URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
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

;; electric-pair mode (mark region, then press "*":
(electric-pair-mode t)
(defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=)
                             (?\_ . ?\_) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'org-mode-hook 'org-add-electric-pairs)

;; only insert electric pairs when selecting something, not on single
;; key press:
(electric-pair-mode)
(defun only-if-use-region (func &rest args)
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

;; start auto-complete-mode:
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
;;(global-set-key (kbd "C-c C-c") 'octave-send-region)

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
;;(unless (file-exists-p ispell-personal-dictionary)
;;(write-region "" nil ispell-personal-dictionary nil 0))

(setenv "LANG" "en_US.UTF-8")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
