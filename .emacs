;;       |          _|  _| 
;;   __| __| |   | |   |   
;; \__ \ |   |   | __| __| 
;; ____/\__|\__,_|_|  _|

;; fun:
;;(Global-set-key (kbd "C-x C-s") 'spook)

;; Melpa
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
 '(package-selected-packages
   '(cuda-mode csv-mode spinner string-inflection json-mode yaml-mode ample-regexps fuzzy auto-complete-auctex luarocks highlight-unique-symbol highlight-defined highlight-function-calls highlight-thing highlight-symbol highlight-parentheses highlight-operators highlight highlight-blocks highlight-escape-sequences highlight-quoted highlight-numbers color-identifiers-mode lua-mode flycheck markdown-mode company auto-complete auctex matlab-mode live-py-mode rainbow-identifiers rainbow-mode ess auto-correct))
 '(warning-suppress-log-types '((auto-save))))

;; highlighting lock:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-yellow ((t (:foreground "black" :background "GoldenRod")))))

;; no welcome message please:
(setq inhibit-startup-message t)

;; inital scratch text:
(setq initial-scratch-message "")

;; theme:
;;(load-theme 'atom-one-dark t)

;; make it easy on eyes:
(set-foreground-color "Linen")
(set-background-color "Black")
(set-face-foreground 'default "Linen")
(set-face-background 'default "Black")
(set-face-foreground 'font-lock-string-face "DarkMagenta")
;;(set-face-foreground 'font-lock-comment-face "SaddleBrown")
(set-face-foreground 'font-lock-comment-face "Teal")
;;(set-face-foreground 'font-lock-comment-face "DarkSlateGray")
(set-face-attribute 'cursor nil :background "Magenta")

;; UI and base colors:
;;(setq theme-color-accent  "#ff6000")
(setq theme-color-accent  "DarkCyan")
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
(global-color-identifiers-mode 't)
(global-font-lock-mode 't)
(setq font-lock-maximum-decoration 't)
(setq font-lock-maximum-size '262144)
(global-hi-lock-mode 't)
(global-highlight-operators-mode 't)
(global-highlight-parentheses-mode 't)
(global-highlight-thing-mode 't)

;; make ugly glyphs from greek letters?
;;global-prettify-symbols-mode 't)

;; highlight line:
;;(global-hl-line-mode 't)
;;(setq hl-line-face 'hl-line)
;;(setq highlight-current-line-globally t)
;;(setq highlight-current-line-high-faces nil)
;;(setq highlight-current-line-whole-line nil)
;;(setq hl-line-face (quote highlight))




;; opacity:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(80 . 80))
(add-to-list 'default-frame-alist '(alpha . (80 . 80)))

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

;; A general transparency function:
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
(set-face-attribute 'fringe nil :background "black")

;; fullscreen: this is bound to f11 in Emacs 24.4::
;(toggle-frame-fullscreen) 

;; up/downcase region:
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; none of these please:
(scroll-bar-mode '0)
(tool-bar-mode '0)
(menu-bar-mode '0)

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
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; hide the mode-line in every buffer by default:
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; save and restore my buffers every time:
;;(desktop-save-mode 1)

;; show date and time in mode-line:
;;(setq display-time-day-and-date t )
;;(setq display-time-24hr-format t)
;;(display-time)

;; fonts:
;;(set-face-attribute 'default nil :family "DejaVu Nerd Font Mono")
;;(set-face-attribute 'default nil :height (* 12 10))
(set-face-attribute 'default nil :family "BitstromWera Nerd Font")
(set-face-attribute 'default nil :height (* 12 10))


;; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; scrolling
(setq
 scroll-margin 5 ;; when to start scrolling
 scroll-conservatively 0
 )
(setq-default
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 )

(defun scroll-down-keep-cursor ()
  ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1)
  )

(defun scroll-up-keep-cursor ()
  ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1)
  )

(global-set-key (kbd "C-,") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-.") 'scroll-up-keep-cursor)
;;(global-set-key (kbd "C-c d") 'insert-current-date)

;; scrolling mouse:
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq Mouse-Wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Interactively Do Things mode:
(require 'ido)
(ido-mode t)

;; custom keyboard shortcuts:
(global-set-key (kbd "C-c m") 'compile)

;; current date:
(defun insert-current-date() (interactive)
       (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %k:%M')"))
       )
(global-set-key (kbd "C-c d") 'insert-current-date)

;; convenience:
(global-set-key (kbd "C-c r") (lambda() (interactive) (load-file "~/.emacs")))
(defun em ()
  (interactive)
  (find-file "~/.emacs")
  )

;; emacs paste on line curser (not mouse):
(setq mouse-yank-at-point t)

;; printer command
(setq lpr-command "lp")
(setq lpr-add-switches nil)

;; switch window with tab:
(global-set-key [C-tab] 'other-window)
(global-set-key [C-C-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)
                  )
                )

;; always show color as color:
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

;; subword mode (camelcase mode):
(global-subword-mode 1)

;; i hate tabs!
(setq-default indent-tabs-mode nil)

;; refresh buffers on change:
(global-auto-revert-mode t)

;; auto break lines in paragraphs:
;; add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; now set it as default un/compact block function:
(global-set-key (kbd "M-q") 'fill-or-unfill)

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
;(global-set-key (kbd "C-c C-c") 'octave-send-region)

;; Latex mode:
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
;        (TeX-command-sequence '("Arara" "View") t))))

;;                  | |      
;;   __| __ \   _ \ | |  __| 
;; \__ \ |   |  __/ | |\__ \ 
;; ____/ .__/ \___|_|_|____/ 
;;      _|                   

;; flyspell mode:
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; If you’re using a Mac, you may need to add the following Elisp code to your config file as well in order for Flyspell to pick up the two-finger clicks (right-clicks):
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

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
;;(unless (file-exists-p ispell-personal-dictionary)
;;(write-region "" nil ispell-personal-dictionary nil 0))

;; (global-set-key (kbd "<C>-<mouse-8>") (kbd "C-v"))
;; (global-set-key (kbd "<C>-<mouse-9>") (kbd "M-v"))

;; (global-set-key (kbd "<M>-<left>") (kbd "C-v"))
;; (global-set-key (kbd "<M>-<right>") (kbd "M-v"))

;; (define-key (current-local-map) (kbd "mouse-8")
;;   (lookup-key (current-local-map) (kbd "C-v")))

(setenv "LANG" "en_US.UTF-8")
