;;  (use-package better-defaults
;;    :ensure t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



;; Backup files to single directory
;; stackoverflow.com/questions/2680389/how-to-remove-all-files-ening-with-made-by-emacs
;; stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(defvar --backup-directory (concat user-emacs-directory (file-name-as-directory "backup"))
  "--backup-directory is user defined location inside the user-emacs-directory, where all backup related files and folders are stored.

These include, but are not limited to all autosaves and backups.")
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(defvar --history-directory (concat user-emacs-directory (file-name-as-directory "history"))
  "--history-directory is user defined location inside the user-emacs-directory, where all backup related files and folders are stored.

These include, but are not limited to tramp places, and url savedplace, savehist, recentf, undo and history as well as the flyspell personal dictionary.")
(if (not (file-exists-p --history-directory))
    (make-directory --history-directory t))

(setq backup-directory-alist`((".*" . ,--backup-directory))
      auto-save-file-name-transforms `((".*" ,--backup-directory t))
      auto-save-list-file-prefix --backup-directory
      backup-by-copying t    ; Dont't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 5    ; How many of the newsest versions to keep
      kept-old-versions 5    ; How many of the old
      vc-make-backup-files t ; Make backups of files, even if they're in version control
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 20   ; idle seconds before autosave (default 30)
      auto-save-interval 300 ; num keystrokes between autosaves (default 300)
      )
;; To completely remove backups
;;(setq make-backup-files nil)

;; saveplace remembers you location in a file when saving
(use-package saveplace
  :ensure t
  :config (progn
            (setq save-place-file (expand-file-name "saveplace" --history-directory))
            ;; Activate it across all buffers
            (setq-default save-place t)
            (setq save-place-limit nil)))

;; savehist keeps track of some history
(use-package savehist
  :ensure t
  :config (progn
            (setq savehist-additional-variables
                  ;; search entries
                  '(kill-ring search-ring regex-search-ring)
                  ;; save every minute
                  savehist-autosave-interval 60
                  ;; keep homedir ~/ clean
                  savehist-file (expand-file-name "savehist" --history-directory))
            (savehist-mode t)))

;; save recently-used files
(use-package recentf
  :ensure t
  :bind (("C-x C-r" . recentf-open-files))
  :config (progn
            (setq recentf-save-file (expand-file-name "recentf" --history-directory)
                  ;; Save list of recent files every ~ 8 mins
                  recentf-max-saved-items 500
                  recentf-max-menu-items 15
                  recentf-exclude '("/auto-install" ".recentf" "/repos/" "/elpa/"
                                    "COMMIT_EDITMSG" ".gz" "~$" "/tmp/")
                  ;; If Emacs has been idle for 10 mins, cleanup recent files
                  recentf-auto-cleanup 600)
            (recentf-mode t)

            ;; May get an error as this function already exists...?
            ;; Taken verbertim from dakrone
            (defun recentf-save-list ()
              "Save the recent list.

Load the list from the file specified by `recentf-save-file', merge the changes of your current session, and save it back to the file."
              (interactive)
              (let ((instance-list (copy-list recentf-list)))
                (recentf-load-list)
                (recentf-merge-with-default-list instance-list)
                (recentf-write-list-to-file)))

            (defun recentf-merge-with-default-list (other-list)
              "Add all items from `other-list' to `recentf-list'."
              (dolist (oitem other-list)
                ;; add-to-list already checks for equal'ity
                (add-to-list 'recentf-list oitem)))

            (defun recentf-write-list-to-file ()
              "Write the recent files list to file.

Uses `recentf-list' as the list and `recentf-save-file' as the file to write to."
              (condition-case error
                  (with-temp-buffer
                    (erase-buffer)
                    (set-buffer-file-coding-system recentf-save-file-coding-system)
                    (insert (format recentf-save-file-header (current-time-string)))
                    (recentf-dump-variable 'recentf-list recentf-max-saved-items)
                    (recentf-dump-variable 'recentf-filter-changer-current)
                    (insert "\n \n;;; Local Variables:\n"
                            (format ";;; coding: %s\n" recentf-save-file-coding-system)
                            ";;; End:\n")
                    (write-file (expand-file-name recentf-save-file))
                    (when recentf-save-file-modes
                      (set-file-modes recentf-save-file recentf-save-file-modes))
                    nil)
                (error
                 (warn "recentf mode: %s" (error-message-string error)))))))

(use-package undo-tree
  :diminish ""
  :ensure t
  :init (global-undo-tree-mode t)
  :config (progn
            ;; Save undo history between sessions, if you have an undo-dir
            (setq undo-tree-auto-save-history
                  (file-exists-p
                   (concat --history-directory "undo"))
                  undo-tree-history-directory-alist
                  ;; Put undo-history files in a directory, if it exists.
                  (let ((undo-dir (concat --history-directory "undo")))
                    (and (file-exists-p undo-dir)
                         (list (cons "." undo-dir)))))
            (setq undo-tree-visualizer-timestamps t)
            (setq undo-tree-visualizer-diff t))
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
;;         ("C-c l" . undo-tree-switch-branch)
;;         ("C-c C-;" . undo-tree-visualize) ;; already bound to C-x C-u
         ))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(use-package smart-mode-line
  :ensure t)
;;    :config (progn
(setq sml/no-confirm-load-theme t)
;;(sml/apply-theme 'powerline)
(setq sml/theme 'dark)
(sml/setup)

(use-package nyan-mode
  :ensure t
  :config (nyan-mode t))

;; (setq-default
;;  mode-line-format
;;  '(; Position, including warning for 80 columns
   ;; (:propertize "%4l:" face mode-line-position-face)
   ;; (:eval (propertize "%3c" 'face
   ;;                    (if (>= (current-column) 80)
   ;;                        'mode-line-80col-face
   ;;                      'mode-line-position-face)))
   ;; ;; emacsclient [default -- keep?]
   ;; mode-line-client
   ;; " "
   ;; ;; read-only or modified status
   ;; (:eval
   ;;  (cond (buffer-read-only
   ;;         (propertize " RO " 'face 'mode-line-read-only-face))
   ;;        ((buffer-modified-p)
   ;;         (propertize " ** " 'face 'mode-line-modified-face))
   ;;        (t " ")))
   ;; " "
   ;; ;; directory and buffer/file name
   ;; (:propertize (:eval (shorten-directory default-directory 30))
   ;;              face mode-line-folder-face)
   ;; (:propertize "%b"
   ;;              face mode-line-filename-face)
   ;; ;; narrow [default -- keep?]
   ;; ;;" %n "
   ;; ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   ;; (vc-mode vc-mode)
   ;; "  %["
   ;; (:propertize mode-name face mode-line-mode-face)
   ;; "%] "
   ;; (:eval (propertize (format-mode-line minor-mode-alist)
   ;;                    'face 'mode-line-minor-mode-face))
   ;; (:propertize mode-line-process
   ;;              face mode-line-process-face)
   ;; " "
   ;; mode-line-misc-info is better than Amit's version
   ;;mode-line-misc-info
   ;;"  "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;;(:eval (when nyan-mode (list (nyan-create))))
   ;;))

;; ;; Helper function
;; (defun shorten-directory (dir max-length)
;;   "Show up to `max-length' characters of a directory name `dir'."
;;   (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;         (output ""))
;;     (when (and path (equal "" (car path)))
;;       (setq path (cdr path)))
;;     (while (and path (< (length output) (- max-length 4)))
;;       (setq output (concat (car path) "/" output))
;;       (setq path (cdr path)))
;;     (when path
;;       (setq output (concat ".../" output)))
;;     output))

;; ;; Extra mode line faces
;; (make-face 'mode-line-read-only-face)
;; (make-face 'mode-line-modified-face)
;; (make-face 'mode-line-folder-face)
;; (make-face 'mode-line-filename-face)
;; (make-face 'mode-line-position-face)
;; (make-face 'mode-line-mode-face)
;; (make-face 'mode-line-minor-mode-face)
;; (make-face 'mode-line-process-face)
;; (make-face 'mode-line-80col-face)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "gray60" :background "gray20"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray20" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;                     :foreground "gray80" :background "gray40"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray40" :style nil))

;; (set-face-attribute 'mode-line-read-only-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#4271ae"
;;                     :box '(:line-width 2 :color "#4271ae"))
;; (set-face-attribute 'mode-line-modified-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#c82829"
;;                     :background "#ffffff"
;;                     :box '(:line-width 2 :color "#c82829"))
;; (set-face-attribute 'mode-line-folder-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "gray60")
;; (set-face-attribute 'mode-line-filename-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#eab700"
;;                     :weight 'bold)
;; (set-face-attribute 'mode-line-position-face nil
;;                     :inherit 'mode-line-face
;;                     :family "Menlo" :height 100)
;; (set-face-attribute 'mode-line-mode-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "gray80")
;; (set-face-attribute 'mode-line-minor-mode-face nil
;;                     :inherit 'mode-line-mode-face
;;                     :foreground "gray40"
;;                     :height 110)
;; (set-face-attribute 'mode-line-process-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#718c00")
;; (set-face-attribute 'mode-line-80col-face nil
;;                     :inherit 'mode-line-position-face
;;                     :foreground "black" :background "#eab700")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Transparency Settings
;;;;;;;;;;;;;;;;;;;;;;;;
;; www.emacswiki.org/emacs/TransparentEmacs
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(90 50) )
(add-to-list 'default-frame-alist '(alpha 90 50))

;; Toggle transparency with C-c t - See keys.el
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 50))))

;; Add Paredit for all the lispy languages
(use-package paredit
  :ensure t
  :diminish "()"
  :config (progn
            ;;(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
            (define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)))

;; Highlight Matching Parentheses
;;(show-paren-mode t)
;;(blink-matching-paren t)
;;(electric-pair-mode t)
(use-package smartparens
  :ensure t
  :diminish ""
;;  :bind (("M-9" . sp-backward-sexp)
;;         ("M-0" . sp-forward-sexp))
  :config (progn
            (use-package smartparens-config)
            (add-hook 'prog-mode-hook #'turn-on-smartparens-mode)
            ;; Turn on showing match for clojure ad elisp
            ;; Not really necessary as shwo-paren-mode and electric pair should handle this
            (add-hook 'clojure-mode-hook #'turn-on-show-smartparens-mode)
            (add-hook 'emacs-lisp-mode-hook #'turn-on-show-smartparens-mode)
            (add-hook 'java-mode-hook #'turn-on-show-smartparens-mode)
            (add-hook 'c-mode-hook #'turn-on-show-smartparens-mode)
            (add-hook 'c++-mode-hook #'turn-on-show-smartparens-mode)

            (add-to-list 'sp-sexp-suffix '(es-mode regex ""))

            (setq sp-base-key-bindings 'paredit)
            ;;(setq sp-autoskip-closing-pair 'always)
            (sp-use-paredit-bindings)
            ;;(smartparens-global-mode t)

            ;; See dakrone's settings.org for keybindings
            ;;(define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
            ;;(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)

            ;; Remove pairing '' in elisp and minibuffer
            ;; These seems to be the default behaviour of Smart Paren
            ;;(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
            ;;(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
            ))
;; Rainbow-Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'org-mode-hook 'rainbow-delimiters-mode)))

;; Make cursor invisible when typing
(setq make-pointer-invisible t)

;; Line highlightng and truncation
(defun my/turn-on-truncate-hl-lines ()
  "Custom mode hook to turn on highlight lines mode and toggle truncate lines.

Very useful for modes such as Dired, iBuffer and list-packages for example"
  (interactive)
  (toggle-truncate-lines t)
  (hl-line-mode t))

(dolist (myhook '(bookmark-bmenu-mode-hook flycheck-error-list-mode-hook package-menu-mode-hook ibuffer-mode-hook dired-mode-hook package-menu-mode-hook gnus-group-mode-hook org-agenda-mode-hook))
  (add-hook myhook #'my/turn-on-truncate-hl-lines))

;; Syntax Highlighting
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t)       ; GNU Emacs
  (setq font-lock-auto-fontify t)   ; XEmacs
  (setq font-lock-maximum-size t))

;; Flash on error
(setq visible-bell t)

;; Generic Mode
(require 'generic-x)
;; www.emacswiki.org/emacs/GenericMode
;; Syntax highlighting for obscure languages i.e. batch, ini, command, registry, samba, etc
;; For editting obscure programs add the followig to the first line
;; # -*- mode: default-generic -*-

;; Numbers and constants
;; stackoverflow.com/questions/14980008/emacs-synyax-highlight-numers-not-part-of-words-with-regex
;; Match arbitrary floating points
;;   [0-9.]
;; Match zero or more digits, followed by optional period, which must be followed at least one digit
;;   [0-9]*\.?[0-9]+
;; Optionally match leading signs
;;   [-+]?
;; Match exponents using optional group
;;   \(?:...\)
;; Limit match to numbers so that they are not part of any idenifiers - One at either end of expression to match whole word.
;; \b...\b
;; Remember Emacs-Lisp requires regex be put into strings and hence must double up on backslashes

;; Add number keywords to all derived modes of prog-mode:
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\_<[-+]?\\b[0-9]*\\.?[0-9]*\\(?:[eE][-+]?[0-9]*\\.?[0-9]+\\)?\\b\\_>" . font-lock-preprocessor-face)))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(add\\-to\\-list\\|equal\\|\\ list\\ \\|and\\|or\\|message\\|load\\|load-file\\|setq\\|add-hook\\|nil\\|t\\|f\\)\\>" . font-lock-keyword-face)))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)
(use-package bookmark+
  :ensure t
  :config (progn
            (bookmark-bmenu-list)
            (switch-to-buffer "*Bookmark List")))

;; Create a maximised initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Create fullheight but not full width frames on every subsequent frame
;; Screen can be maximized with M-f10
;;(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; global line-wrapping with smart indenting, set specific column fill as wnell
  ;; automatic activation of adaptive-wrap for indetation on invocation of global visual mode
  ;; add an org-mode hook for this as well
  (use-package adaptive-wrap
    ;;:diminish ""
    :ensure t
    :config (progn
              (when (fboundp 'adaptive-wrap-prefix-mode)
                (defun my-activate-adaptive-wrap-prefix-mode ()
                  "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously"
                  (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
                (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))
              ;;(visual-line-mode t)
              ;;(global-visual-line-mode t)
              (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
              (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
              (add-hook 'prog-mode-hook 'turn-on-visual-line-mode)))

;; Some variables are buffer local, so changing them using setq will only change them in a single buffer. Using setq-default will change the buffer local variable's default value.

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(diminish 'auto-fill-function "")

;; Automatically and silently revert files that are changed on disk
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)

(when (boundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode t))

(message "Trying to load clipboard settings...")
(setq x-select-enable-clipboard t)
;; Treat clipboard input as UTF-8 string first, and then as compound test second, etc...
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package alert
  :ensure t
  :config
  (progn (setq alert-default-style 'notifications)))
;; Test
;;(alert "Hello")

(use-package smooth-scolling
  :defer t
  :config (setq smooth-scroll-margin 4))

(use-package expand-region
  :ensure t
  :bind (("C-c e" . er/expand-region)
         ("C-c E" . er/contract-region)))

(use-package with-editor
  :ensure t
  :config (progn
            (add-hook 'shell-mode-hook 'with-editor-export-editor)
            (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package easy-kill
  :ensure t
  :config (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package winner
  :ensure t
  :config (winner-mode t))

;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

(use-package transpose-frame
  :ensure t
  :bind ("C-x |" . transpose-frame)
  :config (transpose-frame t))

;; (defun split-horizontally-not-vertically ()
;;   "if there's only one window (excluding any possibly active minibuffer), then split the current window horizontally."
;;   (interactive)
;;   (if (= (length (window-list nil 'dont-include-minibuffer-even-if-active)) 1)
;;       (split-window-horizontally)))
;; (add-hook 'temp-buffer-setup-hook 'split-horizontally-not-vertically)

;; (defun split-horizontally-for-temp-buffers ()
;;   "Split the window horizontally for temp buffers."
;;   (when (and (one-window-p t)
;;              (not (active-minibuffer-window)))
;;     (split-window-horizontally)))
;; (add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;;(windmove-default-keybindings 'meta)
;; These are bound to the same Prefix as whitespace
(global-set-key (kbd "C-c w j") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w i") 'windmove-up)
(global-set-key (kbd "C-c w k") 'windmove-down)

(use-package guide-key
  :ensure t
  :config (progn
            (setq guide-key/guide-key-sequence '("C-c" "C-x"))
            (setq guide-key/recursive-key-sequence-flag t)
            (setq guide-key/highlight-command-regexp
                  '("helm"
                    ("org" . "orange")
                    ("flyspell" . "green")
                    ("flycheck" . "red")
                    ("rectangle" . "IndianRed")
                    ("register" . "SpringGreen")
                    ("bookmark" . "Burlywood")
                    ("undo-tree" . "Magenta")))
            (guide-key-mode t)))

(defface visible-mark-active ;; put this before (use-package visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")

(use-package visible-mark
  :config (progn
            (setq visible-mark-max 2)
            (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
            (global-visible-mark-mode t)))

(use-package vlf-setup)

(use-package idle-highlight-mode
  :config (progn
            (defun my/turn-on-idle-highlight-mode ()
              "Turn on idle-highlight-mode."
              (interactive)
              (idle-highlight-mode t))
            (add-hook 'prog-mode-hook #'my/turn-on-idle-highlight-mode)
            (add-hook 'org-mode-hook #'my/turn-on-idle-highlight-mode)))

(defun my/browse-last-url-in-buffer ()
  "Search backwards, prompting to open any URL found. Then returning to the original mark."
  (interactive)
  (save-excursion
    (let ((ffap-url-regexp
           (concat
            "\\("
            "news\\(post\\)?:\\|mailto:\\|file:"
            "\\|"
            "\\(ftp\\|https?\\|telnet\\|gopher\\|www||\wais\\)://"
            "\\).")))
      (ffap-next t t))))

(global-set-key (kbd "C-c u") 'my/browse-last-url-in-buffer)

;; Define Some Custom Functions
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region selected."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))

(defun nlisa/kill-region-or-backwards-kill-word ()
  "Kill region from BEG to END if active, otherwise run backwards kill word."
  (interactive)
  (if (and (mark) (use-region-p))
      (kill-region (min (point) (mark)) (max (point) (mark)))
    (delete-region (point) (progn (backward-word) (point)))))

;(bind-key "C-w" 'nlisa/kill-region-or-backwards-kill-word)

;; Unbind Keys
;; (unbind-key "C-o")
(unbind-key "C-x C-c") ;; Unbind kill emacs save sessions, too similar to org bind
(unbind-key "M-\\")
(unbind-key "C-x C-r")
(unbind-key "C--")
;;(unbind-key "M-%")

(use-package elisp
;;  :ensure
  ;;:commands comment-line
  :bind (("C-x C-m" . execute-extended-command)
         ("C-w" . nlisa/kill-region-or-backwards-kill-word)
         ;; Rebind C-w which was kill-region
         ;;("C-x C-k" . kill-region)
         ;;("C-c C-k" . edit-kbd-macro)
         ("C-x b" . ibuffer)
         ;; Bind C-<f5> to Toggle Line Numbers On/Off
         ;; emacs-fu.blogspot.com/2008/12/showing-the-line-numbers.html
         ;; (autoload 'linum-mode "linum" "toggle line numbers on/off" t)
         ("C-<f5>" . linum-mode)
         ("C-c t" . toggle-transparency)
         ;;("M-\\" . auto-complete)
         ("C-<f7>" . compile) ;; Redefined under C/C++
         ("C-<f8>" . gdb)
         ("C--" . text-scale-decrease)
         ("C-+" . text-scale-increase)
         ("C-c C" . comment-or-uncomment-region-or-line)
         ("C-<f1>" . nlisa-threaded-server-start-or-switch)
         ("C-<f2>" . mu4e)
         ("C-<f3>" . djcb-erc-start-or-switch)
         ("C-<f4>" . gnus)
         ;; ("C-s" . isearch-forward-regexp)
         ;; ("C-r" . isearch-backward-regexp)
         ;; ("M-%" . query-replace-regexp) ;; already bound to C-M-%
         ;; ("C-g C-/" . undo)
         ))

;; Bind f1 to mu4e - note that f1 is bound to the help character C-h,
;; so it was redundent in any case
;;(global-set-key [f1] 'mu4e)

(defun my/insert-lod ()
  "Sigh... I really expected more from you."
  (interactive)
  (insert "ಠ_ಠ"))

(global-set-key (kbd "C-x 8 d") 'my/insert-lod)

(defun my/insert-ows ()
  "Sup son...?"
  (interactive)
  (insert "¯\_(ツ)_/¯"))

(global-set-key (kbd "C-x 8 )") 'my/insert-ows)

(defun my/insert-wtf-idk ()
  "Ermmm... WTF? Sorry, I've got no idea."
  (interactive)
  (insert "¯\_(°_O)_/¯"))

(global-set-key (kbd "C-x 8 w") 'my/insert-wtf-idk)

(defun my/insert-flipping-tables ()
  "Are you f*cking kidding me! In case you missed it I'm angry."
  (interactive)
  (insert "(╯°□°）╯︵ ┻━┻"))

(global-set-key (kbd "C-x 8 a") 'my/insert-flipping-tables)

(defun my/insert-return-tables ()
  "Sorry, I overreacted..."
  (interactive)
  (insert "┬──┬◡ﾉ(°-°ﾉ)"))

(global-set-key (kbd "C-x 8 s") 'my/insert-return-tables)

(defun my/insert-respect-tables ()
  "That was totally unncessary and uncalled for."
  (interactive)
  (insert "┬─┬ノ(ಠ_ಠノ)"))

(global-set-key (kbd "C-x 8 r") 'my/insert-respect-tables)

(defun my/insert-finger-guns ()
  "You Sir, Are the man. Thanks."
  (interactive)
  (insert "(☞ﾟヮﾟ)☞"))

(global-set-key (kbd "C-x 8 t ") 'my/insert-finger-guns)

(defun my/insert-blank-exasperation ()
  "Really? Well I don't have anything to say to that. It's not worth th effort."
  (interactive)
  (insert "•_•"))

(global-set-key (kbd "C-x 8 b") 'my/insert-blank-exasperation)

(use-package smart-tab
  :ensure t
  :config (global-smart-tab-mode t))

;; Activate whitespace-mode to view all whitespace and newline characters
;; Cleanup trailing whitespace
(global-set-key (kbd "C-c w s") 'whitespace-mode)
(global-set-key (kbd "C-c w c") 'whitespace-cleanup)

;; Show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace t)))

;; Use space to indent by default
(setq-default indent-tabs-mode nil)

;; Cleanup unused whitespace created by electric-indent-mode (auto-indent on RET) and subsequent RETs
(use-package clean-aindent-mode
  :ensure t)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Guess indentation offset
(use-package dtrt-indent
  :config (progn
            (setq dtrt-indent-verbosity nil)
            (dtrt-indent-mode t)))

(use-package ws-butler
  :ensure t)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (setq key-chord-two-keys-delay 0.05)
;;   (key-chord-define-global "x1" 'delete-other-windows)
;;   (key-chord-define-global "xk" 'ace-window)
;;   (key-chord-define-global "0o" 'delete-window)
;;   (key-chord-define-global "xn" 'helm-mini)
;;   (key-chord-define-global "xb" 'projectile-switch-to-buffer)
;;   (key-chord-define-global "jk" 'magit-status)
;;   (key-chord-define-global "xm" 'helm-M-x)
;;   (key-chord-mode +1))

;; 0xax.blogspot.com/2014/11/emacs-mu4e-offlineimap-multiply-accounts.html
;; vxlabs.com/2014/06/06/configuring-emacs-mu4e-with-nullmailer-offlineimap-and-multiple-identities/
;; www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html

;; interesting settings \ config at https://github.com/mardukbp/dotfiles/blob/master/emacs.d/mb-mu4e.el
(use-package mu4e
  :ensure mu4e-maildirs-extension
  :config (progn
            (use-package smtpmail)
            (use-package starttls)
            (use-package org-mu4e)
            (use-package mu4e-contrib)
            ;;(req-package w3m-load)
            ;;(req-package mime-w3m)
            ;;(req-package smtpmail-async)
            ;; Default Maildir location and account
            ;; Preconfigure variables to be reassigned on Maildir switch
            ;;(use-package mu4e
            ;;  :ensure t
            ;;  :config (progn


            (setq
             ;; General

             ;; program to get mail, invoked with 'U' in main view
             mu4e-get-mail-command "offlineimap"
             mu4e-update-interval 600
             ;;mu4e-headers-auto-update t
             ;; suppress constant update messages from offlineimap and mu index
             mu4e-hide-index-messages t
             mu4e-index-update-error-continue t
             mu4e-index-update-error-warning t

             ;; don't keep message buffers around
             message-kill-buffer-on-exit t

             ;; use 'fancy' non-ascii characters in various places in mu4e
             mu4e-use-fancy-chars t

             ;; save attachments (this can also be a function)
             mu4e-attachment-dir "~/Downloads/attachments"

             ;; don't save message to Sent Mail, Gmail/IMAP handles this
             ;; refer to documentation on mu4e-sent-messages-behaviour for iCloud
             mu4e-sent-messages-behavior 'sent

             ;; smtp
             message-send-mail-function 'smtpmail-send-it
             ;;message-send-mail-function 'async-smtpmail-send-it
             ;;smtpmail-stream-type 'starttls

             ;; insert signature
             mu4-compose-signature-auto-include t

             ;; keybindins / shortcuts for frequently used maildirs:
             ;; access them with 'j' ('jump'); i.e. switch to Gmail/INBOX using 'jg'
             mu4e-maildir-shortcuts
             '(("/Gmail/INBOX"        .    ?g)
               ("/Gmail/Sent"         .    ?f)
               ("/Gmail/Drafts"       .    ?d)
               ;;      ("/Gmail/Spam"         .    ?f)
               ;;      ("/Gmail/Trash"        .    ?a)

               ("/nlisaGmail/INBOX"    .    ?l)
               ("/nlisaGmail/Sent"     .    ?k)
               ("/nlisaGmail/Drafts"   .    ?j)
               ;;      ("/nlisaGmail/Spam"     .    ?j)
               ;;      ("/nlisaGmail/Trash"    .    ?t)

               ("/iCloud/INBOX"       .    ?i)
               ("/iCloud/Sent"        .    ?u)
               ("/iCloud/Drafts"      .    ?y)
               ;;      ("/iCloud/Spam"        .    ?u)
               ;;      ("/iCloud/Trash"       .    ?y)
               )
             )

            ;; Additional customization

            ;; customise the reply-quote-string
            ;; M-x find-function RET message-citation-line-format for docs
            (setq message-citation-line-format "%N @ %d-%m-%Y %H:M %Z:\n")
            (setq message-citation-line-function 'message-insert-formatted-citation-line)

            ;; show full addresses in view messages (instead of just names), group related and ignore duplicates
            (setq mu4e-view-show-addresses t)
            (setq mu4e-headers-include-related t)
            (setq mu4e-headers-skip-duplicates t)

            (setq mu4e-msg2pdf "/usr/bin/msg2pdf")
            ;; mu4e-action-view-in-browser is built into mu4e
            ;; by adding it to these lists of custom actions
            ;; it can be invoked by first pressing a, then selecting
            ;;(add-to-list 'mu4e-headers-actions
            ;;      '("in browser" . mu4e-action-view-in-browser) t)
            (add-to-list 'mu4e-view-actions
                         '("in browser" . mu4e-action-view-in-browser) t)
            ;;(add-to-list 'mu4e-view-actions
            ;;    '("as pdf" . mu4e-action-view-as-pdf) t)

            ;; format date-time stamp in the header list
            ;;(setq mu4e-headers-date-format "%d-%m-%Y %H:%M")
            (setq mu4e-headers-date-format "%d-%m-%Y")
            ;; the headers to show in the headers list -- a pair of a field
            ;; and its width, with 'nil' meaning 'unlimited'
            (setq mu4e-headers-fields
                  '( (:human-date    .   25)
                    ;; (:flags   .    6)
                     ;;(:group\list    .    25)
                     (:from    .   22)
                     (:subject .   nil)))

            ;; attempt to show images when viewing messages
            (setq mu4e-view-show-images t)
            (when (fboundp 'imagemagick-register-types)
              (imagemagick-register-types))
            (setq mu4e-headers-leave-behavior 'apply)

            ;;(setq mu4e-image-max-width 800)

            ;; external command for html->text conversion
            (setq mu4e-view-prefer-html t)
            ;;(require 'mu4e-contrib) ;; already required above
            ;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
            (setq mu4e-html2text-command 'mu4e-shr2text)
            ;;(setq mu4e-html2text-command "w3m -T text/html")

            ;;(setq mu4e-split-view 'vertical)
            (setq mail-user-agent 'mu4e-user-agent)

            ;; Show smileys
            (add-hook 'mu4e-view-mode-hook 'smiley-buffer)

            ;; Try to auto-complete Addresses
            (setq mu4e-compose-complete-addresses t)

            ;; Description of all accounts, which must be identical to corresponding directory name under ~/Maildir

            ;; Function for handling account switching for mail composition
            ;; my-mu4e-set-account will be called every time you edit a message. When composing a new message will be asked to choose account to send from (TAB completion works)
            ;; Replying, forwarding, editing an existing draft, account will be chosen automatically based on the first componenet of the maildir (i.e. directory under ~/Maildir)

            (defun my-mu4e-set-account ()
              "Set the account for composing a message."
              (let* ((account
                      (if mu4e-compose-parent-message
                          (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                            (string-match "/\\(.*?\\)/" maildir)
                            (match-string 1 maildir))
                        (completing-read (format "Compose with account: (%s) "
                                                 (mapconcat #'(lambda (var) (car var))
                                                            my-mu4e-account-alist "/"))
                                         (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                         nil t nil nil (caar my-mu4e-account-alist))))
                     (account-vars (cdr (assoc account my-mu4e-account-alist))))
                (if account-vars
                    (mapc #'(lambda (var)
                              (set (car var) (cadr var)))
                          account-vars)
                  (error "No email account found"))))

            (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

            ;; Preserve email account of a draft when starting another draft with another account
            (defun cpb-make-buffer-local ()
              (make-local-variable 'mu4e-compose-signature)
              (make-local-variable 'user-mail-address)
              (flyspell-mode)
              )
            ;;
            (add-hook 'mu4e-compose-mode-hook 'cpb-make-buffer-local)

            ;;(provide 'init-mu4e)
            ))

;; (use-package gnus-dired
;;   ;; make the `gnus-dired-mail-buffers' function also work on message-mode derived modes, such as mu4e-compose
;;   :config (progn
;;             (defun gnus-dired-mail-buffers ()
;;               "Return a list of active message buffers."
;;               (let (buffers)
;;                 (save-current-buffer
;;                   (dolist (buffer (buffer-list t))
;;                     (set-buffer buffer)
;;                     (when (and (derived-mode-p 'message-mode)
;;                                (null message-sent-message-via))
;;                       (push (buffer-name buffer) buffers))))
;;                 (nreverse buffers)))))

;;   (setq gnus-dired-mail-mode 'mu4e-user-agent)
;;   (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; comments.gmane.org/gmane.emacs.gnus.general/84366
;; ~/.emacs.d/.gnus.d/ contains Mail and News folders
;; as well as the .newsrc, .newsrc.eld and .gnus.registry.eieio files
(setq gnus-home-directory    "~/.emacs.d/gnus.d/"
      gnus-init-file         "~/.emacs.d/gnus.d/gnus.el"
      message-directory      (expand-file-name "Mail/" gnus-home-directory)
      nnfolder-directory     (expand-file-name "archive/" message-directory))
(add-hook 'gnus-group-mode-hook (lambda() (hl-line-mode t)))

;; (auto) joining channels
;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be a bit different, which would screw up autoconnect
;; IRC-server with appropriate wildcards and list of channels for that server
;; Need to populate with other project and hobbies

;; emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
;; https://github.com/bbatsov/emacs-dev-kit/blob/master/erc-config.el
;; keramida.wordpress.com/2013/03/14/automatically-joining-password-enabled-channels-in-erc/

(use-package erc
  :ensure erc-image
  :config (progn
            (require 'erc-track)
            (require 'erc-ring)
            (require 'erc-fill)
            ;;(require 'erc-netsplits)
            (require 'erc-log)
            (require 'erc-notify)
            (require 'erc-spelling)
            (require 'erc-autoaway)
            (require 'erc-match)
            (erc-autojoin-mode t)
            ;;(use-package erc
            ;;  :ensure t
            (setq erc-auto-set-away t)
            (setq erc-autoaway-idle-seconds 600)

            (setq erc-autojoin-channels-alist
                  '((".*\\.freenode.net" "#emacs" "#archlinux" "#linux"))) ;;  "#kali-linux" "#conkeror" "#bash" "#debian" "##physics" "#metasploit" "##hamradio" "#regex" "#latex" "#raspberrypi" "#arduino" "#python" "#c++" "#perl" "#zsh" "#mediawiki" "#erc" "#archlinux-newbie" "#git" "#kali-linux" "#openelec" "#xbmc" "#c" "#kernel" "#lisp" "#gnu" "#gcc" "#gnu" "#gcc" "#R-finance" "#econometrics" "#economics"
            ;;      (".*\\.gimp.org" "#unix" "#gtk+" )
            ;;      (".*\\.netsplit.de" "#dropbox")
            ;;      (".*\\.spotchat.org" "#linuxmint-help")))

            ;; channel tracking and notification
            (erc-track-mode t)
            (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                            "324" "329" "332" "333" "353" "477"))
            ;; for above IRC Codes, see http://www.irchelp.org/irchelp/rfc/chapter6.html#c6_2

            ;; don't track any of the following
            (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

            ;; enable input history
            (erc-ring-mode t)

            ;; wrap long lines
            (erc-fill-mode t)

            ;; detect netsplits
            ;;(erc-netsplit-mode t)

            ;; spellcheck - requires local aspell
            (erc-spelling-mode t)

            ;; interpret mIRC-style color commands in IRC chats
            (setq erc-interpret-mirc-color t)

            ;; logging
            (setq erc-log-channels-directory "~/.emacs.d/erc/logs")
            (setq erc-save-buffer-on-part t)
            (setq erc-hide-timestamps nil)

            ;; timestamps
            (erc-timestamp-mode t)
            (setq erc-timestamp-format "[%R-%m/%d]")

            ;; truncate buffers so that they don't hog the core
            (setq erc-max-buffer-size 40000) ;; chars to keep in buffer
            (defvar erc-insert-post-hook)
            (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
            (setq erc-truncate-buffer-on-save t)

            ;; kill buffers when leaving
            (setq erc-kill-buffer-on-part t)

            ;; keep input at the bottom
            (erc-scrolltobottom-mode t)

            ;; ERC Auto Query
            ;; Have query buffers open automatically when someone send a private message
            ;; 'buffer - pops up in place of the current buffer
            ;; 'window - pops up in another window, the new window selected
            ;; 'window-nonselect - pops up in another window, the current buffer remains selected
            ;; 'bury - does not pop up, the current buffer remains visible
            ;; 'frame - opens in a new frame
            (setq erc-auto-query 'buffer)

            ;; Force .authinfo.gpg authentication
            (setq erc-promtp-for-nickserve-password nil)

            ;; Start ERC (or switch to most recently active buffer of already running instance)
            (defun djcb-erc-start-or-switch ()
              "Connect to ERC, or switch to last active buffer"
              (interactive)
              (if (get-buffer "irc.freenode.net:6667") ;; ERC already acive?
                  (erc-track-switch-buffer 1) ;; yes: switch to last active
                (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC - trying erc-ssl...? instead of erc   ;;
                  (select-frame (make-frame));; open in new frame
                  (erc :server "irc.freenode.net" :port 6667 :nick "nlisa" :full-name "nlisa dot nlisa"))
                ;;      (erc :server "irc.gimp.org" :port 6667 :nick "nlisa" :full-name "nlisa.nlisa")
                ;;      (erc :server "irc.spotchat.org" :port 6667 :nick "nlisa" :full-name "nlisa.nlisa"))
                )
              )

            ;; bind C-c e to function to switch to ERC
            ;;(global-set-key (kbd "C-c e") 'djcb-erc-start-or-switch) ;; ERC

            ;;  LocalWords:  nlisa nuk
            )
  :bind ("C-<f3>" . djcb-erc-start-or-switch))

(defun nlisa-mu4e-gnus-erc-start-or-switch ()
  "Connect to mu4e, Gnus and ERC, or switch to most recently active buffer of an already running instance"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ; ERC already active?
      (progn
        (erc-track-switch-buffer 1)     ; yes: switch to last active
        (mu4e))                         ; then immediately open mu4e message
    ;; no: start new ERC server and rerun mu4e and gnus
    (erc :server "irc.freenode.net" :port 6667 :nick "nlisa" :full-name "nlisa dot nlisa")
    (mu4e t) ; start mu4e in the background
    (gnus))) ; start gnus mail server, update news and jump to groups buffer

(defun nlisa-threaded-server-start-or-switch ()
  "Check if the current server is the \"threaded\" server i.e. Mail, News and IRC. Launch mu4e, gnus and erc in threaded server."
  (interactive)
  ;; (setq svr ("Mail_News_IRC"))
  (when (server-running-p "threaded")
    (nlisa-mu4e-gnus-erc-start-or-switch))
  (unless (server-running-p "threaded")
    (server-eval-at "threaded" (nlisa-mu4e-gnus-erc-start-or-switch))))
    ;; (when (y-or-n-p "Start new Mail, News and IRC Server?")
    ;;   (select-frame (make-frame))
    ;;   ;; (set-variable 'server-name "Mail_News_IRC")
    ;;   ;; (server-start)
    ;;   (erc :server "irc.freenode.net" :port 6667 :nick "nlisa" :full-name "nlisa dot nlisa")
    ;;   (mu4e t)
    ;;   (gnus))))

;(setq browse-url-browser-function 'eww-browse-url) ; Use eww as default browser
;;(setq browse-url-generic-program (executable-find "conkeror")
;;      shr-external-browser 'browse-url-generic)
(setq browse-url-browser-function 'browse-url-generic
         browse-url-generic-program "conkeror")

;; Use q to kill the buffer and not to just hide it
(use-package doc-view
  :config (define-key doc-view-mode-map (kbd "q") 'kill-this-buffer))

;; ssh is faster than scp
;; however, some environments only work with scp
;; cannot pipe connections using scp
(use-package tramp
  :defer t
  :config (progn
            (setq tramp-default-method "ssh")
            (setq tramp-persistency-file-name nil)
            (use-package tramp-sh
              :config (progn
                        (add-to-list 'tramp-remote-path "/usr/local/sbin")
))))

(defun dired-linux-xdg-open ()
  "Use the Linux `'xdg-open' command to open a file or URL with the user's preferred application."
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "xdg-open" current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))

(use-package dired
  :config (progn
            (use-package dired-x
              :init (setq-default dired-omit-files-p t))
            (customize-set-variable 'diredp-hide-details-initially-flag nil)

            (use-package dired+)
            (use-package dired-aux
              :init (use-package dired-async))

            ;; Enable use of a in dired buffer, to keep buffer and not open a new one
            (put 'dired-find-alternate-file 'disabled nil)

            ;; always delete and copy recursively
            (setq dired-recursive-copies 'always)
            (setq dired-recursive-deletes 'always)

            ;; show human readable file sizes
            (setq dired-listing-switches "-alh")

            (setq dired-dwim-target t)

            ;; List directories at the top
            (setq ls-lisp-dirs-first t)

            ;; Auto refresh dired
            (setq global-auto-revert-non-file-buffers t)
            (setq wdired-allow-to-change-permissions t)

            ;; Omit files from dired browser with undesired extentions to find
            (setq-default dired-omit-mode t)
            (setq dired-omit-files
                  (concat "^\\.?#"
                          "\\|" "^\\.$"
                          "\\|" "^\\.dvi$"
                          "\\|" "^\\.log$"
                          "\\|" "^\\.gz$"
                          "\\|" "^\\.out$"))
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (add-hook 'dired-mode-hook
                      (lambda ()
                        (define-key dired-mode-map (kbd "^")
                          (lambda () (interactive) (find-alternate-file "..")))))
            (define-key dired-mode-map (kbd "C-<return>") 'dired-linux-xdg-open)
            ;; Change to writable dired mode, i.e.
            ;; Make dired buffer editable - however this is already bound)
            ;;(define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)

  ))

;; Encryption using gpg
;; For a single pinentry popup for multiple accounts in .authinfo.gpg
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(use-package eshell
  :ensure eshell-prompt-extras
  :config (progn
            (use-package em-cmpl)
            (use-package em-prompt)
            (use-package em-term)

            (require 'em-hist)
            (if (boundp 'eshell-save-history-on-exit)
                ;; Automatically and silently save eshell history
                (setq eshell-save-history-on-exit t))

            (setq eshell-cmpl-cycle-completions nil
                  ;; auto truncate after 12k lines
                  eshell-buffer-maximum-lines 12000
                  ;; history size
                  eshell-history-size 350
                  ;; buffer shorthand -> echo foo > #'buffer
                  eshell-buffer-shorthand t
                  ;; treat 'echo' like shell echo
                  )

            (when (not (functionp 'eshell/rgrep))
              (defun eshell/rgrep (&rest args)
                "Use Emacs grep facility instead f calling external grep."
                (eshell-grep "rgrep" args t)))

            (defun eshell/cds ()
              "Change directory to he project's root."
              (eshell/cd (locate-dominating-file default-directory ".git")))

            (defun eshell/ll (&rest args) "Same as `ls -lh'"
                   (apply #'eshell/ls "-lh" args))
            (defun eshell/la (&rest args) "Same as `ls -alh'"
                   (apply #'eshell/ls "-alh" args))

            (defun eshell/clear ()
              "Clear the eshell buffer"
              (interactive)
              (let ((eshell-buffer-maximum-lines 0))
                (eshell-truncate-buffer)))

            (defun eshell/magit ()
              "Function to open magit-status or the current directory."
              (interactive)
              (magit-status default-directory)
              nil)
            ))

(use-package yasnippet
  :ensure t
  :config (progn
            (yas-global-mode t)
            (add-to-list 'yas-snippet-dirs "~/.emacs.d/github/yasnippet-snippets")
            (yas-reload-all)))

(use-package auto-yasnippet
  :ensure t
  :config (progn
            (global-set-key (kbd "H-w") #'aya-create)
            (global-set-key (kbd "H-y") #'aya-expand)
            (global-set-key (kbd "C-o") #'aya-open-line)))

(use-package flycheck
  :ensure t
  :config (progn
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (use-package flycheck-tip
              :ensure t
              ;;:bind (("C-c C-n" . flycheck-tip-cycle)
              ;;       ("C-c C-p" . flycheck-tip-cycle-reverse))
              ;;:config (add-hook 'flycheck-mode-hook  #'my/flycheck-tip-customize)
              :config (progn
                        (define-key flycheck-mode-map (kbd "C-c ! N") 'flycheck-tip-cycle)
                        (define-key flycheck-mode-map (kbd "C-c ! P") 'flycheck-tip-cycle-reverse))
              )
            ;; (use-package flycheck-haskell
            ;;  :config (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
            (use-package helm-flycheck
              :ensure t
              :config (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
            ))

(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'flycheck-error-list-mode 'hl-line-mode)

;; On-the-fly Spell Check - this is also used in the LaTeX settings below
     ;; Must move all the minor mode hooks for flyspell here
     (use-package flyspell
       ;;:defer t
       :diminish ""
       :config (progn
                 (setq flyspell-issue-message-flag nil)
                 (setq ispell-program-name "aspell") ; Or ispell
                 (setq ispell-dictionary "english")
                 ;; (setq ispell-personal-dictionary (expand-file-name "flyspellpersonal" --history-directory))

                  (setq ispell-extra-args
                        (list "--sug-mode=normal" ;; ultra|fast|normal|bad-spellers
                              "--lang=en_GB"
                        ))    ;; Want to debug spelling, run-together checks for camelcase instances in code, but this is too noisy, and often produces far too many candidates
                              ;; "--ignore=3"
                              ;; "--run-together"
                              ;; "--run-together-limit=5"
                              ;; "--run-together-min=2"))
                 ;; Rebind spell-check keybinding
                 ;; stackoverflow.com/questions/16084022/emacs-flyspell-deactivate-c-key-binding
                 ;; https://github.com.bixuanzju/emacs.d/blob/master/emacs-init.org
                 ;; Maybe move this to section under elisp, then un/bind as appropriate
                 (eval-after-load "flyspell"
                   '(define-key flyspell-mode-map (kbd "C-M-i") nil))
                 (eval-after-load "flyspell"
                   '(define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-word-before-point))

                 (use-package abbrev
                   :diminish ""
                   :config
                   (progn
                     (defun my/ispell-word-then-abbrev (p)
                       "Call `ispell-word'. THen create an abbrev for the correction made.

With prefix P, the abbrev will be local. Otherwise it will be global."
                       (interactive "P")
                       (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
                         (call-interactively 'ispell-word)
                         (setq aft (downcase (or (thing-at-point 'word) "")))
                         (unless (string= aft bef)
                           (message "\"%s\" now expands to \"%s\" %sally"
                                    bef aft (if p "loc" "glob"))
                           (define-abbrev
                             (if p local-abbrev-table global-abbrev-table)
                             bef aft))))

                     (setq save-abbrevs t)
                     (setq-default abbrev-mode t)

                     (defun my/enable-abbrev-mode ()
                       (interactive)
                       (abbrev-mode t))

                     (add-hook 'prog-mode-hook #'my/enable-abbrev-mode)
                     (add-hook 'org-mode-hook #'my/enable-abbrev-mode)

                     (eval-after-load "flyspell"
                       '(define-key flyspell-mode-map (kbd "C-c $") 'my/ispell-word-then-abbrev))))

                 ;; C-; is bound to correct previous word

                 ;; Enable Flyspell program mode for emacs lisp mode, which highlights all mispelled words in comments and strings
                 ;;(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; part of prog-mode
                 (add-hook 'prog-mode-hook 'flyspell-prog-mode)
                 ;; Enable spell-check in regular plain text
                 (add-hook 'text-mode-hook 'flyspell-mode)
                 (add-hook 'org-mode-hook 'flyspell-mode)

                 ;; (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
                 ;; (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "\\+END_SRC"))
                 ;; (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "\\+END_EXAMPLE"))

                 ;; (use-package helm-flyspell
                 ;;   :init
                 ;;   (define-key flyspell-mode-map (kbd "C-M-;") 'helm-flyspell-correct))
                 ))

(use-package company
  :diminish ""
  :ensure t
  :config (progn
            ;; (setq company-auto-complete t)
            ;; (setq company-global-modes t)
            (setq company-idle-delay 0.2) ; default is .0.5
            (setq company-minimum-prefix-length 2) ; default is 4
            (setq company-show-numbers t) ; complete with M-<digit>
            ;; (setq company-tooltip-limit 30)
            (setq company-selection-wrap-around t)

            (add-hook 'after-init-hook 'global-company-mode)))

(eval-after-load 'company
               '(add-to-list 'company-backends 'company-capf))
(add-hook 'shell-mode-hook #'company-mode)
(add-hook 'eshell-mode-hook #'company-mode)

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

;; To make sure that this is called instead of company-complete-common, use

(add-hook 'company-mode-hook
          (lambda () (substitute-key-definition
                 'company-complete-common
                 'company-yasnippet-or-completion  company-active-map)))

(use-package company-quickhelp
  :ensure t
  :init (company-quickhelp-mode t))

;; Jump back to the previous position in buffer
(global-set-key (kbd "C-S-r") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(use-package helm
  :ensure t
  :diminish ""
  ;; :init (progn
  ;;         (global-unset-key (kbd "C-x c")))
  :bind (("C-x C-b" . helm-mini)
         ("C-c h k" . helm-descbinds)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c h x" . helm-register))
  :config
  (progn
    (use-package helm-swoop
      :ensure t
      :bind (("C-c h C-s" . helm-swoop)
             ("C-c h C-r" . helm-swoop-back-to-last-point)
             ("C-c h M-s s" . helm-multi-swoop)
             ("C-c h M-s a" . helm-multi-swoop-all))
      :config (progn
                (define-key isearch-mode-map (kbd "M-s s") 'helm-swoop-from-isearch)
                (define-key helm-swoop-map (kbd "M-s s") 'helm-multi-swoop-all-from-helm-swoop)))
    ;; Default helm prefix is "C-x c" is quite close to "C-x C-c" which quits Emacs
    ;; Will be using C-c h prefix instead
    ;; This s done globally as we cannot change `helm-command-prefix' once `helm-config' is loaded
    ;; Moved to the end to ensure it takes effect
    ;;  (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (use-package helm-config)
    (use-package helm-files
      :bind (("C-x C-f" . helm-find-files))
      :config (setq helm-ff-file-compressed-list '("gz" "bz2" "zip" "7z" "tgz")))
    (use-package helm-grep)
    (use-package helm-man)
    (use-package helm-misc)
    (use-package helm-aliases)
    (use-package helm-elisp)
    (use-package helm-imenu)
    (use-package helm-semantic)
    (use-package helm-ring)
    (use-package helm-bookmark)
    (use-package helm-eshell)
    (use-package helm-describe-modes
      :ensure t)

    (use-package helm-projectile)
    (use-package helm-descbinds
      ;;:bind ("C-c h k" . helm-descbinds)
      :config (helm-descbinds-mode t))
    (use-package helm-ag)

    ;; Some usefully keybindings
    ;; Rebind TAB to run persistent action and work in terminal
    ;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;;will leave tab as complete documentation
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; Helm as intended requires use of C-j

    ;; List actions using C-z
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-echo-input-in-header-line t)

    (setq helm-idle-delay 0.01
          helm-exit-idle-delay 0.1
          helm-input-idle-delay 0.01
          helm-truncate-line t

          ;; do not display invisible candidates
          ;; helm-quick-update t
          ;; open helm in another window and don't occupy whole other window
          helm-split-window-default-side 'other
          helm-split-window-in-side-p t
          helm-candidate-number-limit 200

          ;; wrap around beginning and end of source list
          helm-move-to-line-cycle-in-source t

          ;; Enable fuzzy matching
          helm-buffers-fuzzy-matching t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-completion-in-region-fuzzy-match t

          ;; Search for library in `require' and `declare-function' sexp.
          ;; Use recentf for file history candidates
          helm-ff-search-library-in-sexp t
          helm-ff-file-name-history-use-recentf t

          helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-buffer-not-found))
    ;; Enable man page at point
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    ;; Activate helm
    (helm-autoresize-mode t)
    (helm-mode t)
    (global-unset-key (kbd "C-x c"))
    ))

(use-package helm-company
  :ensure t
  :config (progn
            (define-key company-mode-map (kbd "C-:") 'helm-company)
            (define-key company-active-map (kbd "C-:") 'helm-company)
            ))

(use-package projectile
  :ensure t
  :config (progn
            (eval-after-load 'helm
              '(use-package helm-projectile
                 :ensure t))
;;            (add-hook 'python-mode-hook 'projectile-mode)
;;            (add-hook 'c-mode-hook 'projectile-mode)
            ;;            (add-hook 'c++-mode-hook 'projectile-mode)
            (projectile-global-mode)
            (setq projectile-completion-system 'helm)
            (helm-projectile-on)
            (setq projectile-switch-project-action 'helm-projectile)
            (setq projectile-enable-caching t)
            ))

(use-package magit
  :ensure t
  :bind (("C-x g g" . magit-status)
         ("C-x g b" . helm-browse-project))
  :init (progn
          (use-package helm-ls-git
            :ensure t)))

(use-package git-gutter
  :ensure t
  :diminish ""
  :bind (("C-x g h" . git-gutter:popup-hunk)
         ("C-x g n" . git-gutter:next-hunk)
         ("C-x g p" . git-gutter:previous-hunk)
         ("C-x g r" . git-gutter:revert-hunk)
         ("C-x g s" . git-gutter:stage-hunk)
         ("C-x g SPC" . git-gutter:mark-hunk)
         ("C-x g G" . git-gutter-mode))
  :config (progn
            ;; Use git-gutter.el and linum-mode
            ;;(git-gutter:linum-setup)
            ;; Activate it a limited number of modes
            (add-hook 'java-mode-hook 'git-gutter-mode)
            (add-hook 'clojure-mode-hook 'git-gutter-mode)
            (add-hook 'c-mode-hook 'git-gutter-mode)
            (add-hook 'python-mode-hook 'git-gutter-mode)
            (add-hook 'c++-mode-hook 'git-gutter-mode)
            ))

(use-package gist
  :ensure t)

(use-package json
  :ensure t)
(use-package mm-url)
(use-package svg
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/github/imgur")
(require 'imgur)
(add-to-list 'load-path "~/.emacs.d/github/nlisafork/meme")
(require 'meme)
(autoload 'meme "meme.el" "Create a meme from a collection" t)
(autoload 'meme-file "meme.el" "Create a meme from a file" t)

(use-package vkill
  :ensure t
  :commands vkill
  :bind ("C-c h L" . vkill-and-helm-occur)
  :init
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (hl-line-mode t)
    (call-interactivel #'helm-occur)))

(use-package multiple-cursors
  :ensure mc-extras
  :bind (("C-c m t" . mc/mark-all-like-this)
         ("C-c m m" . mc/mark-all-like-this-dwim)
         ("C-c m d" . mc/mark-all-like-this-in-defun)
         ("C-c m l" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)
         ("C-c m s" . mc/mark-sgml-tag-pair)
         ("C-c m n" . mc/mark-next-line-like-this)
         ("C-c m p" . mc/mark-previous-line-like-this)
         ("C-c m c" . mc/compare-chars)))

(use-package phi-search
  :ensure t)

(use-package phi-search-mc
  :ensure t
  :config (phi-search-mc/setup-keys))

(use-package color-identifiers-mode
  :ensure t)

(use-package iedit
  :ensure t
  :bind ("C-c i" . iedit-mode))

(use-package hideshow
  :bind (("C-c s TAB" . hs-toggle-hiding)
         ("C-c s w" . hs-hide-all)
         ("C-c s q" . hs-show-all))
  :config (progn
            (defvar hs-special-modes-alist
              (mapcar 'purecopy
                      '((c-mode "{" "}" "/[*/]" nil nil)
                        (c++-mode "{" "}" "/[*/]" nil nil)
                        (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
                        (java-mode "{" "}" "/[*/]" nil nil)
                        (js-mode "{" "}" "/[*/]" nil)
                        (javascript-mode  "{" "}" "/[*/]" nil))))))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Restrict Semantic from org-mode and org-agenda-mode buffers
(add-to-list 'semantic-inhibit-functions
             (lambda () (member major-mode '(org-mode org-agenda-mode))))

(add-hook 'org-mode-hook '(lambda() (set
                                (make-local-variable 'semantic-mode) nil)))

(add-hook 'org-agenda-mode-hook '(lambda() (set
                                (make-local-variable 'semantic-mode) nil)))

(use-package org
  :ensure writegood-mode
  :config (progn
            ;; Preserves source specific syntax highlighting and indentation for code blocks
            (setq org-src-fontify-natively t)
            (setq org-src-tab-acts-natively t)
            (setq org-src-preserve-indentation t)
            (setq org-pretty-entities t)

            ;; Run C-c C-x C-v or M-x org-display-inline-images
            (setq org-startup-with-inline-images t)

            ;; Encoding System
            (prefer-coding-system 'utf-8)
            ;; (set-default-coding-systems 'UTF-8)
            ;;(set-default-coding-systems 'utf-8)
            (set-keyboard-coding-system 'utf-8)
            (set-selection-coding-system 'utf-8)
            (set-terminal-coding-system 'utf-8)
            (set-buffer-file-coding-system 'utf-8)
            (setq buffer-file-coding-system 'utf-8)
            (setq locale-coding-system 'utf-8)

            (setenv "LANG" "en_GB.UTF-8")
            (setenv "LC_ALL" "en_GB.UTF-8")

            ;; Associate file-types with the appropriate application in org-mode
            (setq org-file-apps
                  '((auto-mode . emacs)
                    ("\\.x?html?\\'" . "conkeror \"%s\"")
                    ("\\.pdf\\'" . "evince \"%s\"")
                    ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")))

            ;; Block switching parent to DONE if there are TODO children
            (setq org-enforce-todo-dependencies t)
            (use-package htmlize
              :ensure t)

            (setq org-use-sub-superscripts '{})

            ;; Setup which languages allow eval
            (use-package ob)
            (use-package ob-C)
            (use-package ob-browser)
            (use-package ob-ipython)
            (use-package ob-php)
            (use-package ob-ditaa)

            ;;(setq org-ditaa-jar-path "/usr/bin/ditaa")
            ;; execute external programs.
            (org-babel-do-load-languages
             'org-babel-load-languages
             '(
               (python . t)
               (sh . t)
               (latex . t)
               (emacs-lisp . t)
               (org . t)
               (C . t)
               (ditaa . t)
               (browser . t)
               (php . t)
               (ipython . t)
               ))

            (setq org-log-done t
                  org-todo-keywords '((sequence "TODO" "BUSY" "DONE"))
                  org-todo-keyword-faces '(("BUSY" . (:foreground "Red" :weight bold))))
            (add-hook 'org-mode-hook
                      (lambda ()
                        (writegood-mode)
                        (hl-line-mode t)))
            ;; (add-hook 'org-mode-hook
            ;;       (lambda ()
            ;;  (auto-complete-mode)))
            (add-hook 'prog-mode-hook
                      (lambda() (hl-line-mode t)))))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (interactive)
;;      (hl-line-mode t)))))

(defvar --org-directory (concat user-emacs-directory (file-name-as-directory "org"))
  "--org-directory is user defined location inside the user-emacs-directory, where all org-mode related files and folders are stored.

These include, but are not limited to GTD todo org files, capture, refile, archive and agenda files and folders.")
(if (not (file-exists-p --org-directory))
    (make-directory --org-directory t))

;; Setup capture file directory
(setq org-default-notes-file --org-directory)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)

;; Setup capture templates
;; %a can be used to link to location from where capture command was called.
(use-package org-protocol
  :init (progn
          (setq org-protocol-default-template-key "l")
          (setq org-capture-templates
                '(("w" "Default template" entry (file+headline (concat --org-directory "journal.org")"Notes")
                   "* TODO [#C] %^{Title} %^G\n\n  Source: %U, %c\n\n %? %i"
                   :prepend)
                  ("j" "Journal" entry (file+datetree (concat --org-directory "journal.org"))
                   "* %^{Journal Entry Log}  %^G\n\nEntered on %U\n\n %?")
                  ;;("t" "General Uncategorised Todo Notes / Tasks." entry (file+headline (concat --org-directory "kaizen.org") "General Tasks")
                  ;; "* TODO %? %^g\n")
                  ("k" "Kaizen - Self Enlightenment." entry (file (concat --org-directory "kaizen.org"))
                   "* TODO [#C] %^{Title} %^G\n\n Entered on: %U\n\n %?")
                  ("e" "Emacs." entry (file+headline (concat --org-directory "code.org")"Emacs")
                   "* TODO [#C] %^{Title} %^g\n\n Entered on: %U\n\n %?")
                  ("d" "Development Environment." entry (file+headline (concat --org-directory "code.org")"Development Environment")
                   "* TODO [#C] %^{Title} %^G\n\n Entered on: %U\n\n %?")
                  ("p" "Programming and Code - I am 8-Bit." entry (file (concat --org-directory "code.org"))
                   "* TODO [#C] %^{Title} %^g\n\n Entered on: %U\n\n %?")
                  ("c" "Chow Course." entry (file (concat --org-directory "course.org"))
                   "* TODO [#C] %^{Title} %^g\n\n Entered on: %U\n\n %?")
                  ("m" "Monopolize the 1%" entry (file (concat --org-directory "monopoly.org.gpg"))
                   "* TODO [#C] %^{Title} %^g\n\n Entered on: %U\n\n %?")
                  ))))

;; Setup agenda files directory
(setq org-agenda-directory --org-directory)
(setq org-agenda-files (directory-files (expand-file-name org-agenda-directory) t "^[^\.][^#][[:alnum:]]+\.org$"))
(add-to-list 'org-agenda-files "/home/nlisa/course/phd/bibliography/notes.org")
;;(setq org-agenda-files (append '("~/.conkerorrc" "~/.emacs.d" (directory-files (expand-file-name org-agenda-directory) t "^[^\.][^#][[:alnum:]]+\.org$"))))
;; TODO Add emacs-init.org

(defadvice org-archive-subtree
  (before add-inherited-tags-before-org-archive-subtree activate)
  "add inherited tags before org-archive-subtree"
  (org-set-tags-to (org-get-tags-at)))

(use-package toc-org
  :ensure t)
(defun *-org-insert-toc ()
  "Create table of contents (TOC) if current buffer is in
`org-mode'."
  (when (= major-mode 'org-mode)
    toc-org-insert-toc))

(use-package org-ref
  :ensure t)
(setq reftex-default-bibliography '("/home/nlisa/course/phd/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "/home/nlisa/course/phd/bibliography/notes.org"
      org-ref-default-bibliography '("/home/nlisa/course/phd/bibliography/references.bib")
      org-ref-pdf-directory "/home/nlisa/course/phd/bibliography/bibtex-pdfs/")

(use-package helm-bibtex
  :ensure helm)

(setq bibtex-completion-bibliography "/home/nlisa/course/phd/bibliography/references.bib"
      bibtex-completion-library-path "/home/nlisa/course/phd/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "/home/nlisa/course/phd/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
;;(setq bibtex-completion-pdf-open-function
;;      (lambda (fpath)
;;        (start-process "open" "*open*" "open" fpath)))

;; alternative
(setq bibtex-completion-pdf-open-function 'org-open-file)

(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Remove backends from VC mode
(setq vc-handled-backends '(SVN GIT))

(defun my/add-watch-words-in-code ()
  "Highlight WARNING, FIXME, TODO, and NOCOMMIT in code."
  (font-lock-add-keywords
   nil '(("\\<\\(WARNING\\|FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "green") (:weight bold)) t))))

(add-hook 'prog-mode-hook #'my/add-watch-words-in-code)
;;(add-hook 'prog-mode-hook
;;          (lambda () hl-line-mode t))

;; Scroll Compilation Window up until first error
;;(setq compilation-scroll-output 'first-error)
(setq compilation-scroll-output t)

;; Make scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook
    (lambda() (linum-mode t)))
(add-hook 'prog-mode-hook
          (lambda() (column-number-mode t)))
(add-hook 'prog-mode-hook
    (lambda() (hl-line-mode t)))

;; tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919
;;(require 'tex)
(use-package tex-site
  :ensure auctex
  :config

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  ;;(require 'tex)
  ;;(TeX-global-PDF-mode t)
  (setq TeX-PDF-mode t)
  (setq-default TeX-master nil)

  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

  (setq TeX-source-correlate-mode t)

  ;;(add-hook 'Latex-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  ;; On-the-fly TeX syntax checking, replaced flymake with flycheck
  ;;(require 'flymake)
  ;;(defun flymake-get-tex-args (filename)
  ;;  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
  ;;(add-hook 'LaTeX-mode-hook 'flymake-mode)
                                        ;(add-hook 'after-init-hook #'global-flycheck-mode)

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)

  ;; Auomatically activate TeX-fold-mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (TeX-fold-mode t)))

  ;; Outline Mode
  ;;   -- Hide parts of
  ;;      tex file
  (defun turn-on-outline-minor-mode ()
    (outline-minor-mode 1))

  (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
  (setq ouline-minor-mode-prefix "C-c C-o")
  ;; Can be other unused alternative for prefix
  ;; C-c C-o C-l : Hide contents of current chapter, section, subsection, etc...
  ;; C-c C-o C-n : Navigate to next "unit" of document, C-p for previous
  ;; C-c C-o C-a : View whole document when lost

  ;; Expand into csquotes macros (for this to work babel must be loaded after csquotes).
  (setq LaTeX-csquotes-close "}"
        LaTeX-csquotes-open-quote "\\enquote{")

  ;; RefTeX Configuration
  ;;(require 'tex-site)
  ;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs LaTeX mode

  ;;(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
  ;;(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  ;;(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  ;;(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)

  (eval-after-load 'reftex-vars
    '(progn
       ;; Prompt for empty optional arguments in cite macros.
       (setq reftex-cite-prompt-optional-args t)
       ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
       (setq reftex-plug-into-AUCTeX t)
       ;; Ensure RefTeX also recognises \addbibresource.
       (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
       ;; Enable RefTeX in Org-mode to acknowledge bibliography
       ;;(setq reftex-default-bibliography '(UNCOMMENT LINE AND INSERT PATH TO BIBLIOGRAPHY)
       ;; Recognize \subcaptions, e.g. reftex-citation
       (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]")
       ;; Get RefTeX with biblatex
       ;; http://tex.sackexchange.com/questions/31966/settings-up-reftex-with-biblatex-citation-commands/
       ;; The first element in each pair is the keyboard character following C-c [ to select citation format.
       ;; Empty square brackets denote optional arguments, for which RefTex has optional arguments for prompting.
       ;; %1 is where the cite key goes.
       (setq reftex-cite-format
             '((?t . "\\textcite[]{%1}")
               (?a . "\\autocite[]{%1}")
               (?c . "\\cite[]{%1}")
               (?s . "\\smartcite[]{%1}")
               (?f . "\\footcite[]{%1}")
               (?n . "\\nocite{%1}")
               (?b . "\\blockquote[]{%1}")))))

  ;; Fontifcation (remove unnecessary entries as not needed)
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html
  ;; http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
  (setq font-latex-match-reference-keywords
        '(
          ;; biblatex
          ("printbibliography" "[{")
          ("addbibresource" "[{")
          ;; Standard commands
          ;; ("cite" "[{")
          ("Cite" "[{")
          ("parencite" "[{")
          ("Parencite" "[{")
          ("footcite" "[{")
          ("footcitetext" "[{")
          ;; Style-specific commands
          ("textcite" "[{")
          ("Textcite" "[{")
          ("smartcite" "[{")
          ("Smartcite" "[{")
          ("cite*" "[{")
          ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists
          ("cites" "[{")
          ("Cites" "[{")
          ("parencites" "[{")
          ("Parencites" "[{")
          ("footcites" "[{")
          ("footcitetexts" "[{")
          ("textcites" "[{")
          ("Textcites" "[{")
          ("smartcites" "[{")
          ("Smartcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands
          ("autocite" "[{")
          ("Autocite" "[{")
          ("autocite*" "[{")
          ("Autocite*" "[{")
          ("autocites" "[{")
          ("Autocites" "[{")
          ;; Text commands
          ("citeauthor" "[{")
          ("Citeauthor" "[{")
          ("citetitle" "[{")
          ("citetitle*" "[{")
          ("citeyear" "[{")
          ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands
          ("fullcite" "[{")))

  (setq font-latex-match-textual-keywords
        '(
          ;; biblatex brackets
          ("parentext" "{")
          ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; auxiliary commands
          ("textelp" "{")
          ("textelp*" "{")
          ("textins" "{")
          ("textins*" "{")
          ;; subcaption
          ("subcaption" "[{")))

  (setq font-latex-match-variable-keywords
        '(
          ;; amsmath
          ("numberwithin" "{")
          ;; enumitem
          ("setlist" "[{")
          ("setlist*" "[{")
          ("newlist" "{")
          ("renewlist" "{")
          ("setlistdepth" "{")
          ("restartlist" "{")))

  ;; Preview-LaTeX
  (load "auctex.el" nil t t)
  ;;(load "preview-latex.el" nil t t)

  ;; General LaTeX settings
  (setq LaTeX-eqnarray-label "eq"
        LaTeX-equation-label "eq"
        LaTeX-figure-label "fig"
        LaTeX-table-label "tab"
        LaTeX-myChapter-label "chap"
        TeX-auto-save t
        TeX-newline-function 'reindent-then-newline-and-indent
        TeX-parse-self t
        TeX-style-path
        '("style/" "auto/"
          "/usr/share/emacs24/site-lisp/auctex/style"
          "/var/lib/auctex/emacs24"
          "/usr/local/share/emacs/site-lisp/auctex/style/")
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
)
(use-package latex-extra
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package ebib)
;;  :ensure helm-bibtex)

;; See emacslife.com/how-to-read-emacs-lisp.html

(defun my/turn-on-paredit-and-eldoc ()
  "Turn on paredit and eldoc mode for elisp mode."
  (interactive)
  (paredit-mode t)
  (eldoc-mode t))

(add-hook 'emacs-lisp-mode-hook #'my/turn-on-paredit-and-eldoc)
(add-hook 'ielm-mode-hook #'my/turn-on-paredit-and-eldoc)

(use-package eldoc
  ;;:diminish 'eldoc-mode ""
  :config
  (progn
    (setq eldoc-idle-delay 0.3)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "chartreuse"
                        :weight 'bold)))

;; Change the faces for elisp regex grouping:

;;(set-face-foreground 'font-lock-regexp-grouping-backslash "deep pink")
;;(set-face-foreground 'font-lock-regexp-grouping-construct "plum")

;; Define some niceties for popping up an ielm buffer
(defun ielm-other-window ()
  "Run ielm on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'ielm-other-window)

;; Turn on elisp-slime-nav if available, can now jump to function definitions using M-.

(use-package elisp-slime-nav
  :ensure slime
  :config (progn
            (slime-setup)
            (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
              (add-hook hook 'turn-on-elisp-slime-nav-mode))))

;; Pretty print results - Borrowed form Steve Purcell's Configuration

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise eval the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; M-x edit-list makes it easier to edit and Emacs Lisp list
(use-package edit-list
  :ensure t)

;; Common lisp primitives
(use-package cl
  :ensure t)

(use-package company-jedi
  :ensure t
  :config (eval-after-load 'company
               '(add-to-list 'company-backends 'company-jedi)))

(use-package python-pylint
  :ensure t
  :bind ("C-c ! L" . python-pylint))

(use-package py-autopep8
  :ensure t
  :config (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package anaconda-mode
  :ensure t
  :config (progn
            (use-package company-anaconda
              :ensure t)
            (eval-after-load 'company
               '(add-to-list 'company-backends 'company-anaconda))
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
            ))

(use-package sphinx-doc
  :ensure t
  :config (add-hook 'python-mode-hook 'sphinx-doc-mode))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i --matplotlib")

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package helm-gtags
  :ensure t
  :config (progn
          (setq helm-gtags-prefix-key "\C-cg")
          (setq helm-gtags-ignore-case t
                helm-gtags-auto-update t
                helm-gtags-use-input-at-cursor t
                helm-gtags-pulse-at-cursor t
                helm-gtags-suggested-key-mapping t)

          ;; Enable helm gtags mode
          (add-hook 'dired-mode-hook 'helm-gtags-mode)
          (add-hook 'eshell-mode-hook 'helm-gtags-mode)
          (add-hook 'c-mode-hook 'helm-gtags-mode)
          (add-hook 'java-hook 'helm-gtags-mode)
          (add-hook 'c++-mode-hook 'helm-gtags-mode)
          (add-hook 'asm-mode-hook 'helm-gtags-mode)

          ;; Keybindings
          (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (unbind-key "M-?" helm-gtags-mode-map)
      (define-key helm-gtags-mode-map (kbd "M-?") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "C-c g SPC") 'helm-gtags-select)
      (unbind-key "M-," helm-gtags-mode-map)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-pop-stack)
          ;;(define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
          ;;(define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
          (define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)
      (unbind-key "M-." helm-gtags-mode-map)
          (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
          (define-key helm-gtags-mode-map (kbd "C-c g d") 'helm-gtags-dwim)
          (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
          (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
          (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
          (define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags)
          (define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-find-files)
          (define-key helm-gtags-mode-map (kbd "C-c g P") 'helm-gtags-parse-file)
          (define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)
          (define-key helm-gtags-mode-map (kbd "C-c g <") 'helm-gtags-previous-history)
          (define-key helm-gtags-mode-map (kbd "C-c g >") 'helm-gtags-next-history)))

(defun my/setup-semantic ()
  "Load and configure semantic `which provides language-aware commands based on source code parsers.'"
  (interactive)
  (use-package semantic
    :config
    (progn
      (use-package cc-mode)
      (use-package semantic/ia)
      (use-package semantic/wisent)
      (use-package stickyfunc-enhance)

      ;; Redundant as semantic already includes =/usr/include= by default
      (semantic-add-system-include "/usr/include/c++")

      ;; Parse visited buffers for semantic content.
      ;; Cache parsing results for future use.
      ;; Periodically check if buffer is out of date and reparse while user is idle.
      (setq semantic-default-submodes
            '(global-semantic-idle-scheduler-mode
              global-semanticdb-minor-mode
              global-semantic-idle-summary-mode
              global-semantic-stickyfunc-mode))
      (setq semanticdb-default-save-directory (concat --history-directory "semanticdb"))

      ;; Additional paths for semantic to parse
      ;; (semantic-add-system-include "/export/home0/lancet_linux")
      ;; (semantic-add-system-include "/usr/local/include")

      ;; View list of include patsh in semantic-dependency-system-include-path
      ;; can add additional paths, eg Boost include paths
      ;;(semantic-add-system-include "/usr/include/boost")

      (add-to-list 'auto-mode-alist '("\\.g$" . c++mode))
      (add-to-list 'auto-mode-alist '("\\.g$" . c-mode)))
    :bind (( [(control return)] . semantic-ia-complete-symbol)
           ("C-c , ." . semantic-ia-fast-jump)
           ;; ( "C-c >" . semantic-complete-analyze-inline)
           ;; ( "C-c ?" . semantic-analyze-proto-impl-toggle)
           )))

;; Explicitly inhibit semantic mode using semantic-inhibit-functions

;; Restrict all non-cc-mode buffers
;; (setq semantic-inhibit-functions
;;       (list (lambda () (not (and (featurep 'cc-defs)
;;                             c-buffer-is-cc-mode)))))

;; Restrict Semantic from org-mode and org-agenda-mode buffers
(add-to-list 'semantic-inhibit-functions
             (lambda () (member major-mode '(org-mode org-agenda-mode))))

(add-hook 'org-mode-hook '(lambda() (set
                                (make-local-variable 'semantic-mode) nil)))

(add-hook 'org-agenda-mode-hook '(lambda() (set
                                (make-local-variable 'semantic-mode) nil)))

;;  (semantic-mode t))
(add-hook 'c++-mode-hook #'my/setup-semantic)
(add-hook 'c-mode-hook #'my/setup-semantic)
(add-hook 'java-mode-hook #'my/setup-semantic)

(use-package function-args
  :ensure t
  :bind ("C-c M-d" . moo-doxygen)
  :config (progn
            (fa-config-default)

            (use-package auto-yasnippet
              :ensure t
              )

            (add-hook 'c-mode-hook 'function-args-mode)
            (add-hook 'c++-mode-hook 'function-args-mode)))
;; (define-key function-args-mode-map (kbd "C-c f o") 'moo-complete)
;; (define-key function-args-mode-map (kbd "C-c f i") 'moo-jump-local)
;; (define-key function-args-mode-map (kbd "C-c f s") 'fa-show)
;; (define-key function-args-mode-map (kbd "C-c f a") 'fa-abort)
;; (define-key function-args-mode-map (kbd "C-c f p") 'fa-idx-cycle-up)
;; (define-key function-args-mode-map (kbd "C-c f n") 'fa-idx-cycle-down)

;; Browse source tree with Speedbar file browsre
(use-package sr-speedbar
  :ensure t
  :bind ("C-c g b" . sr-speedbar-toggle))

;; Use gdb-many-windows by default
;; Display source file containing the main routine at startup
(setq gdb-many-windows t
      gdb-show-main t)
;;

;;(setq company-backend (delete 'company-semantic company-backends))
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

(use-package company-c-headers
  :ensure t
  :config
  (progn
    (eval-after-load 'company
     '(add-to-list 'company-backends 'company-c-headers))
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.1.1/")
    ))

;; (setq company-backend (delete 'company-semantic company-backends))
;; (define-key c-mode-map [(tab)] 'company-complete)
;; (define-key c++-mode-map [(tab)] 'company-complete)

;; (use-package irony
;;   :ensure t
;;   :config (progn
;;             (use-package company-irony
;;               :ensure t
;;               :config (eval-after-load 'company
;;                         '(add-to-list 'company-backends 'company-irony)))

;;             (add-hook 'c++-mode-hook 'irony-mode)
;;             (add-hook 'c-mode-hook 'irony-mode)
;;             (add-hook 'objc-mode-hook 'irony-mode)

;;             ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;             ;; irony-mode's buffers by irony-mode's function
;;             (defun my-irony-mode-hook ()
;;               (define-key irony-mode-map [remap completion-at-point]
;;                 'irony-completion-at-point-async)
;;               (define-key irony-mode-map [remap complete-symbol]
;;                 'irony-completion-at-point-async))
;;             (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;             (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;             (use-package flycheck-irony
;;               :ensure t
;;               :config (eval-after-load 'flycheck
;;                         '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

;;(setq company-backend (delete 'company-c-headers company-backends))
;;(add-to-list 'company-backends 'company-c-headers )

;; (add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(defalias 'perl-mode 'cperl-mode)
;;(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm][xX]?\\|al\\)\\'" . cperl-mode))
;;(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;;(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;;(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Turn on flymake and rebind perldoc instead of info on C-h f
(add-hook 'cperl-mode-hook
          (lambda ()
            (flymake-mode t)
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))

(use-package js2-mode
  :ensure company-tern
  :config (progn
            (eval-after-load 'company
               '(add-to-list 'company-backends 'company-tern))
            (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
            (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
            ;; This may be redundant
            (add-hook 'js2-mode-hook #'my/setup-semantic)
            (add-hook 'js2-mode-hook 'flycheck-mode)))

(use-package markdown-mode
  :ensure t)
