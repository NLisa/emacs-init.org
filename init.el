;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup configuration Resources
;;
;; https://github.com/ivoarch/.dot-org-files/blob/master/emacs.org
;; https://github.com/dakrone/dakrone-dotfiles/blob/master/.emacs.d/settings.org
;; www.aaronbedra.com/emacs.d/
;; https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on debugging during initialization, will be turned off again at the end
;;(setq debug-on-error t)
;;(setq debug-on-quit t)

;;  Keep track of loading times
(defconst emacs-start-time (current-time))

;; ~/.emacs.d folder should be relatively portable, however take note of the additional packages that are distro dependent and may be required to be installed and configured
;; auctex preview-latex tex-site (and all related latex packages) bbdd mu4e (offlineimap and mu) python-mode... amoungst others

;; Need to check and clear all obsolete options, hence need to subsrcibe to newsgroups for all projects...

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(bmkp-last-as-first-bookmark-file "/home/nlisa/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (manoj-dark)))
 '(doc-view-continuous t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-agenda-files
   (quote
    ("/home/nlisa/course/phd/bibliography/notes.org" "/home/nlisa/.emacs.d/org/code.org" "/home/nlisa/.emacs.d/org/course.org" "/home/nlisa/.emacs.d/org/journal.org" "/home/nlisa/.emacs.d/org/kaizen.org" "/home/nlisa/.emacs.d/org/work.org")))
 '(package-selected-packages
   (quote
    (ob-ditaa sphinx-doc auto-yasnippet bookmark+ company-anaconda py-autopep8 org-ref toc-org helm-bibtex markdown-mode ws-butler writegood-mode vkill use-package undo-tree transpose-frame sr-speedbar sml-mode smartparens smart-tab smart-mode-line slime shut-up rainbow-delimiters python-pylint python-pep8 python-mode pymacs phi-search-mc paredit package-build ob-php ob-ipython ob-browser nyan-mode mu4e-maildirs-extension mc-extras magit latex-extra js3-mode js2-mode ipython iedit htmlize helm-swoop helm-projectile helm-ls-git helm-gtags helm-flycheck helm-describe-modes helm-descbinds helm-company guide-key git-rebase-mode git-gutter git-commit-mode gist ggtags fuzzy function-args flycheck-tip expand-region eshell-prompt-extras erc-image edit-list ebib easy-kill company-web company-tern company-statistics company-shell company-quickhelp company-math company-jedi company-flx company-emoji company-cmake company-auctex company-arduino color-identifiers-mode clean-aindent-mode better-defaults bbdb auto-compile alert adaptive-wrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-function-button ((t (:inherit button :foreground "DarkMagenta"))))
 '(apropos-misc-button ((t (:inherit button :foreground "firebrick"))))
 '(apropos-user-option-button ((t (:inherit button :foreground "brown"))))
 '(apropos-variable-button ((t (:inherit button :foreground "DarkRed"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "gainsboro"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "chocolate"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark blue"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "light salmon")))))


;; Temporary bug fix for gnugpg
;;(load-file "~/.emacs.d/elisp/gpgbugfix.el")

;; Proxy related settings
;; (setq url-proxy-services '(;;("no_proxy" . "")
;;                            ("http" . "127.0.0.1:3128")
;;                            ("https" . "127.0.0.1:3128")
;;                           ("ftp" . "127.0.0.1:3128")))

;; Package and Dependency Management
(require 'package)
(package-initialize)

;; Use GNU ELPA, Marmalade and MELPA repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
                         ))
;;(add-to-list 'package-archives
;;  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

(setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Initial package setup
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-verbose t)
(require 'bind-key)
(require 'diminish)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(require 'org)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
		   user-emacs-directory))

;; Message how long did it take to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))


;; Turn off debugging on succesfull load
(setq debug-on-error nil)
(setq debug-on-quit nil)
