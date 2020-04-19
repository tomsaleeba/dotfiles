(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-evil)

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; TODO
;; - auto enable server-start (lazily?)
;; - zt and zb have a few lines of padding (scrolloff)
;; - persist search highlight, https://github.com/juanjux/evil-search-highlight-persist
;; - camel case word navigation
;; - make help open in a new split
;; - add quick buffer list
;; - get projectile prefix group on leader
;; - add leader shortcut for last buffer
;; - figure out visualstar and visual selection weirdness
;; - add some sort of file browser

;; probably lots to learn from https://github.com/cbowdon/Config/blob/master/emacs/init.org

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; Theme
(if
    (member "Hack" (font-family-list))
    (set-face-attribute 'default nil :font "Hack-12")
  (set-face-attribute 'default nil :font "Source Code Pro-12"))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

;; General emacs customisation
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq-default
 indent-tabs-mode nil
 display-line-numbers 'relative
 )

(setq make-backup-files nil
      create-lockfiles nil
      vc-follow-symlinks nil ;; thanks https://stackoverflow.com/a/30900018/1410035
      )

(global-set-key (kbd "C-x .") 'find-init-file)

;; Things useful everywhere
(use-package telephone-line
  :config (telephone-line-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Evil
(setq evil-want-C-u-scroll t)
(use-package evil
  :demand
  :config
  (do-evil-config)
  (install-evil-deps)
  (evil-mode t))

;; Javascript
(use-package vue-mode
  :init
  (add-hook 'mmm-mode-hook
	    (lambda ()
	      (set-face-background 'mmm-default-submode-face nil))))


;; Markdown
(use-package markdown-mode)

;; Clojure
(use-package clojure-mode
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.edn\\'" . clojure-mode)))

;; Lisp
(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package helm-ag)
(use-package helm-projectile)

; my functions
(defun init-file ()
  "~/.emacs.d/init.el")

(defun find-init-file ()
  (interactive)
  (find-file (init-file)))

(defun reload-init-file ()
  (interactive)
  (load-file (init-file)))

(defun smart-find-files ()
  ; thanks https://emacs.stackexchange.com/a/18004/28461
  "Use projectile only if we're in a project."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files "/home/tom/"))) ; FIXME would be nice if it used (pwd), but that doesn't work
    ;; (helm-find-files (pwd))))
