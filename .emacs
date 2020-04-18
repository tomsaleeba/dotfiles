(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; TODO
;; - auto enable server-start (lazily?)
;; - zt and zb have a few lines of padding
;; - persist search highlight, https://github.com/juanjux/evil-search-highlight-persist
;; - camel case word navigation
;; - make help open in a new split
;; - add quick buffer list
;; - get projectile prefix group on leader
;; - add leader shortcut for last buffer
;; - figure out visualstar and visual selection weirdness

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

(defun init-file ()
  "~/.emacs")

(defun find-init-file ()
  (interactive)
  (find-file (init-file)))

(defun reload-init-file ()
  (interactive)
  (load-file (init-file)))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; (define-key projectile-mode-map (kbd "SPC p") 'projectile-command-map)
  ;; FIXME these conflict with my i3 shortcuts
  (define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
  (define-key projectile-mode-map [?\s-f] 'projectile-find-file)
  (define-key projectile-mode-map [?\s-g] 'projectile-grep))

;; Evil
(setq evil-want-C-u-scroll t)
(use-package evil
  :demand
  :config
  (evil-mode t)
  ;; in vim, backtick remembers the col after a :move, evil doesn't
  ;; TODO should be  "mz :m+ <return> `z"
  (define-key evil-normal-state-map (kbd "M-j") (kbd ":m+ <return>"))
  ;; TODO should be  "mz :m-2 <return> `z"
  (define-key evil-normal-state-map (kbd "M-k") (kbd ":m-2 <return>"))
  ;; FIXME vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
  (setq
   ;; thanks https://github.com/emacs-evil/evil/issues/486#issuecomment-273754354
   evil-split-window-below t
   evil-vsplit-window-right t)
  (define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)
  (define-key evil-normal-state-map (kbd "C-f") 'helm-find-files))
; FIXME if we're in a project, use projectile

(use-package evil-leader
  :demand
  :config
  (progn
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)
    )
  (evil-leader/set-key "<SPC>" 'evil-window-next)
  (evil-leader/set-key "/" 'comment-line)
  (evil-leader/set-key "g" 'helm-projectile-ag)
  ;; buffers
  (evil-leader/set-key "b b" 'helm-buffers-list)
  ;; files
  (evil-leader/set-key "f s" 'save-buffer)
  (evil-leader/set-key "f e d" 'find-init-file)
  (evil-leader/set-key "f e R" 'reload-init-file)
  ;; motions
  (evil-leader/set-key "s s" 'avy-goto-char)
  (evil-leader/set-key "s S" 'avy-goto-char-2)
  (evil-leader/set-key "s l" 'avy-goto-line)
  (evil-leader/set-key "s w" 'avy-goto-word-0)
  (evil-leader/set-key "s W" 'avy-goto-word-1)
  ;; projects
  (evil-leader/set-key "p f" 'helm-projectile-find-file)
  ;; windows
  (evil-leader/set-key "w h" 'evil-window-left)
  (evil-leader/set-key "w j" 'evil-window-down)
  (evil-leader/set-key "w k" 'evil-window-up)
  (evil-leader/set-key "w l" 'evil-window-right)
  )

(use-package evil-visualstar
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile which-key vue-mode use-package telephone-line projectile paredit markdown-mode helm-ag gruvbox-theme evil-visualstar evil-visual-mark-mode evil-leader evil-god-state evil-easymotion clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
