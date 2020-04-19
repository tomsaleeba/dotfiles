(defun do-evil-config ()
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
  (define-key evil-normal-state-map (kbd "C-f") 'smart-find-files)
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  )

(defun do-evil-leader-config ()
  (setq evil-leader/in-all-states t)
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-leader/set-key "<SPC>" 'ace-window)
  (evil-leader/set-key "/" 'comment-line)
  (evil-leader/set-key "g" 'helm-projectile-ag)
  ;; buffers
  (evil-leader/set-key "b b" 'helm-buffers-list)
  ;; files
  (evil-leader/set-key "f s" 'save-buffer)
  (evil-leader/set-key "f S" 'evil-write-all)
  (evil-leader/set-key "f e d" 'find-init-file)
  (evil-leader/set-key "f e R" 'reload-init-file)
  ;; motions
  (evil-leader/set-key "s s" 'avy-goto-char)
  (evil-leader/set-key "s S" 'avy-goto-char-2)
  (evil-leader/set-key "s l" 'avy-goto-line)
  (evil-leader/set-key "s w" 'avy-goto-word-0)
  (evil-leader/set-key "s W" 'avy-goto-word-1)
  ;; nav
  (evil-leader/set-key "n f" 'neotree-find)
  (evil-leader/set-key "n q" 'neotree-hide)
  ;; projects
  (evil-leader/set-key "p f" 'helm-projectile-find-file)
  ;; windows
  (evil-leader/set-key "w h" 'evil-window-left)
  (evil-leader/set-key "w j" 'evil-window-down)
  (evil-leader/set-key "w k" 'evil-window-up)
  (evil-leader/set-key "w l" 'evil-window-right)
  )

(defun install-evil-deps ()
  (use-package evil-leader
    :demand
    :config
    (do-evil-leader-config)
    )

  (use-package evil-visualstar
    :config
    (setq evil-visualstar/persistent t)
    (global-evil-visualstar-mode))

  (use-package evil-surround
    :config
    (global-evil-surround-mode t))

  (use-package avy)
  )

(defun neotree-evil-config ()
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter-ace-window)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "|") 'neotree-enter-vertical-split)
  (evil-define-key 'normal neotree-mode-map (kbd "-") 'neotree-enter-horizontal-split)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(provide 'init-evil)
