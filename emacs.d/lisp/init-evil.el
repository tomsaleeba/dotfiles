(defun define-extra-evil-commands ()
  (evil-define-command evil-quit-tab-aware (&optional force)
    "Wrap the evil-quit function so closing the last window in a tab
    will only close that tab if there are still other tabs open"
    :repeat nil
    (interactive "<!>")
    (let ((tab-count (length (funcall tab-bar-tabs-function))))
      (if (and (= (count-windows) 1) (> tab-count 1))
          (tab-close)
        (evil-quit force)))
    )
  )

(defun do-evil-config ()
  (define-key evil-normal-state-map (kbd "M-j") 'drag-stuff-down)
  (define-key evil-normal-state-map (kbd "M-k") 'drag-stuff-up)
  (setq
   ;; thanks https://github.com/emacs-evil/evil/issues/486#issuecomment-273754354
   evil-split-window-below t
   evil-vsplit-window-right t)
  (define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)
  (define-key evil-normal-state-map (kbd "C-f") 'smart-find-files)
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-extra-evil-commands)
  (evil-ex-define-cmd "q[uit]" 'evil-quit-tab-aware)
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
  (evil-leader/set-key "f n" 'find-notes-file)
  ;; motions
  (evil-leader/set-key "j" 'tom/avy-goto-char-below)
  (evil-leader/set-key "J" 'avy-goto-char)
  (evil-leader/set-key "s s" 'avy-goto-char)
  (evil-leader/set-key "s S" 'avy-goto-char-2)
  (evil-leader/set-key "s l" 'avy-goto-line)
  (evil-leader/set-key "s w" 'avy-goto-word-0)
  (evil-leader/set-key "s W" 'avy-goto-word-1)
  ;; nav
  (evil-leader/set-key "n f" 'neotree-find)
  (evil-leader/set-key "n q" 'neotree-hide)
  ;; org mode
  (evil-leader/set-key "o l" 'org-store-link)
  (evil-leader/set-key "o a" 'org-agenda)
  (evil-leader/set-key "o c" 'org-capture)
  ;; projects
  (evil-leader/set-key "p f" 'helm-projectile-find-file)
  ;; replace
  (evil-leader/set-key "r" 'tom/evil-replace-visual-selection-in-buffer)
  ;; windows
  (evil-leader/set-key "w h" 'evil-window-left)
  (evil-leader/set-key "w j" 'evil-window-down)
  (evil-leader/set-key "w k" 'evil-window-up)
  (evil-leader/set-key "w l" 'evil-window-right)
  (evil-leader/set-key "w n" 'make-frame)
  (evil-leader/set-key "w o" 'delete-other-windows)
  (evil-leader/set-key "w q" 'evil-quit)
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
  (use-package evil-anzu)
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
