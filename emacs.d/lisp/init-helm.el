(defun bind-extra-helm-actions ()
  "Binds my extra helm actions"

  ;; buffer - select
  (defun helm-buffer-ace-window-select (buffer)
    ;; thanks https://emacs.stackexchange.com/a/17082/28461
    "Use ‘ace-window’ to select a window for BUFFER."
    (ace-select-window)
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Show buffer with Ace window 'C-c w'" . helm-buffer-ace-window-select)
               :append)

  (defun helm-buffer-run-ace-window-select ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-select)))

  (define-key helm-buffer-map (kbd "C-c w") #'helm-buffer-run-ace-window-select)


  ;; buffer - split-right
  (defun helm-buffer-ace-window-split-right (buffer)
    "Use ‘ace-window’ to select a window to split-right the BUFFER."
    (ace-select-window)
    (select-window (split-window-right))
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Split-right buffer in Ace window ‘C-c v'" . helm-buffer-ace-window-split-right)
               :append)

  (defun helm-buffer-run-ace-window-split-right ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-split-right)))

  (define-key helm-buffer-map (kbd "C-c v") #'helm-buffer-run-ace-window-split-right)


  ;; buffer - split-below
  (defun helm-buffer-ace-window-split-below (buffer)
    "Use ‘ace-window’ to select a window to split-below the BUFFER."
    (ace-select-window)
    (select-window (split-window-below))
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Split-below buffer in Ace window ‘C-c s'" . helm-buffer-ace-window-split-below)
               :append)

  (defun helm-buffer-run-ace-window-split-below ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-split-below)))

  (define-key helm-buffer-map (kbd "C-c s") #'helm-buffer-run-ace-window-split-below)


  ;; find-files - select
  (defun helm-find-files-ace-window-select (the-file)
    "Use ‘ace-window’ to select a window for FILE."
    (ace-select-window)
    (find-file the-file))

  (add-to-list 'helm-find-files-actions
               '("Show file with Ace window ‘C-c w'" . helm-find-files-ace-window-select)
               :append)

  (defun helm-find-files-run-ace-window-select ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-find-files-ace-window-select)))

  ;; FIXME these keys seem to be bound in the value dump in help, but they don't work
  (define-key helm-find-files-map (kbd "C-c w") #'helm-find-files-run-ace-window-select)
  (define-key helm-projectile-find-file-map (kbd "C-c w") #'helm-find-files-run-ace-window-select)
  )


(provide 'init-helm)
