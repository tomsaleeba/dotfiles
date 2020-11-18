(defun bind-extra-helm-actions ()
  "Binds my extra helm actions"

  (defun tom/select-window ()
    (if (> (count-windows) 1)
        (let ((aw-dispatch-always t))
          (ace-select-window))
      (ace-select-window))
    )

  ;; buffer - select
  (defun helm-buffer-ace-window-select (buffer)
    ;; thanks https://emacs.stackexchange.com/a/17082/28461
    ;; although it seems that we don't need to bind projectile keys
    ;; like the post says should we use the
    ;; helm-find-files-after-init-hook as mentioned in
    ;; https://occasionallycogent.com/emacs_custom_helm_actions/index.html
    ;; ?
    "Use ‘ace-window’ to select a window for BUFFER."
    (tom/select-window)
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Show buffer with Ace window 'C-c w'"
                 . helm-buffer-ace-window-select)
               :append)

  (defun helm-buffer-run-ace-window-select ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-select)))

  (define-key helm-buffer-map
    (kbd "C-c w") #'helm-buffer-run-ace-window-select)


  ;; buffer - split-right
  (defun helm-buffer-ace-window-split-right (buffer)
    "Use ‘ace-window’ to select a window to split-right the BUFFER."
    (tom/select-window)
    (select-window (split-window-right))
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Split-right buffer in Ace window ‘C-c v'"
                 . helm-buffer-ace-window-split-right)
               :append)

  (defun helm-buffer-run-ace-window-split-right ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-split-right)))

  (define-key helm-buffer-map
    (kbd "C-c v") #'helm-buffer-run-ace-window-split-right)


  ;; buffer - split-below
  (defun helm-buffer-ace-window-split-below (buffer)
    "Use ‘ace-window’ to select a window to split-below the BUFFER."
    (tom/select-window)
    (select-window (split-window-below))
    (switch-to-buffer buffer))

  (add-to-list 'helm-type-buffer-actions
               '("Split-below buffer in Ace window ‘C-c s'"
                 . helm-buffer-ace-window-split-below)
               :append)

  (defun helm-buffer-run-ace-window-split-below ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-buffer-ace-window-split-below)))

  (define-key helm-buffer-map
    (kbd "C-c s") #'helm-buffer-run-ace-window-split-below)


  ;; find-files - select
  (defun helm-find-files-ace-window-select (the-file)
    "Use ‘ace-window’ to select a window for FILE."
    (tom/select-window)
    (find-file the-file))

  (add-to-list 'helm-find-files-actions
               '("Show file with Ace window ‘C-c w'"
                 . helm-find-files-ace-window-select)
               :append)

  (defun helm-find-files-run-ace-window-select ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-find-files-ace-window-select)))

  (define-key helm-find-files-map
    (kbd "C-c w") #'helm-find-files-run-ace-window-select)


  ;; find-files  - split-right
  (defun helm-find-files-ace-window-split-right (the-file)
    "Use ‘ace-window’ to select a window to split-right the FILE."
    (tom/select-window)
    (select-window (split-window-right))
    (find-file the-file))

  (dolist (e '(helm-find-files-actions helm-type-file-actions))
    ; recentf uses the type-file-actions
    (add-to-list e
                 '("Split-right file in Ace window ‘C-c v'"
                   . helm-find-files-ace-window-split-right)
                 :append)
    )

  (defun helm-find-files-run-ace-window-split-right ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-find-files-ace-window-split-right)))

  (dolist (e (list helm-find-files-map helm-generic-files-map))
    ; recentf uses generic-files-map
    (define-key e
      (kbd "C-c v") #'helm-find-files-run-ace-window-split-right)
    )


  ;; find-files - split-below
  (defun helm-find-files-ace-window-split-below (the-file)
    "Use ‘ace-window’ to select a window to split-below the FILE."
    (tom/select-window)
    (select-window (split-window-below))
    (find-file the-file))

  (dolist (e '(helm-find-files-actions helm-type-file-actions))
    ; recentf uses the type-file-actions
    (add-to-list e
                 '("Split-below file in Ace window ‘C-c s'"
                   . helm-find-files-ace-window-split-below)
                 :append)
    )

  (defun helm-find-files-run-ace-window-split-below ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-find-files-ace-window-split-below)))

  (dolist (e (list helm-find-files-map helm-generic-files-map))
    ; recentf uses generic-files-map
    (define-key e
      (kbd "C-c s") #'helm-find-files-run-ace-window-split-below)
    )
  )

(provide 'init-helm)
