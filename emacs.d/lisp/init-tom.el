;; thanks https://github.com/redguardtoo/evil-mark-replace
(defun tom/evil-replace-visual-selection-in-buffer ()
  "Prepopulate a buffer wide substitution with the visual selection"
  (interactive)
  (let ((the-selection (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        escaped-the-selection)
    (unless the-selection (setq the-selection (read-string "String to be replaced:")) )

    (setq escaped-the-selection (replace-regexp-in-string "\\$" "\\\\$" the-selection))

    ;; quit the active region
    (if (region-active-p) (set-mark nil))

    (save-excursion
      (evil-ex (concat "%s/" escaped-the-selection "/")))))
(provide 'init-tom)
