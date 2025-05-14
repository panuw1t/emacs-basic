(defvar fk/default-scroll-lines 15)

(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-interpolation-factor 1.0)
  :bind
  (([remap scroll-up-command]   . fk/pixel-scroll-up-command)
   ([remap scroll-down-command] . fk/pixel-scroll-down-command)
   ([remap recenter-top-bottom] . fk/pixel-scroll-recenter-top-bottom))
  :hook
  (dashboard-after-initialize . pixel-scroll-precision-mode)
  :config
  (defun fk/pixel-scroll-up-command ()
    "Similar to `scroll-up-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (- (* fk/default-scroll-lines (line-pixel-height)))))

  (defun fk/pixel-scroll-down-command ()
    "Similar to `scroll-down-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (* fk/default-scroll-lines (line-pixel-height))))

  ;; TODO: fix window position after scrolling is done
  ;; (run-at-time pixel-dead-time nil (lambda ()
  ;;                                    (unless (pixel-scroll-in-rush-p)
  ;;                                      (set-window-vscroll nil 0 t))))

  (defun fk/pixel-scroll-recenter-top-bottom ()
    "Similar to `recenter-top-bottom' but with pixel scrolling."
    (interactive)
    (let* ((current-row (cdr (nth 6 (posn-at-point))))
           (target-row (save-window-excursion
                         (recenter-top-bottom)
                         (cdr (nth 6 (posn-at-point)))))
           (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
      (pixel-scroll-precision-interpolate distance-in-pixels))))
