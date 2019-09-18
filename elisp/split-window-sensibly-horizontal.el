;;; split-window-sensibly-horizontal --- split window prefer horizontal -*- lexical-binding: t; -*-
;;; Commentary:

;; https://emacs.stackexchange.com/questions/39034/prefer-vertical-splits-over-horizontal-ones

;;; Code:

(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on `split-window-sensibly`, but designed to prefer a horizontal split.
i.e. windows tiled side-by-side WINDOW."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
               (split-window-right))))))))

;;;###autoload
(defun split-window-sensibly-horizontal (&optional window)
  "Split the current window prefering horizontal.
An optional WINDOW to split can be provided."
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(provide 'split-window-sensibly-horizontal)
;;; split-window-sensibly-horizontal.el ends here
