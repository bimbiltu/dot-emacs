;;; copy-tdd-cmd.el --- Copy npm TDD command -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a helper for copying a project-relative npm TDD command.

;;; Code:

;;;###autoload
(defun copy-tdd-cmd ()
  "Copy an npm TDD command for the current buffer file.
The command is of the form `npm run tdd-path -- <relative-path>` where
`<relative-path>` is the path from the repository root to the buffer's file."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (user-error "Current buffer is not visiting a file"))))
    (let* ((root (or (locate-dominating-file file ".git")
                    (user-error "Could not locate repository root from %s" file)))
           (relative (file-relative-name file root))
           (command (format "npm run tdd-path -- %s" relative))
           (pbcopy (or (executable-find "pbcopy")
                       (user-error "`pbcopy` executable not found"))))
      (with-temp-buffer
        (insert command)
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              pbcopy nil nil nil)))
          (if (zerop exit-code)
              (message "Copied to clipboard: %s" command)
            (user-error "pbcopy failed with exit code %s" exit-code)))))))

(provide 'copy-tdd-cmd)

;;; copy-tdd-cmd.el ends here
