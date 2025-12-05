;;; copy-cmds.el --- Copy test commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides helpers for copying project-relative test commands.

;;; Code:

(defun copy-cmds--get-relative-path ()
  "Get the relative path from repository root to the current buffer's file."
  (let ((file (or (buffer-file-name)
                  (user-error "Current buffer is not visiting a file"))))
    (let ((root (or (locate-dominating-file file ".git")
                    (user-error "Could not locate repository root from %s" file))))
      (file-relative-name file root))))

(defun copy-cmds--copy-to-clipboard (text)
  "Copy TEXT to the clipboard using pbcopy."
  (let ((pbcopy (or (executable-find "pbcopy")
                    (user-error "`pbcopy` executable not found"))))
    (with-temp-buffer
      (insert text)
      (let ((exit-code (call-process-region (point-min) (point-max)
                                            pbcopy nil nil nil)))
        (if (zerop exit-code)
            (message "Copied to clipboard: %s" text)
          (user-error "pbcopy failed with exit code %s" exit-code))))))

;;;###autoload
(defun copy-tdd-cmd ()
  "Copy an npm TDD command for the current buffer file.
The command is of the form `npm run tdd-path -- <relative-path>` where
`<relative-path>` is the path from the repository root to the buffer's file."
  (interactive)
  (let* ((relative (copy-cmds--get-relative-path))
         (command (format "npm run tdd-path -- %s" relative)))
    (copy-cmds--copy-to-clipboard command)))

;;;###autoload
(defun copy-rspec-cmd ()
  "Copy an RSpec command for the current buffer file.
The command is of the form `bundle exec rspec <relative-path>` where
`<relative-path>` is the path from the repository root to the buffer's file."
  (interactive)
  (let* ((relative (copy-cmds--get-relative-path))
         (command (format "bundle exec rspec %s" relative)))
    (copy-cmds--copy-to-clipboard command)))

(provide 'copy-cmds)
;;; copy-cmds.el ends here
