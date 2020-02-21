;;; npm-bin-utils.el --- Functions for finding executables in node_modules -*- lexical-binding: t; -*-

;;; Commentary:

;; Some utility functions for finding parent node_modules/.bin directories and
;; searching them for executables.

;;; Code:

(defun npm-bin-utils--join (root &rest dirs)
  "Join the directories DIRS to the file ROOT.
This always returns a directory.  None of DIRS should have a preceding /.
While DIRS can be a path like foo/bar/baz, this is not recommended for
portability."
  ;; replace with mapconcat? or probably dolist?
  (let ((parts (list (file-name-as-directory root))))
    (dolist (part dirs) (push (file-name-as-directory part) parts))
    (string-join (nreverse parts))))

(defun npm-bin-utils--parent-dir (dir-or-file)
  "Get the parent directory of DIR-OR-FILE."
  (file-name-directory (directory-file-name dir-or-file)))

;;;###autoload
(defun npm-bin-utils-foreach-dir (func &optional startdir)
  "Call FUNC with each parent node_modules/.bin dir starting with the closest.
This function starts searching in STARTDIR if provided, otherwise it starts in
the directory of the open buffer or `default-directory` if the buffer is not
backed by a file.

When FUNC returns a non-nil value we stop iterating on parent node_modules/.bin
directories and returns the value that FUNC returned."
  (let* ((start (or startdir buffer-file-name default-directory))
         (current-dir (locate-dominating-file start "node_modules"))
         (bin-dir-temp nil)
         (parent-dir-temp nil)
         (done nil))
    (while (and current-dir (not done))
      ;; Normalize out ~
      (setq bin-dir-temp (expand-file-name
                  (npm-bin-utils--join current-dir "node_modules" ".bin")))

      ;; Is node_modules guaranteed to have a .bin?
      (when (file-directory-p bin-dir-temp)
        (setq done (funcall func bin-dir-temp)))

      ;; prevent infinite recursion when /node_modules/.bin exists in root of fs
      (setq parent-dir-temp (npm-bin-utils--parent-dir current-dir))
      (if (string= current-dir parent-dir-temp)
          (setq current-dir nil)
        (setq current-dir parent-dir-temp)))
    done))

;;;###autoload
(defun npm-bin-utils-find-dirs (&optional startdir)
  "Return a list of absolute paths to parent node_module/.bin directories.

The list returned by this function is ordered from closest node_modules to
furthest.

This function starts searching in STARTDIR if provided, otherwise it starts in
the directory of the open buffer or `default-directory` if the buffer is not
backed by a file."
  (let ((dirs '()))
    (defun add-to-dirs (x) (progn (push x dirs) nil))
    (npm-bin-utils-foreach-dir 'add-to-dirs startdir)

    ;; reverse dirs so that the first item in the list is the closest node_modules
    (reverse dirs)))

;;;###autoload
(defun npm-bin-utils-find (bin &optional startdir)
  "Find the binary BIN in the closest possible node_modules/.bin directory.

If the closest .bin directory does not contain BIN then parent directories are
also searched which is desirable in a monorepo.

This function starts searching in STARTDIR if provided, otherwise it starts in
the directory of the open buffer or `default-directory` if the buffer is not
backed by a file."
  (defun bin-exists-helper (bindir)
    (let ((binfile (expand-file-name bin bindir)))
      (message binfile)
      (if (file-exists-p binfile) binfile nil)))

  (npm-bin-utils-foreach-dir 'bin-exists-helper startdir))

;;;###autoload
(defun npm-bin-utils-add-to-path ()
  "Add all parent node_module/.bin directories to `exec-path`.

The closest node_module/.bin folder is added to `exec-path` first, meaning
binaries will resolve with the typical node resolution strategy.  This is
especially useful for monorepos where binaries like `eslint` may be at the top
of the monorepo if you are using a build like vue-cli.

Note that this function adds absolute paths to the end of `exec-path` and also
makes the variable buffer local."
  (make-local-variable 'exec-path)
  (defun add-to-exec (path) (progn
                              (add-to-list 'exec-path path t)
                              nil))
  (npm-bin-utils-foreach-dir 'add-to-exec))

(provide 'npm-bin-utils)
;;; npm-bin-utils.el ends here
