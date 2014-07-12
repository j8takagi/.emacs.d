(let ((elpafiles (directory-files "~/.emacs.d/elpa" 1 directory-files-no-dot-files-regexp)))
  (dolist (dir elpafiles elpafiles)
    (if (and (file-accessible-directory-p dir) (string-match "\\([^/]+\\)-[0-9.]+$" dir))
        (package--make-autoloads-and-compile (substring dir (match-beginning 1) (match-end 1)) dir))))
