(defun all-packages-make-autoloads-and-compile ()
  "Generate autoloads and do byte-compilation for all packages in the user directory."
  (interactive)
  (let*
      ((pkgdir (expand-file-name package-user-dir))
       (pkgfiles
        (directory-files pkgdir 1
                         directory-files-no-dot-files-regexp)))
    (dolist (dir pkgfiles pkgfiles)
      (when (and
             (file-accessible-directory-p dir)
             (string-match "\\([^/]+\\)-[0-9.]+$" dir))
        (package--make-autoloads-and-compile
         (substring dir (match-beginning 1) (match-end 1)) dir)))))

(provide 'packages-compile)
