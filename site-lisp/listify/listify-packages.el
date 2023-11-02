;;; listify-packages.el ---

;; Copyright (C) 2018-2023 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(require 'package)
(require 'listify)

(unless package--initialized
  (package-initialize))

(defun listify-packages-add-archives (&rest archives)
  "Add package archives to `package-archives'.
Each element of ARCHIVES has the form (ID LOCATION)."
  (dolist (aarch archives)
    (update-or-add-alist 'package-archives (car aarch) (cadr aarch)))
  (condition-case err
      (package-refresh-contents 1)
    (error
     (message "Warining: Fails to download package descriptions."))))

(defun listify-packages-install (pkg)
  (if (not (assq pkg package-archive-contents))
      (message "Warning: Package `%s' is NOT found on archives." pkg)
    (message "Installation of package `%s' begins." pkg)
    (condition-case err
        (package-install pkg)
      (error
       (message "Warining: Fails to install package `%s'.\n%s: %s"
                pkg (car err) (cadr err))))))

(defun listify-packages-required (pkg)
  (let ((pkgdesc (assq pkg package-alist)))
    (when pkgdesc
      (mapcar 'car (package-desc-reqs (cadr pkgdesc))))))

(defun listify-packages-list-dependent (pkg &optional pkg-from)
  "Check whether PKG is installed. When not installed, the installation begins.
If the package requires other packages, installation of the packges begin recursively.
This function returns the list of (`package' `required package')."
  (let (pkgs req-pkgs)
    (unless (package-installed-p pkg)
      (when pkg-from
        (message "Package `%s' required from `%s' is not installed." pkg pkg-from))
      (listify-packages-install pkg))
    (push (cons pkg pkg-from) pkgs)
    (dolist (req-pkg (listify-packages-required pkg))
      (dolist (rp (listify-packages-list-dependent req-pkg pkg))
        (push rp pkgs)))
    pkgs))

(defun listify-packages-message-update (pkgs)
  (package-menu--refresh pkgs)
  (dolist (pkg (package-menu--find-upgrades))
    (message "Info: Package %s is updated. Version %s is available."
             (car pkg) (package-desc-version (cdr pkg)))))

(defun listify-packages-message-unexpected (pkgs real-pkgs)
  (dolist (real-pkg real-pkgs)
    (unless (member real-pkg pkgs)
      (message "Info: Package %s is installed, but unexpected." real-pkg))))

(defun listify-packages-dependent-alist (pkg &optional pkg-from)
  "Return the list of dependent packages alist.
alist form is :
(`package' . `dependent package') ..."
  (let (pkgs req-pkgs)
    (unless (package-installed-p pkg)
      (when pkg-from
        (message "Package `%s' required from `%s' is not installed." pkg pkg-from))
      (listify-packages-install pkg))
    (unless (equal pkg 'emacs)
      (push (cons pkg pkg-from) pkgs)
      (dolist (req-pkg (listify-packages-required pkg))
        (dolist (rp (listify-packages-dependent-alist req-pkg pkg))
          (push rp pkgs))))
    (delete-dups pkgs)))

(defun listify-packages-dependent-list (pkg-from)
  (let (pkgs)
    (dolist (pkg
             (delete-dups
              (mapcar 'car (listify-packages-dependent-alist pkg-from))))
      (push pkg pkgs))
    pkgs))

(defun listify-packages-check (&rest packages)
  "Check the packages in PACKAGE, packages and dependent packages are
installed and updated, and unexpected packages are not installed."
  (unless package--initialized
    (package-initialize))
  (let (real-pkgs pkgs)
    (message "Specified packages - %s" packages)
    (setq real-pkgs (nreverse (mapcar 'car package-alist)))
    (message "Installed packages - %s" real-pkgs)
    (dolist (pkg-from packages)
      (dolist (pkg (listify-packages-dependent-list pkg-from))
        (unless (equal pkg pkg-from)
          (message "Package `%s' is required from `%s'." pkg pkg-from))
        (push pkg pkgs)))
    (delete-dups pkgs)
    ;; updated packages
    (listify-packages-message-update real-pkgs)
    ;; installed packages not in REQ-PKG-LIST
    (listify-packages-message-unexpected pkgs real-pkgs)
    pkgs))

(provide 'listify-packages)
;;; listify-packages.el ends here
