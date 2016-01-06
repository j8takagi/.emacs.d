;;; my-init.el --- 

;; Copyright (C) 2016 by Kazubito Takagi

;; Authors: Kazubito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:
(defun my-init-require (feature)
  "Require FEATURE, and the result is written into the `*Messages*' buffer."
  (if (featurep feature)
      (message "Info: Feature `%s' is already required." feature)
    (if (not (locate-library (symbol-name feature)))
        (message "Warning: Feature `%s' is NOT found." feature)
      (condition-case err
          (progn
            (require feature)
            (message "Feature `%s' is required." feature))
        (error (message "Warning: Fails to require feature `%s'.\n%s: %s" feature (car err) (cadr err)))))))

(defun my-init-install-package (pkg &optional pkg-from)
  "引数として指定されたパッケージがインストールされているかチェックし、
未インストールの場合はインストールを実行する。
パッケージが別パッケージを要求していた場合は、要求されたパッケージも再帰的にチェック・インストールする。
返り値は、(パッケージ 要求するパッケージ)のリスト"
  (let (pkgs req-pkgs pkg-desc)
    (unless (package-installed-p pkg)
      (if pkg-from
          (message "Package %s required from %s is not installed." pkg pkg-from))
      (if (not (assq pkg package-archive-contents))
          (message "Warning: Package `%s' is NOT found on archives." pkg)
        (message "Installation of package `%s' begins." pkg)
        (condition-case err
            (package-install pkg)
          (error (message "Warining: Fails to install package `%s'.\n%s: %s" pkg (car err) (cadr err))))))
    (when (setq pkg-desc (assq pkg package-alist))
      (add-to-list 'pkgs `(,pkg ,pkg-from) 1)
      (dolist (req-pkg (mapcar 'car (package-desc-reqs (cadr pkg-desc))))
        (dolist (rp (my-init-install-package req-pkg pkg))
          (add-to-list 'pkgs rp 1))))
    pkgs))

(provide 'my-init)
;;; my-init.el ends here
