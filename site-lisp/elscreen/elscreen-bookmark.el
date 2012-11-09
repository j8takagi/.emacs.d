(require 'bookmark)
(defun elscreen-bookmark-bmenu-list ()
  "Display a list of existing bookmarks in new screen.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (elscreen-create)
  (bookmark-maybe-load-default-file)
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "% Bookmark\n- --------\n")
    (add-text-properties (point-min) (point)
			 '(font-lock-face bookmark-menu-heading))
    (bookmark-maybe-sort-alist)
    (mapcar
     (lambda (full-record)
       ;; if a bookmark has an annotation, prepend a "*"
       ;; in the list of bookmarks.
       (let ((annotation (bookmark-get-annotation
                          (bookmark-name-from-full-record full-record))))
         (if (and annotation (not (string-equal annotation "")))
             (insert " *")
           (insert "  "))
	 (let ((start (point)))
	   (insert (bookmark-name-from-full-record full-record))
	   (if (and (display-color-p) (display-mouse-p))
	       (add-text-properties
		start
		(save-excursion (re-search-backward
				 "[^ \t]")
				(1+ (point)))
		'(mouse-face highlight
		  follow-link t
		  help-echo "mouse-2: go to this bookmark in other window")))
	   (insert "\n")
	   )))
     bookmark-alist))
  (goto-char (point-min))
  (forward-line 2)
  (bookmark-bmenu-mode)
  (if bookmark-bmenu-toggle-filenames
      (bookmark-bmenu-toggle-filenames t)))

(global-set-key "\C-zr" 'elscreen-bookmark-bmenu-list)
