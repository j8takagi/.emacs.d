;;; mpv-transcription.el --- 

;; Copyright (C) 2015 by Kazuhito Takagi

;; Authors: Kazuhito Takagi
;; Keywords: 

;;; Commentary:


;;; Code:


(defvar mpv-transcription-default-pace 16)

(defvar mpv-transcription-current-time 0)

(defvar mpv-transcription-speakers '("芹生" "高木" "高橋" "村杉"))

(defvar mpv-transcription-audio-file "/Users/kazubito/2015_09/monogatari_rodo/15.07.24 芹生さん/150724_001（個人ＩＣ）.MP3")

(defun mpv-transcription-get-previous-time ()
  (let ((sec 0))
    (when (re-search-backward "^\\[\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\)\\]" nil t 1)
      (setq sec (string-to-number (buffer-substring (match-beginning 3) (match-end 3))))
      (setq sec (+ sec (* (string-to-number (buffer-substring (match-beginning 2) (match-end 2))) 60)))
      (setq sec (+ sec (* (string-to-number (buffer-substring (match-beginning 1) (match-end 1))) 3600))))
    sec))

(defun mpv-transcription-get-previous-speaker ()
  (let (speaker)
    (when (re-search-backward " \\(.+\\):" nil t 1)
      (setq speaker (buffer-substring (match-beginning 1) (match-end 1))))))

(defun mpv-transcription-insert-speak ()
  (interactive)
  (let ((p (point)) timestr speaker)
    (setq timestr
          (format-time-string
           "%T"
           (seconds-to-time
            (+ (mpv-transcription-get-previous-time) mpv-transcription-default-pace)) 1))
    (goto-char p)
    (setq speaker (mpv-transcription-get-previous-speaker))
    (goto-char p)
    (insert "\n")
    (insert (concat "[" timestr "] " speaker ": "))
    (mpv-transcription-start timestr)))

(defun mpv-transcription-insert-new-speak ()
  (interactive)
  (let ((p (point)) timestr)
    (setq timestr
          (read-string
           "時間? "
           (format-time-string
            "%T"
            (seconds-to-time
             (+ (mpv-transcription-get-previous-time) mpv-transcription-default-pace)) 1)))
    (goto-char p)
    (when (> p 1)
      (insert "\n\n"))
    (insert
     (concat
      "[" timestr "]"
      " "
      (completing-read "発言者? " mpv-transcription-speakers nil t nil 'mpv-transcription-speakers)
      ": "))
    (mpv-transcription-start timestr)))

(defun mpv-transcription-play-inline-time ()
  (interactive)
  (let ((p (point)) timestr)
    (beginning-of-line)
    (when (re-search-forward "\\[\\([0-9]\\{1,2\\}:[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\)\\]" (line-end-position) t 1)
      (setq timestr (buffer-substring (match-beginning 1) (match-end 1)))
      (mpv-transcription-start timestr))
    (goto-char p)))

(defun mpv-transcription-start (timestr)
  (interactive)
  (let ((cmdstr) (buf (get-buffer-create "*mpv*")))
    (setq cmdstr
          (concat
           "mpv \"" mpv-transcription-audio-file "\" "
           "--start=" timestr " "
           "--length=" (number-to-string mpv-transcription-default-pace)))
    (start-process-shell-command "mpv" buf cmdstr)))

(defun mpv-transcription-mode ()
  "This mode is for transcription using mpv."
  (interactive)
  (setq major-mode 'mpv-transcription-mode
        mode-name "mpv ts mode")
  (setq mpv-transcription-mode-map (make-keymap))
  (dolist
      (map
       '(
         ("<C-return>" mpv-transcription-insert-speak)
         ("<C-M-return>" mpv-transcription-insert-new-speak)
         ("S-SPC" mpv-transcription-play-inline-time)
         ))
    (let ((key (car map)) (func (nth 1 map)))
      (if (not (functionp func))
          (message "Warning: function `%s' is NOT defined." func)
        (define-key mpv-transcription-mode-map (kbd key) func))))
  (use-local-map mpv-transcription-mode-map))

(provide 'mpv-transcription)
;;; mpv-transcription.el ends here
