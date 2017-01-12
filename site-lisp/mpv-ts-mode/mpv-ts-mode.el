;;; mpv-ts-mode.el --- 

;; Copyright (C) 2015 by Kazuhito Takagi

;; Authors: Kazuhito Takagi
;; Keywords: mpv transcription

;;; Commentary:


;;; Code:

(defgroup mpv-ts-mode nil
  "Transcription using mpv."
  :prefix "mpv-ts-"
  :group 'wp)

(defcustom mpv-ts-default-pace 5
  "再生ペースの初期値。秒単位。時刻挿入時と再生時に用いられる"
  :group 'mpv-ts-mode)

(defcustom mpv-ts-speakers nil
  "発言者のリスト"
  :group 'mpv-ts-mode)

(defcustom mpv-ts-speakers-string nil
  "発言者のリストの文字列"
  :group 'mpv-ts-mode)

(make-variable-buffer-local 'mpv-ts-speakers-string)
(put 'mpv-ts-speakers-string 'safe-local-variable 'string-or-null-p)

(defcustom mpv-ts-audio-file nil
  "再生する音声ファイル"
  :group 'mpv-ts-mode)

(make-variable-buffer-local 'mpv-ts-audio-file)
(put 'mpv-ts-audio-file 'safe-local-variable 'string-or-null-p)

(defcustom mpv-ts-mode-hook nil
  "Normal hook when entering `mpv-ts-mode'."
  :type 'hook
  :group 'mpv-ts-mode)

(defvar mpv-ts-time-pattern
  "\\(\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\)\\)"
  "時刻をあらわす文字列のパターン。\1は時刻全体、\2は時、\3は分、\4は秒")

(defvar mpv-ts-time-speaker-pattern
  (concat "\\[" mpv-ts-time-pattern "\\] \\([^:]*\\): ?")
  "時刻と発言者をあらわす文字列のパターン。\\1は時刻全体（00:00:00）、\\2は時、\\3は分、\\4は秒、\\5は発言者")

(defvar mpv-ts-process-buffer-name
  "*mpv*"
  "the name of buffer associate with mpv process.")

(defvar mpv-ts-mode-map nil)

(defun mpv-ts-second-to-time (second)
  "秒単位の数値を時刻を表す文字列 00:00:00 に変換する"
  (format-time-string "%T" (seconds-to-time second) 0))

(defun mpv-ts-time-to-second (time-string)
  "時刻を表す文字列 00:00:00 を秒単位の数値に変換する"
  (let (sec min hour)
    (string-match mpv-ts-time-pattern time-string)
    (setq sec (string-to-number (substring time-string (match-beginning 4) (match-end 4))))
    (setq min (string-to-number (substring time-string (match-beginning 3) (match-end 3))))
    (setq hour (string-to-number (substring time-string (match-beginning 2) (match-end 2))))
    (+ sec (* 60 (+ min (* 60 hour))))))

(defun mpv-ts-time-add (time-string second)
  (mpv-ts-second-to-time (+ (mpv-ts-time-to-second time-string) second)))

(defun mpv-ts-current-line-time ()
  "現在の行から再生開始時刻を取得する"
  (let (time)
    (save-excursion
      (beginning-of-line)
      (when (looking-at mpv-ts-time-speaker-pattern)
        (setq time (buffer-substring (match-beginning 1) (match-end 1))))
      time)))

(defun mpv-ts-next-time ()
  "次の行以降から再生開始時刻を取得する"
  (save-excursion
    (forward-line)
    (when (re-search-forward mpv-ts-time-speaker-pattern nil t 1)
      (buffer-substring (match-beginning 1) (match-end 1)))))

(defun mpv-ts-current-line-time-forward (&optional diff)
  "現在の行の再生開始時刻を先に進める"
  (interactive "p")
  (save-excursion
    (unless diff
      (setq diff 1))
    (let (new-time begin end)
      (forward-line 0)
      (when (looking-at mpv-ts-time-speaker-pattern)
        (goto-char (match-beginning 1))
        (setq end (match-end 1))
        (setq new-time (mpv-ts-time-add (buffer-substring (point) end) diff))
        (delete-region (point) end)
        (insert new-time)))))

(defun mpv-ts-current-line-time-backward (&optional diff)
  "現在の行の再生開始時刻を過去に戻す"
  (interactive "p")
  (unless diff
    (setq diff 1))
  (mpv-ts-current-line-time-forward (- diff)))

(defun mpv-ts-current-line-speaker ()
  "発言者を現在行から取得する"
  (save-excursion
    (forward-line 0)
    (when (looking-at mpv-ts-time-speaker-pattern)
      (buffer-substring (match-beginning 5) (match-end 5)))))

(defun mpv-ts-speakers ()
  "発言者のリスト"
  (when mpv-ts-speakers-string
    (split-string mpv-ts-speakers-string ":")))

(defun mpv-ts-read-speaker ()
  "発言者を入力させる"
  (unless mpv-ts-speakers
    (setq mpv-ts-speakers (mpv-ts-speakers)))
  (completing-read
   "発言者? "
   mpv-ts-speakers nil t nil 'mpv-ts-speakers))

(defun mpv-ts-replace-current-line-speaker ()
  (interactive)
  "現在行の発言者を別の発言者に置換する"
  (let ((speaker (mpv-ts-read-speaker)))
    (when speaker
      (save-excursion
        (forward-line 0)
        (when (looking-at mpv-ts-time-speaker-pattern)
          (goto-char (match-beginning 5))
          (delete-region (match-beginning 5) (match-end 5))
          (insert speaker))))))

(defun mpv-ts-insert-speak-prefix (time speaker)
  "再生開始時刻と発言者を挿入する"
  (insert "[" time "] " speaker ": "))

(defun mpv-ts-insert-speak-line (&optional pace)
  "再生開始時刻と前行と同じ発言者を新しい行に挿入する。
挿入する再生時刻は、現在行の時刻より再生ペース分だけ後の時刻"
  (interactive "P")
  (let ((prev-time (mpv-ts-current-line-time))
        speaker start-time)
    (when prev-time
      (when (null pace)
        (setq pace mpv-ts-default-pace))
      (setq start-time (mpv-ts-time-add prev-time pace))
      (setq speaker (mpv-ts-current-line-speaker))
      (insert "\n")
      (mpv-ts-insert-speak-prefix start-time speaker)
      (mpv-ts-play mpv-ts-audio-file start-time (mpv-ts-time-add start-time pace)))))

(defun mpv-ts-insert-new-speak-line (&optional pace)
  "再生開始時刻と新しい発言者を新しい行に挿入する。
挿入する再生開始時刻は、現在行の時刻より1秒だけ後の時刻"
  (interactive "P")
  (let (
        (mac-auto-ascii-mode 0)
        (prev-time (mpv-ts-current-line-time))
        start-time)
    (when prev-time
      (when (null pace)
        (setq pace mpv-ts-default-pace))
      (setq start-time (mpv-ts-time-add prev-time 1))
      (insert "\n\n"))
      (mpv-ts-insert-speak-prefix start-time (mpv-ts-read-speaker))
      ))

(defun mpv-ts-jump-eob (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))))

(defun mpv-ts-play (audio-file &optional start-time end-time)
  "mpvによる音声再生を開始"
    (let
        ((cmdstr)
         (buf (get-buffer-create mpv-ts-process-buffer-name))
         (curr-dir (expand-file-name (car cd-path)))
         (curr-proc (get-buffer-process mpv-ts-process-buffer-name)))
      (set-buffer buf)
      (cd curr-dir)
      (goto-char (point-max))
      (setq cmdstr (concat "mpv --vo=null \"" audio-file "\""))
      (when start-time
        (setq cmdstr (concat cmdstr " --start=" start-time)))
      (when end-time
        (setq cmdstr (concat cmdstr " --end=" end-time)))
      (setq buffer-read-only nil)
      (insert (format-time-string "[%Y/%m/%d %H:%M:%S]" (current-time)) " $ " cmdstr "\n")
      (when curr-proc
        (interrupt-process curr-proc))
      (start-process-shell-command "mpv" buf cmdstr)
      (run-with-timer 0 nil 'mpv-ts-jump-eob buf)
      (mpv-ts-display-process)
      (setq buffer-read-only 1)))

(defun mpv-ts-play-interrupt (&optional process)
  "mpvによる音声再生を中止"
  (interactive)
  (unless process
    (setq process (get-buffer-process mpv-ts-process-buffer-name)))
  (when process
    (interrupt-process process)
    (mpv-ts-display-process)))

(defun mpv-ts-display-process ()
  "mpvプロセスの表示"
  (save-excursion
    (let
        (
         (proc-win-height 4)
         (proc-win (get-buffer-window mpv-ts-process-buffer-name))
         (curr-win (selected-window)))
      (unless proc-win
        (split-window nil (- (+ proc-win-height 1)))
        (set-window-buffer (next-window) mpv-ts-process-buffer-name)
        (setq proc-win (next-window)))
      (set-window-point proc-win (point-max))
      (select-window curr-win))))

(defun mpv-ts-delete-process-window ()
  "mpvプロセスのウィンドウを削除"
  (interactive)
  (let ((proc-win (get-buffer-window mpv-ts-process-buffer-name)))
    (when proc-win
      (delete-window proc-win))))

(defun mpv-ts-delete-process-window-unless-mpv-ts ()
  "mpvプロセスのウィンドウを削除"
  (unless (eq major-mode 'mpv-ts-mode)
      (let ((proc-win (get-buffer-window mpv-ts-process-buffer-name)))
        (when proc-win
          (delete-window proc-win)))))

(defun mpv-ts-delete-process-change-mode ()
  "メジャーモードが変わったら、MPVプロセスのウィンドウを消す"
  (unless (eq major-mode "mpv-ts-mode")
    (mpv-ts-delete-process-window)))

(defun mpv-ts-line-empty-p (&optional n)
  "行が空の場合はtrueを返す"
  (save-excursion
    (if (null n)
        (setq n 0))
    (forward-line n)
    (looking-at "\n")))

(defun mpv-ts-play-current-line (&optional second)
  "現在行の再生開始時刻からmpvによる音声再生を開始"
  (interactive "P")
  (let ((current-time (mpv-ts-current-line-time))
        next-time)
    (if (null current-time)
        (message "Current line is not transcription.")
      (if second
          (setq next-time (mpv-ts-time-add current-time second))
        (setq next-time (mpv-ts-next-time)))
      (when (and next-time (mpv-ts-line-empty-p 1))
        (setq next-time (mpv-ts-time-add next-time 1)))
      (if (or (null next-time) (<= (mpv-ts-time-to-second next-time) (mpv-ts-time-to-second current-time)))
        (setq next-time (mpv-ts-time-add current-time mpv-ts-default-pace)))
      (mpv-ts-play mpv-ts-audio-file current-time next-time))))

(defun mpv-ts-forward-scroll-line (&optional arg)
  "行を引数分移動し、移動した行数分スクロールする。引数省略時は1行移動、スクロール"
  (interactive "p")
  (when (null arg)
    (setq arg 1))
  (forward-line arg)
  (scroll-up arg))

(defun mpv-ts-move-next-line-play ()
  "次の行に移動し、mpvによる音声再生を開始"
  (interactive)
  (mpv-ts-forward-scroll-line)
  (while (and (not (eobp)) (mpv-ts-line-empty-p))
    (mpv-ts-forward-scroll-line))
  (mpv-ts-move-beginning-of-scription)
  (mpv-ts-play-current-line))

(defun mpv-ts-move-beginning-of-scription ()
  "行内で、書き起こしの先頭に移動する"
  (interactive)
  (beginning-of-line)
  (when (re-search-forward mpv-ts-time-speaker-pattern nil t 1)
    (goto-char (match-end 0))))

(defun mpv-ts-next-line-time ()
  "次の行以降から再生開始時刻を取得する"
  (save-excursion
    (forward-line)
    (when (re-search-forward mpv-ts-time-speaker-pattern nil t 1)
      (buffer-substring (match-beginning 0) (match-end 0)))))

(defun mpv-ts-insert-unknwon-tag ()
  "不明のタグ【不明:】を挿入する"
  (interactive)
  (insert "【不明:】")
  (backward-char 1))

(defun mpv-ts-do-join-line ()
  "行の連結を実行する"
  (delete-region (match-beginning 0) (match-end 0))
  (delete-char -1))

(defun mpv-ts-join-line ()
  "行を連結する"
    (when (looking-at mpv-ts-time-speaker-pattern)
      (mpv-ts-do-join-line)))

(defun mpv-ts-join-next-line ()
  "次の行と連結する"
  (interactive)
  (save-excursion
    (forward-line)
    (mpv-ts-join-line)))

(defun mpv-ts-join-line-or-delete-time (prev-speaker)
  "現在行の発言者が引数と同じ場合は行を連結し、違う場合は時刻を削除する。返り値は、現在行の発言者"
  (let ((speaker ""))
    (when (looking-at mpv-ts-time-speaker-pattern)
      (setq speaker (buffer-substring (match-beginning 5) (match-end 5)))
      (if (string= speaker prev-speaker)
          (mpv-ts-do-join-line)
        (delete-region (match-beginning 0) (match-beginning 5))))
    speaker))

(defun mpv-ts-join-line-or-delete-time-buffer ()
  "バッファ全体で、現在行の発言者が前行と同じ場合は行を連結し、違う場合は時刻を削除する"
  (let ((prev-speaker ""))
    (goto-char (point-min))
    (while (not (eobp))
      (setq prev-speaker (mpv-ts-join-line-or-delete-time prev-speaker))
      (forward-line))))

(defun mpv-ts-delete-local-variables ()
  "ローカル変数の記述を削除する"
  (interactive)
  (let ((kill-whole-line 1) in-lvar)
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at ";; Local Variables:")
        (setq in-lvar t)
        (when (looking-back "\n\\(\n+\\)" 1 1)
              (delete-region (match-beginning 1) (match-end 1))))
      (if (not in-lvar)
          (forward-line)
        (when (looking-at ";; End:")
          (setq in-lvar nil))
        (kill-line)))))

(defun mpv-ts-create-simple-text ()
  (interactive)
  (save-excursion
    (let
        (new-buf
         (prev-speaker "") speaker)
      (setq new-buf (create-file-buffer (concat (buffer-file-name) ".txt")))
      (copy-to-buffer new-buf (point-min) (point-max))
      (set-buffer new-buf)
      (mpv-ts-join-line-or-delete-time-buffer)
      (mpv-ts-delete-local-variables))))

;;; Font lock
(require 'font-lock)

(defgroup mpv-ts-faces nil
  "Faces used in MPV TS Mode"
  :group 'mpv-ts-mode
  :group 'faces)

(defface mpv-ts-time-speaker-face
  '((t (:bold nil :foreground "navy")))
  "face for time in transcription."
  :group 'mpv-ts-mode)

(defface mpv-ts-unknown-tag-face
  '((t (:bold t :foreground "red")))
  "face for unknown tag."
  :group 'mpv-ts-mode)

(defun mpv-ts-mode ()
  "This mode is for transcription using mpv."
  (interactive)
  (setq major-mode 'mpv-ts-mode
        mode-name "mpv ts mode")
  (setq mpv-ts-mode-map (make-keymap))
  (dolist
      (map
       '(
         ("<C-S-escape>" mpv-ts-play-interrupt)
         ("<C-S-return>" mpv-ts-insert-new-speak-line)
         ("<S-return>" mpv-ts-insert-speak-line)
         ("C-+" mpv-ts-current-line-time-forward)
         ("C-S-SPC" mpv-ts-play-current-line)
         ("C-S-a" mpv-ts-move-beginning-of-scription)
         ("C-S-j" mpv-ts-join-next-line)
         ("C-S-n" mpv-ts-move-next-line-play)
         ("C-S-s" mpv-ts-replace-current-line-speaker)
         ("C-_" mpv-ts-current-line-time-backward)
         ("C-c k" mpv-ts-delete-process-window)
         ("C-c u" mpv-ts-insert-unknwon-tag)
         ))
    (let ((key (car map)) (func (nth 1 map)))
      (if (not (functionp func))
          (message "Warning: function `%s' is NOT defined." func)
        (define-key mpv-ts-mode-map (kbd key) func))))
  (use-local-map mpv-ts-mode-map)
  (setq font-lock-defaults
        '((
          ("^\\[[0-9]+:[0-5][0-9]:[0-5][0-9]\\] [^ ]*: " . 'mpv-ts-time-speaker-face)
          ("【不明:.*?】" . 'mpv-ts-unknown-tag-face)
          ("^[ \\t]*;.+$" . 'font-lock-comment-face)
          )))
  (dolist
      (hook
       '(
         window-configuration-change-hook
         ))
    (add-hook hook 'mpv-ts-delete-process-window-unless-mpv-ts)))

(provide 'mpv-ts-mode)
;;; mpv-ts-mode.el ends here
