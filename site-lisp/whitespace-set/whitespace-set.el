(require 'whitespace)

;; whitespaceを無効にするメジャーモード
(defcustom whitespace-disabled-major-mode-list nil
  "Major mode of disable whitespace-mode even if whitespace-mode is enabled"
  :type '(repeat symbol)
  :group 'whitespace)

;; メジャーモード設定後、バッファーが読み取り専用の場合と、
;; whitespaceを無効にするメジャーモードの場合以外、whitespaceを有効にする
(defun set-whitespace-mode ()
  "Enable whitespace mode unless the buffer is read only or the major
mode is in the disabled list."
  (if (or buffer-read-only (member major-mode whitespace-disabled-major-mode-list))
      (whitespace-mode 0)
    (whitespace-mode 1)))

(provide 'whitespace-set)
