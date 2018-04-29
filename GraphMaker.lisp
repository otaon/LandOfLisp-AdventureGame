(defun dot-name (exp)
  "ノードの識別子を変換する
   exp: symbol
    'living-room > LIVING_ROOM
    'foo! > FOO_"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

; ラベル名の最大文字数
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  "グラフのノードにラベルをつける
   exp: symbol
    *max-label-length*より長い文字列 > 文末に...がついた文字列"
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

