(defun dot-name (exp)
  "ノードの識別子を変換する"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
