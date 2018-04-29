(defun dot-name (exp)
  "ノードの識別子を変換する
   exp: symbol
    'living-room > LIVING_ROOM
    'foo! > FOO_"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
