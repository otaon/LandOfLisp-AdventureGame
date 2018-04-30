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

(defun nodes->dot (nodes)
  "ノードのDOT情報を生成する
   nodes: list
    (nodes->dot '((1 (x1-a)) (a2 (x2-a)) (a3 (x3-a)) (a4 (4-a)))) >
    1[label=\"(1 (X1-A))\"];
    A2[label=\"(A2 (X2-A))\"];
    A3[label=\"(A3 (X3-A))\"];
    A4[label=\"(A4 (|4-A|))\"];"
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  "エッジをDOTフォーマットに変換する"
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  "DOTデータを完成させる"
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  "DOTファイルを画像にする
   fname: 出力先ファイル名
   thunk: ファイルに書き出す文字列を出力する関数"
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))


