; ゲーム中の場所の描写
(defparameter *nodes* '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beatiful garden.
                                     there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))))

; ゲームのマップ上でプレーヤーが場所間を移動可能な通り道
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

; ゲーム世界に存在するオブジェクトのリスト
(defparameter *objects*  '(whiskey bucket frog chain))

; オブジェクトの場所
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; プレーヤーの現在地
(defparameter *location* 'living-room)

;; cleanな関数群
(defun describe-location (location nodes)
  "情景を描写する"
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  "通り道を描写する"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  "複数の通り道を一度に描写する"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  "与えられた場所から見えるオブジェクトのリストを返す"
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  "見えるオブジェクトを描写する"
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; dirtyな関数群
(defun look ()
  "全てを描写する"
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  "ゲーム世界を動き回る"
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

(defun pickup (object)
  "オブジェクトを手に取る"
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  "持っているものを調べる"
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


(defun game-repl ()
  "ゲーム専用REPL"
  (loop (print (eval (read)))))
