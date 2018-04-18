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

(defun describe-location (location nodes)
  "情景を描写する"
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  "通り道を描写する"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

