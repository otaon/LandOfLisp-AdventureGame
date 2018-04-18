; ゲーム中の場所の描写
(defparameter *nodes* '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beatiful garden.
                                     there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))))


(defun describe-location (location nodes)
  "情景を描写する"
  (cadr (assoc location nodes)))
