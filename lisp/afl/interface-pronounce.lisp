
  ;;; Method: LOCAL-SET-STATE                                  Author: raman
  ;;; Created: Thu Mar 25 09:19:37 1993

(defmethod local-set-state ((mode symbol))
  "Set pronunciation mode. "
  (set-pronunciation-mode mode))

(defmethod global-set-state((mode symbol ))
  "Set global pronunciation mode. "
  (setf *global-pronunciation-mode* mode ))
