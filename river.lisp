;Student Name: Renij Shrestha
;Student Number:20129428

(defun make-state (w x y z)
  (list w x y z))

(defparameter *start-state* 
  (make-state '1 '2 '3 'e))

(defparameter *goal-state*
  (make-state '0 '0 '0 'w))

; *OPERATORS* is a list of the names of operators
(defparameter *operators*
  '(farmer-takes-self farmer-takes-1wolf farmer-takes-2cat
    farmer-takes-2duck farmer-takes-1cat farmer-takes-1duck
    farmer-takes-1wolf-1cat farmer-takes-1wolf-1duck farmer-takes-1cat-1duck))

(defun solution-state? (state)
  (equalp state *goal-state*))

; State accessor functions
(defun wolf-side (state)
   (nth 0 state))

(defun cat-side (state)
   (nth 1 state))

(defun duck-side (state)
   (nth 2 state))

(defun farmer-side (state)
   (nth 3 state))

; OPPOSITE takes a side (e or w) as input, and returns the opposite
(defun opposite (side)
  (cond ((equalp side 'e) 'w)
        ((equalp side 'w) 'e)))

;wolf-opposite tells the number of wolf on the opposite side(west), similar for other.
(defun wolf-opposite (state)
  (- 1 (wolf-side state)))

(defun cat-opposite (state)
  (- 2 (cat-side state)))

(defun duck-opposite (state)
  (- 3 (duck-side state)))

; SAFE takes a state as input. Returns nil if the state is not safe otherwise it returns the state
(defun safe (state)
  (cond ((and(or (and (<= (cat-side state) (wolf-side state))(not(= (cat-side state) 0)))
                 (and (<= (duck-side state) (cat-side state)) (not (= (duck-side state) 0)))) (equalp (farmer-side state) 'w)) nil)            
        ((and (or (and(<= (cat-opposite state) (wolf-opposite state)) (not(= (cat-opposite state) 0))) 
                  (and (<= (duck-opposite state) (cat-opposite state)) (not(= (duck-opposite state) 0)))) (equalp (farmer-side state) 'e)) nil)
        (t state)))
 

(defun farmer-takes-self (state)
  (safe
    (make-state
      (wolf-side state)
      (cat-side state)
      (duck-side state)
      (opposite (farmer-side state)))))

(defun farmer-takes-1wolf (state)
  (cond ((and (equalp (farmer-side state) 'e) (> (wolf-side state) 0))
         (safe 
           (make-state
             (- (wolf-side state) 1)
             (cat-side state)
             (duck-side state)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (> (wolf-opposite state) 0))
         (safe 
           (make-state
             (+ (wolf-side state) 1)
             (cat-side state)
             (duck-side state)
             (opposite (farmer-side state)))))
        (t nil)))

(defun farmer-takes-2cat (state)
  (cond ((and (equalp (farmer-side state) 'e) (> (cat-side state) 1))
         (safe 
           (make-state
             (wolf-side state)
             (- (cat-side state) 2)
             (duck-side state)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (> (cat-opposite state) 1))
         (safe
           (make-state 
             (wolf-side state)
             (+ (cat-side state) 2)
             (duck-side state)
             (opposite (farmer-side state)))))
        (t nil)))

(defun farmer-takes-2duck (state)
  (cond ((and (equalp (farmer-side state) 'e) (> (duck-side state) 1))
         (safe
           (make-state
             (wolf-side state)
             (cat-side state)
             (- (duck-side state) 2)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (> (duck-opposite state) 1))
         (safe
           (make-state
             (wolf-side state)
             (cat-side state)
             (+ (duck-side state) 2)
             (opposite (farmer-side state)))))
        (t nil)))
(defun farmer-takes-1cat (state)
  (cond ((and (equalp (farmer-side state) 'e) (> (cat-side state) 0))
         (safe
           (make-state
             (wolf-side state)
             (- (cat-side state) 1)
             (duck-side state)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (> (cat-opposite state) 0))
         (safe
           (make-state
             (wolf-side state)
             (+ (cat-side state) 1)
             (duck-side state)
             (opposite (farmer-side state)))))
         (t nil)))

(defun farmer-takes-1duck (state)
  (cond ((and (equalp (farmer-side state) 'e) (> (duck-side state) 0))
         (safe
           (make-state
             (wolf-side state)
             (cat-side state)
             (- (duck-side state) 1)
             (opposite (farmer-side state)))))
         ((and (equalp (farmer-side state) 'w) (> (duck-opposite state) 0))
          (safe
            (make-state
               (wolf-side state)
               (cat-side state)
               (+ (duck-side state) 1)
               (opposite (farmer-side state)))))
         (t nil)))

(defun farmer-takes-1wolf-1cat (state)
  (cond ((and (equalp (farmer-side state) 'e) (and (> (wolf-side state) 0) (> (cat-side state))))
         (safe
           (make-state
             (- (wolf-side state) 1)
             (- (cat-side state) 1)
             (duck-side state)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (and (> (wolf-opposite state) 0) (> (cat-opposite state))))
         (safe
           (make-state
             (+ (wolf-side state) 1)
             (+ (cat-side state) 1)
             (duck-side state)
             (opposite (farmer-side state)))))
         (t nil)))

(defun farmer-takes-1wolf-1duck (state)
  (cond ((and (equalp (farmer-side state) 'e) (and (> (wolf-side state) 0) (> (duck-side state))))
         (safe
           (make-state
              (- (wolf-side state) 1)
              (cat-side state) 
              (- (duck-side state) 1)
              (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (and (> (wolf-opposite state) 0) (> (duck-opposite state))))
         (safe
           (make-state
             (+ (wolf-side state) 1)
             (cat-side state)
             (+ (duck-side state) 1)
             (opposite (farmer-side state)))))
        (t nil)))
(defun farmer-takes-1cat-1duck (state)
  (cond ((and (equalp (farmer-side state) 'e) (and (> (cat-side state) 0) (> (duck-side state))))
         (safe
           (make-state
             (wolf-side state)
             (- (cat-side state) 1)
             (- (duck-side state) 1)
             (opposite (farmer-side state)))))
        ((and (equalp (farmer-side state) 'w) (and (> (cat-opposite state) 0) (> (duck-opposite state))))
         (safe
           (make-state
             (wolf-side state)
             (+ (cat-side state) 1)
             (+ (duck-side state) 1)
             (opposite (farmer-side state)))))
        (t nil)))




















