(defun atom? (arg)
  (not (listp arg)))

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l))
    (lat? (cdr l)))
   (t nil)))

(defun member? (a lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a)
          (member? a (cdr lat))))))

(defun rember (a lat)
  (cond
   ((null lat) ())
   ((eq a (car lat)) (cdr lat))
   (t (cons (car lat)
            (rember a (cdr lat))))))

(defun firsts (l)
  (cond
   ((null l) ())
   (t (cons (car (car l))
            (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
   ((null lat) ())
   ((eq old (car lat)) (cons old
                             (cons new
                                   (cdr lat))))
   (t (cons (car lat) (insertR new
                               old (cdr lat))))))

(defun insertL (new old lat)
  (cond
   ((null lat) ())
   ((eq old (car lat)) (cons new lat))
   (t (cons (car lat)
            (insertL new old (cdr lat))))))

(defun subst (new old lat)
  (cond
   ((null lat) ())
   ((eq old (car lat)) (cons new (cdr lat)))
   (t (cons (car lat)
            (subst new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond
   ((null lat) ())
   ((or (eq o1 (car lat))
        (eq o2 (car lat))) (cons new
                                 (cdr lat)))
   (t (cons (car lat)
            (subst2 new o1 o2 (cdr lat))))))

(defun multirember (a lat)
  (cond
   ((null lat) ())
   ((eq a (car lat)) (multirember a (cdr lat)))
   (t (cons (car lat)
            (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
   ((null lat) ())
   ((eq old (car lat)) (cons old
                             (cons new
                                   (multiinsertR new old (cdr lat)))))
   (t (cons (car lat)
            (multiinsertR new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond
   ((null lat) ())
   ((eq old (car lat)) (cons new 
                             (multisubst new old (cdr lat))))
   (t (cons (car lat)
            (multisubst new old (cdr lat))))))

(defun add1 (n)
  (+ 1 n ))

(defun sub1 (n)
  (- n 1))

(defun plus (n m)
  (cond
   ((zerop m) n)
   (t (add1 (plus n (sub1 m))))))

(defun minus (n m)
  (cond
   ((zerop m) n)
   (t (sub1 (minus n (sub1 m))))))

(defun addtup (tup)
  (cond
   ((null tup) 0)
   (t (plus (car tup) (addtup (cdr tup))))))

(defun × (n m)
  (cond
   ((zerop m) 0)
   (t (plus n (× n (sub1 m))))))

(defun tup+ (tup1 tup2)
  (cond
   ((and (null tup1) (null tup2)) ())
   (t (cons (plus (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6))

(defun tup+ (tup1 tup2)
  (cond
   ((null tup1) tup2)
   ((null tup2) tup1)
   (t (cons (plus (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(3 7) '(4 6 8 1))
(tup+ '(3 7 8 1) '(4 6))

(defun > (n m)
  (cond
   ((zerop n) nil)
   ((zerop m) t)
   (t (> (sub1 n) (sub1 m)))))

(> 12 133)
(> 120 11)
(> 3 3)

(defun < (n m)
  (cond
   ((zerop m) nil)
   ((zerop n) t)
   (t (< (sub1 n) (sub1 m)))))

(< 4 6)
(< 8 3)
(< 6 6)

(defun = (n m)
  (cond
   ((< n m) nil)
   ((> n m) nil)
   (t t)))

(= 3 4)
(= 4 3)
(= 4 4)

(defun ^ (n m)
  (cond
   ((zerop m) 1)
   (t (× n (^ n (sub1 m))))))

(^ 1 1)
(^ 2 3)
(^ 5 3)

(defun ÷ (n m)
  (cond
   ((< n m) 0)
   (t (add1 (÷ (minus n m) m)))))

(÷ 15 4)

(defun length (lat)
  (cond
   ((null lat) 0)
   (t (add1 (length (cdr lat))))))

(length '(hotdogs with mustard sauerkraut and pickles))
(length '(ham and cheese on rye))

(defun pick (n lat)
  (cond
   ((zerop (sub1 n)) (car lat))
   (t (pick (sub1 n) (cdr lat)))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(defun rempick (n lat)
  (cond
   ((zerop (sub1 n)) (cdr lat))
   (t (cons (car lat)
            (rempick (sub1 n) (cdr lat))))))

(rempick 3 '(hotdogs with hot mustard))

(defun no-nums (lat)
  (cond
   ((null lat) ())
   (t (cond
       ((numberp (car lat)) (no-nums (cdr lat)))
       (t (cons (car lat)
                (no-nums (cdr lat))))))))

(no-nums '(5 pears 6 prunes 9 dates))

(defun all-nums (lat)
  (cond
   ((null lat) ())
   ((numberp (car lat))
    (cons (car lat)
          (all-nums (cdr lat))))
   (t (all-nums (cdr lat)))))

(all-nums '(5 pears 6 prunes 9 dates))

(defun eqan? (a1 a2)
  (cond
   ((and (numberp a1) (numberp a2))
    (= a1 a2))
   ((or (numberp a1) (numberp a2))
    nil)
   (t (eq a1 a2))))

(eqan? 'a 6)

(defun occur (a lat)
  (cond
   ((null lat) 0)
   ((eqan? a (car lat))
    (add1 (occur a (cdr lat))))
   (t (occur a (cdr lat)))))

(occur 'knives '(6 pears 6 knives 6 6 spoons knives))

(defun one? (n)
  (= n 1))

(one? 0)

(defun rempick (n lat)
  (cond
   ((one? n) (cdr lat))
   (t (cons (car lat)
            (rempick (sub1 n) (cdr lat))))))
(rempick 3 '(lemon meringue salty pie))
