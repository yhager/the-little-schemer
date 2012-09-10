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

(defun times (n m)
  (cond
   ((zerop m) 0)
   (t (plus n (times n (sub1 m))))))

