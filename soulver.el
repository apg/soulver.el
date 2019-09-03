(defun soulver--to-unit (x unit)
  "Scales x to be represented in unit."
  (if (and (soulver-unitp x)
           (eq (cadr x) (cadr unit)))
    (let ((units (assq (cadr unit) soulver-units)))
      (if (not units)
          (error "unknown unit")
        (let* ((xn (soulver--normalize-unit x))
               (div (assq (caddr unit) units)))
          (if (not div)
              (error "unknown target unit")
            (list (/ (float (car xn)) (cadr div))
                  (cadr unit)
                  (caddr unit))))))
    (error "value has incompatible units")))

(defun soulver--make-binop (op)
  "TODO: binop func takes care of units. Must be same units unless scalar * unit"
  `(lambda (x y)
     (let ((compute (lambda (x y)
                      (let ((xn (soulver--normalize-unit x))
                            (yn (soulver--normalize-unit y)))
                        (list (,op (car xn) (car yn))
                              (cadr xn) (caddr xn))))))
         (cond
          ((and (numberp x) (numberp y)) (,op x y))
          ((and (numberp x) (soulver-unitp y))  ;; scalar add
           (funcall compute (cons x (cdr y)) y))
          ((and (numberp y) (soulver-unitp x))  ;; scalar add
           (funcall compute x (cons y (cdr x)))
           (list (,op (car x) y) (cadr x) (caddr x)))
          ((and (soulver-unitp x) (soulver-unitp y)
                (eq (cadr x) (cadr y)))
           (funcall compute x y))
     (t (error "incompatible arguments"))))))

(defconst soulver-operators
  `((+    10   left    ,(soulver--make-binop '+))
    (-    10   left    ,(soulver--make-binop '-))
    (*    20   left    ,(soulver--make-binop '*))
    (/    20   left    ,(soulver--make-binop '/))
    (to   1    left    soulver--to-unit)
    ))

(defconst soulver-token-regexps
  `((int "^[0-9]+")
    (number "^\\(-\\|+\\)?[0-9]+\\(\\.[0-9]+\\)?")
    (unit ,(concat "^" (regexp-opt '("ms" "s" "m" "h" "d" "B" "KB" "MB" "GB" "TB" "PB"))))
    (binop "^\\+")
    (space "^[ \t]+")))

(defun soulver--tokenize (line)
  (let* ((output '())
         (tokens (split-string line " ")))
    (while tokens
      (let* ((current-token (car tokens))
             (num (string-to-number current-token))
             (sym (intern current-token)))
        (cond
         ((memq sym soulver-keywords)
          (if (soulver-binopp sym)
              (setq output (cons sym output))
            ;; otherwise, if it's a unit
            (progn
              (let ((family (soulver--unit-family soulver-units sym)))
                (if family
                    (if (and (> (length output) 0)
                             (numberp (car output)))
                        (setq output
                              (cons (list (car output)
                                          family
                                          sym)
                                    (cdr output)))
                      (setq output
                            (cons (list 1 family sym) (cdr output))))
                  (error "unrecognized unit"))))))
         ;; if non-zero, it definitely worked.
         ((not (= 0 num)) (setq output (cons num output)))
         ((and (= 0 num) ;; is it legitimate zero?
               (string-equal "0" current-token))
          (setq output (cons num output)))
         (t (error "unknown token read."))))
      (setq tokens (cdr tokens)))
    (reverse output)))

;;; Perhaps unneeded?
(defun soulver--interpret-token (line)
  (let* ((output '())
         (tokens (split-string line " ")))
    (while tokens
      (let ((num (string-to-number (car tokens)))
            (sym (intern (car tokens))))
        (cond
         ((not (= 0 num)))
         ;;;;;; TODO: figure out how to test if string-to-number works.
         ((not (and (= 0 num)
                    (string-equal "0" (car tokens))))
          (setq output (cons num output)))
         ((memq sym solve-keywords)
          (cond
           ((solve-binopp sym) (setq output (cons sym output)))
           (t (let ((family (solve--family-unit solve-units sym)))
                (if family
                    (if (and (> (length output) 0)
                             (numberp (car output)))
                        (cons (list (car output)
                                    family
                                    sym)
                              (cdr output))
                      (cons (list 1 family sym) (cdr output)))
                  (error "unrecognized unit"))))))
         (t (error "unknown token read."))))
      (setq tokens (cdr tokens)))
    output))


(defconst soulver-units
  '((duration (ms 1)
              (s  1000)
              (m  60000)
              (h  3600000)
              (d  86400000))
    (bytes (B  1)
           (KB 1024)
           (MB 1048576)
           (GB 1073741824)
           (TB 1099511627776)
           (PB 1125899906842624))))

(defconst soulver-keywords
  '(+ - * / ms s m h d B KB MB GB TB PB))

(defun soulver--unit-family (units u)
  "Returns the family name of the unit u, or nil"
  (cond
   ((not units) nil)
   (t (let ((found (assq u (cdar units))))
         (if (not found)
             (soulver--unit-family (cdr units) u)
           (caar units))))))

(defun soulver--normalize-unit (u)
  "Turns u into the smallest unit in the unit's family."
  (if (= (length u) 2)
      (cons 0 u) ;; in this case, it's meant only as a unit indicator.
      (let ((units (assq (cadr u) soulver-units)))
        (if (not units)
            (error "unknown unit")
          (progn
            (let ((mult (assq (caddr u) (cdr units))))
              (list (* (car u) (cadr mult))
                    (cadr u)
                    (caadr units))))))))

(defun soulver-unitp (x)
  "Tests if x is in unit notation: (number symbol symbol)"
  (and
   (listp x)
   (= (length x) 3)
   (numberp (car x))
   (symbolp (cadr x))
   (symbolp (caddr x))))

(defun soulver-binopp (x)
  "Returns t if x is a known binary operator."
  (assq x soulver-operators))

(defun soulver--binop-prec (op)
  "Returns the precedence of the operator"
  (let ((entry (assq op soulver-operators)))
    (when entry
      (cadr entry))))

(defun soulver--binop-assoc (op)
    "Returns the associtivity of the operator"
  (let ((entry (assq op soulver-operators)))
    (when entry
      (caddr entry))))

(defun soulver--binop-function (op)
  "Returns the code to handle the operator"
  (let ((entry (assq op soulver-operators)))
    (when entry
      (cadddr entry))))

(defun soulver--rpn-eval (stack)
  "Evaluates stack as an RPN expression. Compatible with unit notation."
  (let ((data '()))
    (while stack
      (let ((top (car stack)))
        (cond
         ((numberp top) (setq data (cons top data)))
         ((soulver-unitp top) (setq data (cons top data)))
         ((soulver-binopp top)
          (progn
           (if (< (length data) 2)
               (error "binop requires at least 2 arguments")
             (progn
               (setq data (cons (apply (soulver--binop-function top) (list (car data) (cadr data)))
                                (cddr data)))))))
         (t (error "unknown item in stack") ))
        (setq stack (cdr stack))))
    (car data)))

(defun soulver (tokens)
  "Computes the result of an infix expression, possibly with units."
  (let ((output '())
        (operators '()))
    (while tokens
      (let ((curr (car tokens)))
        (cond
         ((numberp curr) (setq output (cons curr output)))
         ((and (listp curr)
               (= (length curr) 2)
               (symbolp (car curr))
               (symbolp (cadr curr)))
          ;;; turn this into '(mag family unit), but don't normalize.
          (setq output (cons (cons 0 curr) output)))
         ((soulver-unitp curr) (setq output (cons curr output)))
         ((soulver-binopp curr)
          (progn
            (while (and operators
                        (or (> (soulver--binop-prec (car operators))
                               (soulver--binop-prec curr))
                            (and (eq 'left (soulver--binop-assoc (car operators)))
                                 (= (soulver--binop-prec (car operators))
                                    (soulver--binop-prec curr)))))
              (setq output (cons (car operators) output))
              (setq operators (cdr operators)))
            (setq operators (cons curr operators))))
         (t (progn
              (print curr)
              (error "unknown token")))))
      (setq tokens (cdr tokens)))
    (soulver--rpn-eval (append (reverse output) (reverse operators)))))
