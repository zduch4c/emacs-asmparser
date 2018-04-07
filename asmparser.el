(defun caddr (x)
 "caddr lol" (cadr (cdr x)))

(defun cadddr (x)
 "cadddr lol" (caddr (cdr x)))

(defun get-function (str-id operands func-list)
  "Finds an instruction function given its identifier string"
  (let* ( (instruction (car func-list))
          (instruction-str (car instruction))
          (instruction-function (cadr instruction))
          (instruction-pattern (caddr instruction))
          (instruction-param (cadddr instruction)))
    (cond
      ((equal func-list '()) nil)
      ((and (equal str-id instruction-str) (matches? operands instruction-pattern)) (list instruction-function instruction-param))
      (t (get-function str-id operands (cdr func-list))))))


(defun instruction-mov (param operands memory ip)
  "Moves from memory to memory."
  (setf (nth (car operands) memory) (nth (cadr operands) memory))
  (+ ip 1))

(defun instruction-movi (param operands memory ip)
  "Move immediate value to memory."
  (let ((op1 (cadr operands)))
  (setf (nth (car operands) memory) op1)
  (+ ip 1)))

(defun arithmetic-immediate (op operands memory ip)
 (let ((op1 (nth (cadr operands) memory))
       (op2 (caddr operands)))
   (setf (nth (car operands) memory) (funcall op op1 op2))
   (+ ip 1)))

(defun arithmetic-memory (op operands memory ip)
 (setf (nth (car operands) memory) (funcall op (nth (cadr operands) memory) (nth (cadr operands) memory)))
 (+ ip 1))

(defun instruction-beqz (param operands memory ip)
  "Branches if equal to zero."
  (cond ((= (nth (car operands) memory) 0) (cadr operands))
         (t (+ ip 1))))

(defun instruction-bneqz (param operands memory ip)
  "Branches if not equal to zero."
  (cond ((not (= (nth (car operands) memory) 0)) (cadr operands))
         (t (+ ip 1))))

(defun instruction-display (param operands memory ip)
  "Prints ASCII character."
  (message (car operands)))

(defun tokenize (operands)
  "Tokenizes operands."
  (let ((tokens '()))
    (dolist (token operands)
      (cond ((string-prefix-p "$" token)
             (setf tokens (append tokens (list (list 'int (string-to-number (mapconcat 'identity (cdr (split-string token "" t)) "")))))))
            ((string-prefix-p "!" token)
             (setf tokens (append tokens (list (list 'string (mapconcat 'identity (cdr (split-string token "" t)) ""))))))
            (t
              (setf tokens (append tokens (list (list 'cell (string-to-number token))))))))
    tokens))

(defun get-values (tokens)
  "Gets the values of tokens."
  (let ((values '()))
    (dolist (token tokens)
      (setf values (append values (cdr token))))
    values))

(defun matches? (tokens expected-types)
  "Returns true if tokens matches expected-types."
  (if (and (equal tokens nil) (equal expected-types nil)) t
    (and
      (equal (caar tokens) (car expected-types))
      (matches? (cdr tokens) (cdr expected-types))
      )))

(defun parse-lines (raw-list instruction-def-list)
  (cond ((equal raw-list '()) '())
        (t (let* (
                  (line (car raw-list))
                  (parts (split-string line " " t))
                  (instruction-id (car parts))
                  (operands (tokenize (cdr parts)))
                  (function-object (get-function instruction-id operands instruction-def-list))
                  (function-symbol (car function-object))
                  (function-param (cadr function-object))
                  )

             (cond ((equal function-symbol nil) (error (concat "Unrecognized instruction: " instruction-id)))
                   (t (cons (list function-symbol (get-values operands) function-param) (parse-lines (cdr raw-list) instruction-def-list)))
                   )))))

(defun execute (instruction-list memory instruction-pointer)
 (cond ((< instruction-pointer (length instruction-list))
  (let* ((instruction (nth instruction-pointer instruction-list))
        (instruction-function (car instruction))
        (instruction-values (cadr instruction))
        (instruction-param (caddr instruction)))
        (message "IP:%d (%s) MEM:%s" instruction-pointer (symbol-name instruction-function) memory)
        (execute instruction-list memory (funcall instruction-function instruction-param instruction-values memory instruction-pointer))))
    (t instruction-pointer)
  ))

(defun define-arithmetic-immediate (str func)
 (list str 'arithmetic-immediate (list 'cell 'cell 'int) func)
 )

(defun define-arithmetic-memory (str func)
 (list str 'arithmetic-memory (list 'cell 'cell 'cell) func)
 )

(let ((memory (make-list 16 0))
      (instruction-list '())
      (instruction-set (list
                         (list "MOV" 'instruction-mov (list 'cell 'cell) '())
                         (list "MOV" 'instruction-movi (list 'cell 'int) '())
                         (list "BNEQZ" 'instruction-bneqz (list 'cell 'int) '())
                         (list "BEQZ" 'instrution-beqz (list 'cell 'int) '())
                         (list "DISPLAY" 'instruction-display (list 'string) '())
                         (define-arithmetic-memory "ADD" '+)
                         (define-arithmetic-memory "SUB" '-)
                         (define-arithmetic-memory "DIV" '/)
                         (define-arithmetic-memory "MUL" '*)
                         (define-arithmetic-memory "MOD" '%)
                         (define-arithmetic-immediate "ADD" '+)
                         (define-arithmetic-immediate "SUB" '-)
                         (define-arithmetic-immediate "DIV" '/)
                         (define-arithmetic-immediate "MUL" '*)
                         (define-arithmetic-immediate "MOD" '%)
                       ))
      )
  (execute (parse-lines (list
      "MOV 0 $5"
      "SUB 0 0 $1"
 ;     "DISPLAY !hello"
      "BNEQZ 0 $1"
     )instruction-set) memory 0)
  (message "MEM:%s" memory))
