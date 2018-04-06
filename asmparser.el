(defun caddr (x)
 "caddr lol" (cadr (cdr x)))

(defun get-function (str-id operands func-list)
  "Finds an instruction function given its identifier string"
  (let* ( (instruction (car func-list))
          (instruction-str (car instruction))
          (instruction-function (cadr instruction))
          (instruction-pattern (caddr instruction)))
    (cond
      ((equal func-list '()) nil)
      ((and (equal str-id instruction-str) (matches? operands instruction-pattern)) instruction-function)
      (t (get-function str-id operands (cdr func-list))))))

(defun instruction-mov (operands memory ip)
  "Moves from memory to memory."
  (setf (nth (car operands) memory) (nth (cadr operands) memory))
  (+ ip 1))

(defun instruction-movi (operands memory ip)
  "Move immediate value to memory."
  (setf (nth (car operands) memory) (cadr operands))
  (+ ip 1))

(defun instruction-add (operands memory ip)
  "Adds memory to memory, stores in memory."
  (setf (nth (car operands) memory) (+ (nth (cadr operands) memory) (nth (car (cdr operands)) memory)))
  (+ ip 1))

(defun instruction-addi (operands memory ip)
  "Adds memory to immediate value, stores in memory."
  (setf (nth (car operands) memory) (+ (nth (cadr operands) memory) (car (cdr operands))))
  (+ ip 1))

(defun instruction-mul (operands memory ip)
  "Multiplies memory with memory, stores in memory."
  (setf (nth (car operands) memory) (* (nth (cadr operands) memory) (nth (car (cdr operands)) memory)))
  (+ ip 1))

(defun instruction-muli (operands memory ip)
  "Multiples memory with immediate value, stores in memory."
  (setf (nth (car operands) memory) (* (nth (cadr operands) memory) (nth (car (cdr operands)))))
  (+ ip 1))

(defun instruction-div (operands memory ip)
  "Divides memory with memory, stores in memory."
  (setf (nth (car operands) memory) (/ (nth (cadr operands) memory) (nth (car (cdr operands) memory))))
  (+ ip 1))

(defun instruction-divi (operands memory ip)
  "Divides memory with immediate value, stores in memory."
  (setf (nth (car operands) memory) (/ (nth (cadr operands) memory) (nth (car (cdr operands)))))
  (+ ip 1))

(defun instruction-mod (operands memory ip)
  "Stores remainder of two values in memory."
  (setf (nth (car operands) memory) (% (nth (cadr operands) memory) (nth (car (cdr operands) memory))))
  (+ ip 1))

(defun instruction-modi (operands memory ip)
  "Stores remainder of two values in memory."
  (setf (nth (car operands) memory) (% (nth (cadr operands) memory) (car (cdr operands))))
  (+ ip 1))

(defun instruction-beqz (operands memory ip)
  "Branches if equal to zero."
  (cond ((= (nth (car operands) memory) 0) (cadr operands))
         (t (+ ip 1))))

(defun instruction-bneqz (operands memory ip)
  "Branches if not equal to zero."
  (cond ((not (= (nth (car operands) memory) 0)) (cadr operands))
         (t (+ ip 1))))

(defun instruction-display (operands memory ip)
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
                  (function-symbol (get-function instruction-id operands instruction-def-list))
                  )

             (cond ((equal function-symbol nil) (error (concat "Unrecognized instruction: " instruction-id)))
                   (t (cons (list function-symbol (get-values operands)) (parse-lines (cdr raw-list) instruction-def-list)))
                   )))))

(defun execute (instruction-list memory instruction-pointer)
 (cond ((< instruction-pointer (length instruction-list))
  (let* ((instruction (nth instruction-pointer instruction-list))
        (instruction-function (car instruction))
        (instruction-values (car (cdr instruction))))
        (message "IP:%d MEM:%s" instruction-pointer memory)
        (execute instruction-list memory (funcall instruction-function instruction-values memory instruction-pointer))))
    (t instruction-pointer)
  ))

(let ((memory (make-list 16 0))
      (instruction-list '())
      (instruction-set (list
                         (list "MOV" 'instruction-mov (list 'cell 'cell))
                         (list "MOV" 'instruction-movi (list 'cell 'int))
                         (list "ADD" 'instruction-add (list 'cell 'cell 'cell))
                         (list "ADD" 'instruction-addi (list 'cell 'cell 'int))))
      )
  (execute (parse-lines (list
      "MOV 0 $10"
      "MOV 1 0"
      "ADD 2 0 1"
     )instruction-set) memory 0)
  (message "MEM:%s" memory))
