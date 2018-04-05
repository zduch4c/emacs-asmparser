(defun instruction-mov (operands)
  "Moves from memory to memory."
  (setf (nth (car operands) memory) (nth (cadr operands) memory)))

(defun instruction-movi (operands)
  "Move immediate value to memory."
  (setf (nth (car operands) memory) (cadr operands)))

(defun instruction-add (operands)
  "Adds memory to memory, stores in memory."
  (setf (nth (car operands) memory) (+ (nth (cadr operands) memory) (nth (car (cdr operands)) memory))))

(defun instruction-addi (operands)
  "Adds memory to immediate value, stores in memory."
  (setf (nth (car operands) memory) (+ (nth (cadr operands) memory) (car (cdr operands)))))

(defun instruction-mul (operands)
  "Multiplies memory with memory, stores in memory."
  (setf (nth (car operands) memory) (* (nth (cadr operands) memory) (nth (car (cdr operands)) memory))))

(defun instruction-muli (operands)
  "Multiples memory with immediate value, stores in memory."
  (setf (nth (car operands) memory) (* (nth (cadr operands) memory) (nth (car (cdr operands))))))

(defun instruction-div (operands)
  "Divides memory with memory, stores in memory."
  (setf (nth (car operands) memory) (/ (nth (cadr operands) memory) (nth (car (cdr operands) memory)))))

(defun instruction-divi (operands)
  "Divides memory with immediate value, stores in memory."
  (setf (nth (car operands) memory) (/ (nth (cadr operands) memory) (nth (car (cdr operands))))))

(defun instruction-mod (operands)
  "Stores remainder of two values in memory."
  (setf (nth (car operands) memory) (% (nth (cadr operands) memory) (nth (car (cdr operands) memory)))))

(defun instruction-modi (operands)
  "Stores remainder of two values in memory."
  (setf (nth (car operands) memory) (% (nth (cadr operands) memory) (car (cdr operands)))))

(defun instruction-beqz (operands)
  "Branches if equal to zero."
  (when (= (nth (car operands) memory) 0)
    (setf instruction-pointer (1- (cadr operands)))))

(defun instruction-bneqz (operands)
  "Branches if not equal to zero."
  (when (not (= (nth (car operands) memory) 0))
    (setf instruction-pointer (1- (cadr operands)))))

(defun instruction-display (operands)
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
    (and (equal (car (car tokens)) (car expected-types)) (matches? (cdr tokens) (cdr expected-types)))))

(defun parse-line (line)
  (let* ((parts (split-string line " " t))
	 (operation (car parts))
	 (operands (tokenize (cdr parts))))
    (cond ((string= operation "MOV")
	   (cond ((matches? operands (list 'cell 'cell))
		  (setf instruction-list (append instruction-list (list (list 'mov (get-values operands))))))
		 ((matches? operands (list 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'movi (get-values operands))))))))
	  ((string= operation "ADD")
	   (cond ((matches? operands (list 'cell 'cell 'cell))
		  (setf instruction-list (append instruction-list (list (list 'add (get-values operands))))))
		 ((matches? operands (list 'cell 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'addi (get-values operands))))))))
	  ((string= operation "MUL")
	   (cond ((matches? operands (list 'cell 'cell 'cell))
		  (setf instruction-list (append instruction-list (list (list 'mul (get-values operands))))))
		 ((matches? operands (list 'cell 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'muli (get-values operands))))))))
	  ((string= operation "DIV")
	   (cond ((matches? operands (list 'cell 'cell 'cell))
		  (setf instruction-list (append instruction-list (list (list 'div (get-values operands))))))
		 ((matches? operands (list 'cell 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'divi (get-values operands))))))))
	  ((string= operation "MOD")
	   (cond ((matches? operands (list 'cell 'cell 'cell))
		  (setf instruction-list (append instruction-list (list (list 'mod (get-values operands))))))
		 ((matches? operands (list 'cell 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'modi (get-values operands))))))))
	  ((string= operation "BEQZ")
	   (cond ((matches? operands (list 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'beqz (get-values operands))))))))
	  ((string= operation "BNEQZ")
	   (cond ((matches? operands (list 'cell 'int))
		  (setf instruction-list (append instruction-list (list (list 'bneqz (get-values operands))))))))
	  ((string= operation "DISPLAY")
	   (cond ((matches? operands (list 'string))
		  (setf instruction-list (append instruction-list (list (list 'display (get-values operands)))))))))))

(defun execute (instruction)
  (let ((instruction-symbol (car instruction))
	(instruction-values (car (cdr instruction))))
    (cond ((equal instruction-symbol 'mov) (instruction-mov instruction-values))
	  ((equal instruction-symbol 'movi) (instruction-movi instruction-values))
	  ((equal instruction-symbol 'add) (instruction-add instruction-values))
	  ((equal instruction-symbol 'addi) (instruction-addi instruction-values))
	  ((equal instruction-symbol 'mul) (instruction-mul instruction-values))
	  ((equal instruction-symbol 'muli) (instruction-muli instruction-values))
	  ((equal instruction-symbol 'div) (instruction-div instruction-values))
	  ((equal instruction-symbol 'divi) (instruction-divi instruction-values))
	  ((equal instruction-symbol 'mod) (instruction-mod instruction-values))
	  ((equal instruction-symbol 'modi) (instruction-modi instruction-values))
	  ((equal instruction-symbol 'beqz) (instruction-beqz instruction-values))
	  ((equal instruction-symbol 'bneqz) (instruction-bneqz instruction-values))
	  ((equal instruction-symbol 'display) (instruction-display instruction-values)))))

(defun run ()
  (dotimes (instruction-pointer (safe-length instruction-list))
    (message "IP:%d MEM:%s" instruction-pointer memory)
    (execute (nth instruction-pointer instruction-list))))

(let ((memory (make-list 16 0))
      (instruction-list '()))
  (parse-line "MOV 0 $10")
  (parse-line "MOV 1 0")
  (parse-line "ADD 2 0 1")
  (run)
  (message "MEM:%s" memory))
