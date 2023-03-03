(in-package :cllox.scanner)
   

;;; keywords   
   
(defparameter *keywords*
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key value) in
          '(("and" :and)
            ("class" :class)
            ("else" :else)
            ("false" :false)
            ("for" :for)
            ("fun" :fun)
            ("if" :if)
            ("nil" :nil)
            ("or" :or)
            ("print" :print)
            ("return" :return)
            ("super" :super)
            ("this" :this)
            ("true" :true)
            ("var" :var)
            ("while" :while))
          do (setf (gethash key ht) value))
    ht))


;;; scanner

(defmacro add-token (type lexeme &optional literal)
  "literal will be nil if not provided."
  `(setf tokens
         (cons (make-instance 'cllox.token:token
                              :type ,type
                              :lexeme ,lexeme
                              :literal ,literal
                              :line line)
               tokens)))

;; is this class unnecessary? it doesnt have inheritence
(defclass scanner ()
  ((source :initarg :source :accessor scanner-source
           :documentation "raw src code")
   (tokens :initarg :tokens :initform '() :accessor scanner-tokens
           :documentation "list of tokens")
   (start :initarg :start :initform 0 :accessor scanner-start)
   (current :initarg :current :initform 0 :accessor scanner-current)
   (line :initarg :line :initform 1 :accessor scanner-line))
  (:documentation "a handcraft scanner"))

(defun scan-tokens (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((at-endp scanner))
      (setf start current)
      (scan-token scanner))
    ;; add eof to the end
    (add-token :eof "")
    (setf tokens (nreverse tokens))))

(defmacro 1char-token-case (keyform &body lst)
  `(case ,keyform
     ,@(loop for (a b) in lst collect
             `(,a (advance scanner)
                  (add-token ,b (subseq source start current))
                  ;; maybe this return is not needed
                  (return-from scan-token)))))

(defmacro 2char-token-case (keyform &body lst)
  "string-case need extra paren around keyform"
  `(string-case:string-case (,keyform :default nil)
     ,@(loop for (a b) in lst collect
             `(,a (advance scanner)
                  (advance scanner)
                  (add-token ,b (subseq source start current))
                  (return-from scan-token)))))

(defmacro space-token-case (keyform lst)
  `(case ,keyform
     ,@(loop for a in lst collect
             `(,a (advance scanner)
                  (return-from scan-token)))))

(defun scan-token (scanner)
  (with-slots (source tokens start current line)
      scanner

    (when (< (1+ current) (length source))
      (let ((str (subseq source current (+ 2 current))))
        (2char-token-case str
          ("!=" :bang-equal)
          ("==" :equal-equal)
          ("<=" :less-equal)
          (">=" :greater-equal))
        (when (string= str "//")        ; comment
          (do ()
              ;; newline will be handled later
              ((eql (peek scanner) #\newline))
            (advance scanner)))))
    
    (space-token-case (peek scanner)
                      (#\space
                       #\return
                       #\tab
                       #\newline))
    
    ;; != need be tested before ! 
    (1char-token-case (peek scanner)
      (#\( :left-paren)
      (#\) :right-paren)
      (#\{ :left-brace)
      (#\} :right-brace)
      (#\, :comma)
      (#\. :dot)
      (#\- :minus)
      (#\+ :plus)
      (#\; :semicolon)
      (#\* :star)
      (#\! :bang)
      (#\= :equal)
      (#\< :less)
      (#\> :greater)
      (#\/ :slash))

    ;; string
    (when (eql #\" (peek scanner))
      (scan-string scanner)
      (return-from scan-token))
    
    (when (digit-char-p (peek scanner))
      (scan-number scanner)
      (return-from scan-token))

    (when (alpha-char-p (peek scanner))
      (scan-identifier scanner)
      (return-from scan-token))
    
    (error 'cllox:run-error :reason "Unexpected character"
                            :line line)))

(defun scan-string (scanner)
  (with-slots (source tokens start current line)
      scanner
    (advance scanner)
    (when (eql (peek scanner) #\")
      (advance scanner)
      (add-token :string
                 (subseq source start current)
                 (subseq source (1+ start) (1- current)))
      (return-from scan-string))
    
    (when (eql (peek scanner) #\newline)
      (setf line (1+ line)))

    (when (at-endp scanner)
      (error 'cllox:run-error :reason "Unterminated string"
                              :line line))
    (scan-string scanner)))

(defun scan-number (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((or (null (peek scanner))
             (null (or (digit-char-p (peek scanner))
                       (eql #\. (peek scanner))))))
      (setf current (1+ current)))
    (add-token :number
               (subseq source start current)
               (parse-number:parse-number (subseq source start current)))))

(defun scan-identifier (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((or (null (peek scanner))
             (null (alpha-char-p (peek scanner)))))
      (advance scanner))
    
    (let ((type (gethash (subseq source start current) *keywords*)))
      (when (eql type nil)
        (setf type :indentifier))
      (add-token type (subseq source start current)))))

(defun at-endp (scanner)
  (with-slots (source current)
      scanner
    (>= current (length source))))

(defun peek (scanner)
  "look into the char at current"
  (with-slots (source current)
      scanner
    (unless (at-endp scanner)
      (elt source current))))

(defun advance (scanner)
  (with-slots (current)
      scanner
    (setf current (1+ current))))

