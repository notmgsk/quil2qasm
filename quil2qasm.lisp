;;;; quil2qasm.lisp

(in-package #:quil2qasm)

(defparameter *qasm* nil)
(setf *qasm* "
// quantum teleportation example
OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[3];
creg c0[1];
creg c1[1];
creg c2[1];
// optional post-rotation for state tomography
gate post q {
}
u3(0.3,0.2,0.1) q[0];
h q[1];
cx q[1],q[2];
barrier q;
cx q[0],q[1];
h q[0];
measure q[0] -> c0[0];
measure q[1] -> c1[0];
if(c0==1) z q[2];
if(c1==1) x q[2];
post q[2];
measure q[2] -> c2[0];
measure a[1] -> c3[1];
")

(defparameter *qasm1* nil)
(setf *qasm1* "
// quantum teleportation example
OPENQASM 2.0;
include \"test.quil\";
qreg p[3];
qreg q[3];
creg c0[1];
measure q[1] -> c0[0];
")

(deftype qasm-keywords ()
  '(member
    :OPENQASM :QREG :CREG :GATE :BARRIER
    :MEASURE :RESET :OPAQUE :INCLUDE
    :IF :CX :U))

(deftype token-type ()
  '(or
    qasm-keywords
    (member :SEMI-COLON :LEFT-PAREN :RIGHT-PAREN :COMMA
            :ARROW :LEFT-SQUARE-BRACKET :RIGHT-SQUARE-BRACKET :LEFT-CURLY-BRACKET
            :RIGHT-CURLY-BRACKET :LEFT-ANGLE-BRACKET :RIGHT-ANGLE-BRACKET :PLUS
            :MINUS :TIMES :DIVIDE :EXPT :EQUALSEQUALS :NNINTEGER :REAL :ID
            :STRING :KEYWORD :OPENQASM :QREG :CREG :GATE)))

(defparameter *valid-functions*
    '(("sin"  cl:sin)
      ("cos"  cl:cos)
      ("sqrt" cl:sqrt)
      ("exp"  cl:exp)
      ("ln"   cl:log)
      ("tan"  cl:tan)))

(defvar *line-start-position*)
(defvar *line-number*)

(defstruct (token (:constructor tok (type &optional payload)))
  "A lexical token."
  (line nil :type (or null (integer 1)))
  (type nil :type token-type)
  (payload nil))

(alexa:define-string-lexer line-lexer
  "A lexical analyzer for lines of Quil."
  ((:int    "\\d+")
   (:real  "(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?")
   (:ident  "[a-z](?:[A-Za-z0-9_\\-]*[A-Za-z0-9_])?")
   (:string "\\\"(?:[^\\\"]|\\\\\\\")*\\\"")
   (:newline "(?:\\r\\n?|\\n)"))
  ((eager "//[^\\n\\r]*")
   nil)
  ((eager "{{NEWLINE}}")
   ;; We return a keyword because they get split out and
   ;; removed. They're not actually logical tokens of the Quil
   ;; language.
   (incf *line-number*)
   (setf *line-start-position* $>)
   (return ':NEWLINE))
  ((eager "\\;")
   (return (tok :SEMI-COLON)))
  ((eager "\\(")
   (return (tok :LEFT-PAREN)))
  ((eager "\\)")
   (return (tok :RIGHT-PAREN)))
  ((eager "\\[")
   (return (tok :LEFT-SQUARE-BRACKET)))
  ((eager "\\]")
   (return (tok :RIGHT-SQUARE-BRACKET)))
  ((eager "\\{")
   (return (tok :LEFT-CURLY-BRACKET)))
  ((eager "\\}")
   (return (tok :RIGHT-CURLY-BRACKET)))
  ((eager "\\<")
   (return (tok :LEFT-ANGLE-BRACKET)))
  ((eager "\\>")
   (return (tok :RIGHT-ANGLE-BRACKET)))
  ((eager "\\,")
   (return (tok :COMMA)))
  ((eager "->")
   (return (tok :ARROW)))
  ("OPENQASM|qreg|creg|barrier|measure|reset|opaque|include"
   (return (tok (intern (string-upcase $@) :keyword))))
  ("\\+" (return (tok :PLUS)))
  ("\\-" (return (tok :MINUS)))
  ("\\*" (return (tok :TIMES)))
  ("\\/" (return (tok :DIVIDE)))
  ("\\^" (return (tok :EXPT)))
  ("=="  (return (tok :EQUALSEQUALS)))
  ((eager "{{STRING}}")
   (return (tok :STRING (read-from-string $@))))
  ((eager "{{INT}}")
   (return (tok :NNINTEGER (parse-integer $@))))
  ((eager "{{REAL}}")
   (return (tok :REAL (parse-float:parse-float $@))))
  ("{{IDENT}}"
   (return (tok :ID $@)))
  ;; Non-newline whitespace. In newer Perl, you can just use \h, for
  ;; "horizontal space", but this isn't supported in CL-PPCRE.
  ("[^\\S\\n\\r]+"
   nil))

;;; SPLIT-SEQUENCE was too slow for 10k+ tokens.
(defun nsplit (wedge list)
  "Split LIST into non-empty sublists that were separated by WEDGE."
  (declare (type symbol wedge)
           (type list list)
           (optimize speed (space 0)))
  (let* ((pieces      (cons nil nil))
         (pieces-last pieces))
    (declare (type cons pieces pieces-last))
    (labels ((doit (start last end)
               (declare (type list start last end))
               (cond
                 ;; Done
                 ((null end)
                  (unless (eq start end)
                    (rplacd pieces-last (cons start nil))
                    (setf pieces-last (cdr pieces-last)))
                  pieces)
                 ;; Found a wedge.
                 ((eql wedge (car end))
                  (cond
                    ((eq start end)
                     (let ((next (cdr start)))
                       (doit next next next)))
                    (t
                     (rplacd pieces-last (cons start nil))
                     (setf pieces-last (cdr pieces-last))
                     (setf start (cdr end))
                     (rplacd last nil)
                     (doit start start start))))
                 ;; Keep on truckin'.
                 (t
                  (doit start end (cdr end))))))
      (cdr (doit list list list)))))

(defun tokenize-line (lexer string)
  "Given a lexer (as defined by DEFINE-STRING-LEXER) and a string, return a list of tokens represented by that string."
  (let ((f (funcall lexer string))
        (*line-number* 1)
        (*line-start-position* 0))
    (loop :for tok := (funcall f)
          :until (null tok)
          :collect tok)))

(defun tokenize (string)
  (let* ((lines (nsplit ':NEWLINE (tokenize-line 'line-lexer string))))
    ;; (map-into lines #'ensure-indentation lines)
    ;; (process-indentation lines)
    lines))

;; parse

(define-condition q2q-parse-error (alexandria:simple-parse-error)
  ()
  (:documentation "Representation of an error parsing QASM."))

(defun q2q-parse-error (format-control &rest format-args)
  "Signal a Q2Q-PARSE-ERROR with a descriptive error message described by FORMAT-CONTROL and FORMAT-ARGS."
  (error 'q2q-parse-error :format-control format-control
         :format-arguments format-args))

(declaim (inline nop))
(defun nop (tok-lines)
  (values (make-instance 'quil::no-operation)
          (rest tok-lines)))

(defun parse-program-lines (tok-lines)
  "Parse the next AST object from the list of token lists. Returns two values:

1. The next AST object.
2. A list of lines that remain unparsed.
"
  (let* ((line (first tok-lines))
         (tok (first line))
         (tok-type (token-type tok)))
    (case tok-type
      ;; The OPENQASM line is uninteresting. Return the remaining lines.
      ((:OPENQASM)
       (nop tok-lines))

      ((:INCLUDE)
       (parse-include tok-lines))

      ((:QREG)
       (parse-qreg tok-lines))

      ;; creg name[size]; defines a classical memory register. In Quil
      ;; it maps to a declared memory register of BITs.
      ((:CREG)
       (parse-creg tok-lines))

      ;; barrier
      ;; commuting blocks??
      ((:BARRIER)
       (nop tok-lines))

      ;; Quil's DEFGATEs are specified as a matrix, whereas QASM's are
      ;; specified as a list of built-in gates. i.e. more like Quil's
      ;; DEFCIRCUIT? In any case this is not going to be a simple
      ;; translation.
      ((:GATE)
       (nop tok-lines))

      ((:MEASURE)
       (parse-measure tok-lines))

      ((:RESET)
       (nop tok-lines))

      ((:OPAQUE)
       (nop tok-lines))

      ((:ID)
       (parse-application tok-lines))

      (otherwise
       (q2q-parse-error "Got an unexpected token of type ~S ~
                         when trying to parse a program." tok-type)))))

(defmacro destructuring-token-bind (token-idents token-line &body body)
  ;; TODO Throw some lib-specific condition if destructuring-bind
  ;; fails.
  ;;
  ;; TODO Allow for "fall-through" cases. i.e. if the first
  ;; TOKEN-IDENTS doesn't match, try the next one, etc. I'm thinking
  ;; this might be helpful when matching `measure a -> b` vs `measure
  ;; a[0] -> b[0]`.
  ;;
  ;; TODO I wrote this while very tired. Untested.
  (let ((ts (mapcar #'alexandria:ensure-list (remove '_ token-idents))))
    `(progn
       (unless (= ,(length token-idents) (length ,token-line))
         (q2q-parse-error "Expected ~d tokens but parsed ~d."
                          ,(length token-idents) (length ,token-line)))
       (loop :for (tok-name tok-type) :in ',ts
             :for tok :in ,token-line
             :when tok-type :do
               (assert (eql tok-type (token-type tok)) ()
                       "Expected a token of type ~S but got a token (~S) of type ~S."
                       tok-type tok (token-type tok)))
       ;; Some ugliness follows to remove ignored elements (those
       ;; whose token name is the underscore).
       (destructuring-bind ,(mapcar #'first (remove-if (lambda (tok) (eql '_ (car tok))) ts))
           (loop :for (tok-name tok-type) :in ',ts
                 :for tok :in ,token-line
                 :unless (eql tok-name '_)
                   :collect tok)
         ,@body))))

(defun parse-expression-list (tok-line)
  (when (null tok-line)
    (q2q-parse-error "Unexpectedly reached end of program, expecting expression list."))

  ;; (a, b, c) q1, q2, q3 ==> '((a b c) (q1, q2, q3))
  (destructuring-bind (params-line &rest rest)
      ;; ugly but whatever
      (list (rest (subseq tok-line 0 (position ':RIGHT-PAREN tok-line :from-end t :key 'token-type)))
            (subseq tok-line (1+ (position ':RIGHT-PAREN tok-line :from-end t :key 'token-type))))
    (let ((params (split-sequence:split-sequence ':COMMA params-line
                                                 :key 'token-type)))
      (list params rest))))

(defun parse-qubits-list (tok-line)
  (when (null tok-line)
    (q2q-parse-error "Unexpectedly reached end of program, expecting qubit list."))

  (let ((qubits (mapcar #'first (split-sequence:split-sequence ':COMMA tok-line :key 'token-type))))
    (unless (every (lambda (q) (eql ':ID (token-type q))) qubits)
      (q2q-parse-error "Expected some qubits"))
    qubits))

(defun parse-application (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting gate application."))

  (let* ((application-line (first tok-lines)))
    (unless (eql ':SEMI-COLON (token-type (first (last application-line))))
      (q2q-parse-error "Missing semi-colon"))

    (destructuring-bind (name &rest rest)
        (butlast application-line)
      (destructuring-bind (expression-list rest-of-line)
          (parse-expression-list rest)
        (let ((qubit-list (parse-qubits-list (first rest-of-line))))
          )))))

;; u1(alpha) qubit;

(defun parse-creg (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting creg."))

  (let ((creg-line (first tok-lines)))
    (destructuring-token-bind ((_ :CREG) (name :ID) (_ :LEFT-SQUARE-BRACKET)
                               (length :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET)
                               (_ :SEMI-COLON))
        creg-line
      (let ((name (token-payload name))
            (length (token-payload length)))
        (setf (gethash name *creg-names*) length)
        (values (quil::make-memory-descriptor :name name
                                              :type quil::quil-bit
                                              :length length)
                (rest tok-lines))))))

(defun parse-qreg (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting qreg."))

  (let ((qreg-line (first tok-lines)))
    (destructuring-token-bind ((_ :QREG) (name :ID) (_ :LEFT-SQUARE-BRACKET)
                               (length :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET)
                               (_ :SEMI-COLON))
        qreg-line
      (let ((name (token-payload name))
            (length (token-payload length)))
        ;; While a qreg doesn't directly map to a quil instruction, we
        ;; do need to keep track of named qubit registers so that we
        ;; can properly translate instructions that reference qubit
        ;; registers.
        ;;
        ;; e.g. the translation of "measure q[3] -> r[0];" might
        ;; simply be "MEASURE 3 r[0]". But what about "measure q[3] ->
        ;; r[0]; measure p[3] -> r[1];"? In OpenQASM the qubits "q[3]"
        ;; and "p[3]" are not the same.
        (setf (gethash name *qreg-names*) (list *qubit-count* length))
        (incf *qubit-count* length)
        (nop tok-lines)))))

(defun parse-include (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting include."))

  (let ((include-line (first tok-lines)))
    (destructuring-token-bind ((_ :INCLUDE)
                               (path-tok :STRING)
                               (_ :SEMI-COLON))
        include-line
      (values (make-instance 'quil::include :pathname (token-payload path-tok))
              (rest tok-lines)))))

(defun %qreg-to-qubit (qreg qreg-index)
  (destructuring-bind (offset size)
      (gethash qreg *qreg-names*)
    (assert (< qreg-index size) ()
            "The index ~s is out-of-bounds for qreg ~s"
            qreg-index qreg)
    (quil::qubit (+ offset qreg-index))))

(defun %creg-to-address (creg creg-index)
  (quil::mref creg creg-index))

(defun parse-measure (tok-lines)
  (when (null tok-lines)
    (q2q-parse-error "Unexpectedly reached end of program, expecting measure."))
  (let ((include-line (first tok-lines)))
    (destructuring-token-bind ((_ :MEASURE) (qreg :ID) (_ :LEFT-SQUARE-BRACKET)
                               (qreg-index :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET)
                               (_ :ARROW) (creg :ID) (_ :LEFT-SQUARE-BRACKET)
                               (creg-index :NNINTEGER) (_ :RIGHT-SQUARE-BRACKET)
                               (_ :SEMI-COLON))
        include-line
      (values (make-instance 'quil::measure :address (%creg-to-address (token-payload creg)
                                                                       (token-payload creg-index))
                                            :qubit (%qreg-to-qubit (token-payload qreg)
                                                                   (token-payload qreg-index)))
              (rest tok-lines)))))

(defparameter *creg-names* (make-hash-table :test #'equal)
  "Maps a creg name to its size.")

(defparameter *qreg-names* (make-hash-table :test #'equal)
  "Maps a qreg name to the pair (offset . size). A qreg defined with
  `qreg q[size];` maps to Quil qubits (offset, offset + 1, offset + 2,
  ..., offset + size - 1). This complication maintains unique qubits
  in the Quil translation.")

(defparameter *qubit-count* 0
  "The number of qubits registered by qreg.")

(defun parse-qasm (string)
  "Parse STRING into a raw, untransformed PARSED-PROGRAM object."
  (check-type string string)
  (let* ((tok-lines (tokenize string)))
    (let ((parsed-program nil)
          (*creg-names* (make-hash-table :test #'equal))
          (*qreg-names* (make-hash-table :test #'equal))
          (*qubit-count* 0))
      (loop :named parse-loop
            :until (null tok-lines) :do
              (multiple-value-bind (program-entity rest-toks)
                  (parse-program-lines tok-lines)
                (push program-entity parsed-program)
                (setf tok-lines rest-toks)))
      (setf parsed-program (nreverse parsed-program))
      ;; Return the parsed sequence of objects.
      (multiple-value-bind (gate-defs circ-defs memory-defs exec-code)
          (quil::extract-code-sections parsed-program)
        (make-instance 'quil::parsed-program
                       :gate-definitions gate-defs
                       :circuit-definitions circ-defs
                       :memory-definitions memory-defs
                       :executable-code (coerce exec-code 'simple-vector))))))

(defun parse-qasm-string (string &optional originating-file)
  "Parse STRING into a PARSED-PROGRAM object, applying all transforms."
  (let ((pp (parse-qasm string)))
    (setf pp (quil::transform 'quil::process-includes pp originating-file))
    (setf pp (quil::transform 'quil::resolve-applications pp))
    (setf pp (quil::transform 'quil::expand-circuits pp))
    (setf pp (quil::transform 'quil::type-check pp))
    (setf pp (quil::transform 'quil::patch-labels pp))))
