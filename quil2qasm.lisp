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

(deftype qasm-keywords ()
  '(member "OPENQASM" "qreg" "creg" "gate" "barrier" "measure" "reset" "opaque" "include"))

(deftype token-type ()
  '(or
    qasm-keywords
    (member :SEMI-COLON :LEFT-PAREN :RIGHT-PAREN :COMMA :ARROW
                        :LEFT-SQUARE-BRACKET :RIGHT-SQUARE-BRACKET
                        :LEFT-CURLY-BRACKET :RIGHT-CURLY-BRACKET
                        :LEFT-ANGLE-BRACKET :RIGHT-ANGLE-BRACKET
                        :PLUS :MINUS :TIMES :DIVIDE :EXPT :EQUALSEQUALS
                        :NNINTEGER :REAL :ID :STRING :KEYWORD)))

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
   (return (tok :LEFT-SQUARE-BRACKET)))
  ((eager "\\}")
   (return (tok :RIGHT-SQUARE-BRACKET)))
  ((eager "\\<")
   (return (tok :LEFT-ANGLE-BRACKET)))
  ((eager "\\>")
   (return (tok :RIGHT-ANGLE-BRACKET)))
  ((eager "\\,")
   (return (tok :COMMA)))
  ("OPENQASM|qreg|creg|barrier|measure|reset|opaque"
   (return (tok :KEYWORD $@)))
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
