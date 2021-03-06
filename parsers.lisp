#|
Licensed under 3-clause BSD License:
Copyright © 2010, Mathieu Lemoine
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
* Neither the name of Mathieu Lemoine nor the
names of contributors may be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mathieu Lemoine ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mathieu Lemoine BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION\) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\(INCLUDING NEGLIGENCE OR OTHERWISE\) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(cl:in-package #:GAMS-exploration)

(defun GAMS-no-data-line-p (line)
  (or (seq-contains-only-p '(#\Space) line)
      (char= (elt line 0) #\*)))

(defmacro do-GAMS-declarations (destructuring-declaration file
                                &body body)
  "Applies body to each GAMS declaration in the file.
destructuring-declaration is used to destructure the cons representing each declaration.
If file may be a list corresponding to (file &optional ignore-line), c.f. PARSE-GAMS-DECLARATION.
Returns the list of values generated by body."
  (let ((file (if (not (listp file)) (list file) file)))
    (with-gensyms (declaration)
      `(mapcar (lambda (,declaration)
                 (destructuring-bind ,destructuring-declaration ,declaration
                   ,@body))
               (parse-GAMS-declarations ,@file)))))

(defun parse-GAMS-declarations (file &optional (ignore-line #'GAMS-no-data-line-p))
  "Returns the list of GAMS declarations in the file.
The file should be a comma/new-line separated list of declarations.
A declaration is as follow:
key[(arg[, ...])][ comment]
Each line is tested with ignore-line, if the result is non-nil, the line is not parsed.
The result is a list of cons, whose car and cdr are respectively the list (key . args) and the comment."
  (with-open-file (stream file)
    (loop
       for line = (read-line stream nil nil)
       for trimed-line = (string-trim '(#\Space) line)
       while line

       unless (funcall ignore-line line)
       nconc (parse-GAMS-line trimed-line))))

(defun parse-GAMS-line (line)
  "Parses a line of GAMS declarations."
  (loop
     for found-declaration-p = nil

     collect
       (register-groups-bind ((#'length length) key nil args comment eol)
           ("(^ *([^( ]+) *(\\(([^)]+)\\))? *([^(,]*)(,|$))" line :sharedp t)
         (setq found-declaration-p t
               line (subseq line
                            (if (zerop (length eol))
                                length
                                (1+ length))))
         (cons (list* (string-trim '(#\Space) key)
                      (when args
                        (mapcar (curry #'string-trim '(#\Space))
                                (split-sequence #\, args))))
               (string-trim '(#\Space) comment)))

     until (or (null line)
               (zerop (length line)))

     unless found-declaration-p
     do (error "The line contains no declaration.")))

(defun parse-sets (file)
  "Parses the dynamic sets GAMS file.
Set descriptor: set-name[([start-size,[min-size,]]max-size)]"
  (do-GAMS-declarations ((set-name &rest args) . comment) file
    (declare (ignore comment))
    (destructuring-bind (&optional first second third)
        (mapcar #'parse-integer args)
      (apply #'make-dynamic-set set-name
             (ecase (length args)
               (0 nil)
               (1 (list first))
               (2 (list second first))
               (3 (list third first second)))))))

(defun parse-variable-list (file)
  "Parses the variable list GAMS file.
Empty lines, lines whose first character is not a space
  and lines consiting only of spaces and a single ';' are ignored.
Variable descriptor: variable-name[(indice-set[, ...])] [comment]"
  (do-GAMS-declarations ((name . indices) . comment)
      (file (lambda (line)
              (let* ((columns (split-sequence:split-sequence
                               #\Space line
                               :remove-empty-subseqs t
                               :count 2))
                     (cols (length columns)))
                (or (zerop cols)
                    (not (alexandria:starts-with #\Space line))
                    (and (= cols 1)
                         (string= ";" (first columns)))))))
    (declare (ignore comment))
    (make-GAMS-variable name (length indices))))

(defun parse-strategies (file)
  "Parses the strategies GAMS file.
Strategy descriptor: strategy-file-name[(derivation [, set])] [comment]
Strategy domain descriptor: strategy-file-name.[lo|up](set) = bound;
The values of derivation may be enclosed in single quotes.

= the strategy-file-name file must be written in GAMS, its content will be
 included in the model to complete the initialization of the initial-point, as
 if included using:
    $batinclude strategy-file-name [new-set-element [old-set-element]]
 new-set-element will be present only if the derivation is :derived or :family.
 old-set-element will be present only if the derivation is :family.

= derivation must be one of:
  - i[ndependent], this is the default;
  - d[erived];
  - f[amily].

= set, if present, must be the name of an existing set;

= bound must be a non-negative integer.

The domain descriptor is used to define the domain of a strategy (with which
list of set-point is the strategy applicable).

/!\\ If an upper bound is greater than the start size of its associated set,
/!\\ /!\\ the strategy will simply never be used, no error message will be
emited. /!\\

If the set is the one specified in the declaration (i.e. if it is the set used
to derive new points), strategy lower bound must be greater than the set's start
size."
  (let ((strategies))
    (do-GAMS-declarations ((file-name &rest args) . comment) file
      (cond
        ((or (ends-with-subseq ".lo" file-name :test #'char-equal)
             (ends-with-subseq ".up" file-name :test #'char-equal))
         (let* ((set-name  (first args))
                (set       (find-set-from-name set-name))
                (kind      (if (ends-with-subseq ".lo" file-name :test #'char-equal)
                               :lower
                               :upper))
                (file-name (subseq file-name 0 (- (length file-name) 3)))
                (strategy  (find file-name strategies
                                 :key #'strategy-file-name
                                 :test #'string-equal)))
           (when (null strategy)
             (error "The strategy ~A does not exist (yet?)" file-name))
           (when (and (not (or
                            (null set-name)
                            (zerop (length set-name))))
                      (null set))
             (error "set-name, if present, must be an existing set name."))
           (when (not (and
                       (starts-with #\= comment)
                       (ends-with #\; comment)))
             (error "Invalid bound designator"))
           (let ((bound (parse-integer (string-trim '(#\Space)
                                                    (subseq comment 1 (1- (length comment)))))))
             (when (minusp bound)
               (error "bound must be a non-negative integer."))
             (when (and (eq kind :lower)
                        (equalp set (strategy-step-set strategy))
                        (<= bound (1+ (dynamic-set-start-size set))))
               (error "bound for a step set must be greater than the set start ~
               size."))
             (ecase kind
               (:lower (setf (strategy-set-min-size strategy set) bound))
               (:upper (setf (strategy-set-max-size strategy set) bound))))))
        (t (let ((derivation (ecase (elt (string-trim '(#\')
                                                      (or (first args) "i"))
                                         0)
                               (#\i :independent)
                               (#\d :derived)
                               (#\f :family)))
                 (set-name (second args))
                 (set     (find-set-from-name (second args))))
             (when (and (not (or
                              (null set-name)
                              (zerop (length set-name))))
                        (null set))
               (error "set-name, if present, must be an existing set name."))
             (setq strategies
                   (cons (make-strategy file-name derivation set)
                         strategies))))))
    strategies))

(defun parse-stop-criteria (file)
  "Parses stop criteria list.
Stop criteria list is the only file not using a GAMS-like syntax.
Bounds on sets size are defined using SETS-MAX-SIZE and SETS-MIN-SIZE.
Advanced stop criteria may be defined using the DEF-STOP-CRITERION macro."
  (load file)
  (values))

(defun parse-result-point (file initial-point)
  "Extracts a single result point from a lst file."
  (with-open-file (stream file :direction :input)
    (let ((solver)
          (solver-status)
          (solver-iteration)
          (solver-time)
          (objective-value)
          (point (make-hash-table :test 'equalp)))
      (loop
         for line = (read-line stream nil nil) while line

         when (begins-with "     SOLVER  " line :test #'string=)
         do (setq solver (string-nth-column line 2))

         when (begins-with "**** MODEL STATUS " line :test #'string=)
         do (setq solver-status (parse-integer
                                 (string-nth-column line 4)))
           (when (%erroneous-point-p solver-status)
             (loop-finish))

         when (begins-with "**** OBJECTIVE VALUE " line :test #'string=)
         do (setq objective-value (parse-real-number (string-nth-column line 4)))

         when (begins-with " ITERATION COUNT, LIMIT " line :test #'string=)
         do (setq solver-iteration (parse-integer
                                    (string-nth-column line 4)))

         when (begins-with " RESOURCE USAGE, LIMIT " line :test #'string=)
         do (setq solver-time (parse-positive-real-number
                               (string-nth-column line 4)))

         when (begins-with "---- VAR " line :test #'string=)
         do (parse-result-point-variables stream point line)
           (loop-finish))
      (make-result-point initial-point solver
                         solver-status solver-iteration solver-time
                         objective-value point))))

(defun read-next-data-or-variable-line-in-result-point (stream)
  "Ignores all data until:
- a line denoting a new variable value;
- the line denoting the end of variables' value."
  (declare (type stream stream))
  (loop
     for line = (read-line stream)

     when (starts-with #\Page line)
     do (read-line stream)
       (read-line stream)

     until (or (begins-with "**** REPORT SUMMARY :" line :test #'string=)
               (begins-with "---- VAR " line :test #'string=)
               (and (> (length line) 0)
                    (alpha-char-p (elt line 0))))
     finally (return line)))

(defun parse-result-point-variables (stream point first-line)
  "Parses the value of the variables in an lst file."
  (declare (type stream stream)
           (hash-table point)
           (string first-line))
  (loop
     with line = first-line
     for variable-name = (string-nth-column line 3)
     for variable = (find variable-name *variables*
                          :key #'GAMS-variable-name
                          :test #'string-equal)

     do (assert (begins-with "---- VAR " line :test #'string=) ()
                "line must be the first line of a variable display")

     if variable
     do (setq line
              (if (zerop (GAMS-variable-dimension variable))
                  (parse-result-scalar-variable stream point line)
                  (parse-result-vector-variable stream point line)))
     else
     do (setq line (read-next-data-or-variable-line-in-result-point stream))

     until (begins-with "**** REPORT SUMMARY :" line :test #'string=)))

(defun parse-result-scalar-variable (stream point line)
  "Parses a scalar variable in a lst file."
  (declare (type stream stream)
           (hash-table point)
           (string line))
  (let ((columns (split-sequence #\Space line
                                 :remove-empty-subseqs t)))
    (assert (<= 6 (length columns) 8))
    (setf (gethash (make-point-key (third columns)) point)
          (let ((value (fifth columns)))
            (if (string= "." value)
                0
                (the real (parse-number value)))))
    (read-next-data-or-variable-line-in-result-point stream)))

(defun parse-result-vector-variable (stream point first-line)
  "Parses a vector variable in a lst file."
  (declare (type stream stream)
           (hash-table point)
           (string first-line))
  (let ((variable-name (string-nth-column first-line 3)))
    (loop
       for line = (read-next-data-or-variable-line-in-result-point stream)
       while (alpha-char-p (elt line 0))

       do (let ((columns (split-sequence #\Space line
                                         :remove-empty-subseqs t
                                         :count 3)))
            (setf (gethash (apply #'make-point-key variable-name
                                  (split-sequence #\. (first columns)))
                           point)
                  (let ((value (third columns)))
                    (if (string= "." value)
                        0
                        (the real (parse-number value))))))

       finally (return line))))

(defun parse-solver-list (file)
  "Parses the solver list.
The file contains one solver set specification per line.
A solver set specification is either:
- 'default'
- a space-separated list of pairs {model-kind}={solver}
  {model-kind} being a kind of GAMS model (lp, nlp, mip, ...)"
  (with-open-file (stream file :direction :input)
    (loop
       for line = (read-line stream nil nil) while line

       if (string= line "default") collect nil
       else collect (split-sequence #\Space line
                                    :remove-empty-subseqs t))))
