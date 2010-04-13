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

(cl:in-package #:GAMS-dynamic-sets)

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

(defun parse-GAMS-declarations (file &optional (ignore-line (rcurry #'empty-string-p '(#\Space))))
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
  (break)
  (loop
     for found-declaration-p = nil

     collect
       (register-groups-bind ((#'length length) key nil args comment eol)
           ("(^ *([a-zA-Z][a-zA-Z0-9]*) *(\\(([^)]+)\\))? *([^(,]*)(,|$))" line :sharedp t)
         (setq found-declaration-p t
               line (subseq line
                            (if (zerop (length eol))
                                length
                                (1+ length))))
         (cons (list* (string-trim '(#\Space) key)
                      (mapcar (curry #'string-trim '(#\Space))
                              (split-sequence #\, args)))
               (string-trim '(#\Space) comment)))

     until (or (null line)
               (zerop (length line)))

     unless found-declaration-p
     do (error "The line contains no declaration.")))

(defun parse-sets (file)
  "Parses the dynamic sets file.
The file should contains a comma/new-line separated list of set descriptors.
A set descriptor is as follow:
set-name[([min-size,]max-size)] [comment]"
  (do-GAMS-declarations ((set-name &optional min-size max-size) . comment) file
    (declare (ignore comment))
    (when (null max-size)
      (shiftf max-size min-size 1))
    (make-dynamic-set set-name max-size min-size)))

(defun parse-variable-list (file)
  "Parses the variable list file.
The file is a GAMS file used within the model.
Empty lines, lines whose first character is not a space
  and lines consiting only of a ';' and some spaces are ignored.
Other lines must contain only one variable declaration per line.
A variable declaration is as follows:
variable-name[(indice-set[, ...])] [comment]"
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
    (make-GAMS-variable name indices)))

(defun parse-strategies (file)
  "Parses the strategy file.
The file should contains a strategy specification per line.
A strategy specification is as follow:
strategy-file-name(derivation [, [set] [, stage]]) [comment]

= the strategy-file-name file must be written in GAMS,
 Its content will be include to complete the initialization of the initial-point,
 as if include using:
    $batinclude strategy-file-name [new-set-element [old-set-element]]
 new-set-element will be present only if the derivation is :derived or :family.
 old-set-element will be present only if the derivation is :family.

= derivation, if present, must be one of:
  - i[ndependent], this is the default;
  - d[erived];
  - f[amily].

= set, if present, must be the name of an existing set;

= stage, if present, must be one of:
  - e[mpty-set];
  - 1 or f[irst-element];
  - n or a[dditional-element];
  - * or A[lways];
  - + or N[on-empty-set]."
  (do-GAMS-declarations ((file-name derivation &optional set-name stage) . comment) file
    (declare (ignore comment))
    (let ((derivation (ecase (elt derivation 0)
                        (#\i :independent)
                        (#\d :derived)
                        (#\f :family)))
          (set     (find-set-from-name set-name))
          (stage   (when stage
                     (ecase (elt stage 0)
                       (#\e       :empty-set)
                       ((#\1 #\f) :first-element)
                       ((#\n #\a) :additional-element)
                       ((#\* #\A) :always)
                       ((#\+ #\N) :non-empty-set)))))
      (when (and (not (or
                       (null set-name)
                       (zerop (length set-name))))
                 (null set))
        (error "set-name, if present, must be an existing set name."))
      (create-strategy file-name derivation set stage))))

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
    (assert (member (length columns) '(6 7) :test #'=))
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
