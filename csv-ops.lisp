(in-package #:cl-user)
(defpackage :csv-ops
  (:use #:cl)
  (:export
   :check-dependencies
   :txt->csv
   ))

(in-package :csv-ops)

(defun check-dependencies ()
  "&&& ensure all is as expected"
  ;; all good
  T)

(defun txt->csv (txt &key (separator #\Tab) (output-dir *process-dir*))
  "
ARGS:
txt: white space delimited file
DOES:
uses the separator character to read the delimited text file
writes a csv to to output-dir, reusing the name of the txt file
RETS:
pathname to the csv
"
  (let ((data (cl-csv:read-csv txt :separator separator))
        (out-pathname (make-pathname :defaults output-dir
                                 :name (pathname-name txt)
                                 :type "csv")))
    ;; write csv
    (with-open-file (out out-pathname :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
      (cl-csv:write-csv data :stream out))
    ;; returning
    out-pathname))
