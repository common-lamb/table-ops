;; (in-package #:cl-user)
(defpackage :xlsx-ops
  (:use #:cl)
  (:local-nicknames (:c :cmd)
                    (:t :transducers)
                    (:ls :ls-user)
                    (:xlsx :lisp-xl)
                    (:fuzz :fuzzy-match)
                    (:fifi :file-finder))
  (:export :check-dependencies
           :xlsx->overview
           :xlsx->df
           ))

(in-package :xlsx-ops)

(defun check-dependencies ()
  "&&& ensure all is as expected"
  ;; all good
  T)

(defun xlsx->overview (xlsx &key (n-rows 4))
  "generic xlsx summary to orient user for navigation
args:
  xlsx, #P to a spreadsheet
  :n-rows, n of rows in head, 1 based, set to nil for all lines"

  ;; intro
  (format t "~&~%Summary of: ~A~&~%" xlsx)

  ;; enumerate and print all sheet titles
  (format t "~&Contents:~&")
  (xlsx:with-all-excel-sheets (xlsx sheet n name)
    (format t "~&N: ~A Sheet-title: ~A~&" n name))

  ;; detailed sheet stats
  (xlsx:with-all-excel-sheets (xlsx sheet n name)
    ;; intro title
    (format t "~&~%N: ~A Sheet: ~A~&~%" n name)

    ;; print head
    (format t "~&Head:")
    (xlsx:process-sheet sheet :max-row n-rows :row-function #'print)

    )
  (pathname-name xlsx))

(defun drop-trailing-commas (line)
  "drop all trailing commas"
  (str:replace-all ",{1,}$" "" line :regex t))

(defun replace-pound (line)
  "# -> XNUMX
"
  (str:replace-all "#" "XNUMX" line :regex nil))

(defun count-commas (line)
  "count only delimiting commas in a csv file line,
  does not include the textual commas in a string"
  (let* (
         ;; drop quote delimited regions which contain a comma"
         (drop (ppcre:regex-replace-all "\"[^\"]*,+[^\"]*\"" line ""))
         ;; remaining commas separate columns
         (count (str:count-substring "," drop)))
    count))

(defun add-commas (line max)
  "ensure there are n=max commas in the line by adding any needed"
  (let* ((missing (- max
                 (count-commas line)))
         (suf (if (> missing 0)
                  ;; build if +ve
                  (make-string missing :initial-element #\,)
                  "" ;; add nothing if add is -ve
                  ))
         (padded (str:concat line suf)))
    padded))

(defun drop-linefeed (line)
  "\r -> nil, fix windows line ends
&&& not really tested, it started working immediately after I wrote this. maybe its scared"
  (string-trim '(#\Return) line))

(defun xlsx->df (xlsx &key sheet-index (initial-row 1) )
  "open the indicated sheet of the spreadsheet, starting at row
replace symbols and square-up trailing commas
return a lispstat dataframe"
  (assert (not (null sheet-index)) (sheet-index) "you must select a sheet index for xlsx:~%  ~A" xlsx)
  ;; &&& test file type
  (uiop:with-temporary-file (:stream in)
    (uiop:with-temporary-file (:stream out)
      (handler-bind
          ;; catch file-exists
          ((error (lambda (c)
                    (format t "~%Handling error: ~a~%" c)
                    ;; does not recognize supersede
                    ;; (print (find-restart 'supersede c))
                    (invoke-restart
                     (first (compute-restarts c))))))
        ;; errors on the existence of the in temp file
        (lisp-xl-csv:excel-to-csv xlsx in sheet-index :initial-row initial-row))

      (let* (
             ;; repair dirty file common issues
             (clean (t:transduce
                     (t:comp
                      ;;(t:map #'read-line) ;; automatically read by lines
                      (t:map #'drop-linefeed)
                      ;; kill spurious empty cells
                      (t:map #'drop-trailing-commas)
                      ;; replace erroneous symbols
                      (t:map #'replace-pound)
                      ;; &&& sanitize characters
                      )
                     #'t:cons
                     in))

             ;; count max columns
             (max (t:transduce
                   (t:comp
                    (t:map #'count-commas)
                    )
                   (t:fold #'cl:max 0)
                   clean))

             ;; append any needed cells and write line
             (square (t:transduce
                      (t:comp
                       (t:map (lambda (l) (add-commas l max)))
                       )
                      (t:for (lambda (l) (write-line l out)))
                      clean))
             ) ; let* binding form
        (force-output out) ; flush stream
        (file-position out 0) ; back to the top for the read
        (ls:read-csv out)
        ))))
