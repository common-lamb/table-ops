(defsystem "lamb.files.xlsx"
  :description "xlsx open, convert and report"
  :author "common-lamb (https://github.com/common-lamb)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               ;; essential
               :cmd
               :str
               :alexandria
               :serapeum
               :iterate
               :bordeaux-threads
               ;; this project
               :filesystem-utils
               :fuzzy-match
               :file-finder
               :transducers
               ;; clone
               :filepaths
               :lisp-xl
               :lamb.base.click
               :lisp-stat
               )
  :serial t
  :components ((:file "xlsx-ops"))
  )
