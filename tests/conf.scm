;; Configuration file for tests.  To be used together with with other
;; test files, like:
;;
;;    $ awful ajax.scm conf.scm

(use spiffy awful html-tags)

(debug-log (current-error-port))
(ajax-library "jquery.min.js")

(page-exception-message
 (lambda (exn)
   (<pre> convert-to-entities?: #t
          (with-output-to-string
            (lambda ()
              (print-call-chain)
              (print-error-message exn))))))
