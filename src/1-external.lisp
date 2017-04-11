

(in-package :alien)

(cl-syntax:use-syntax :cl-interpol)

(defun find-domain (problem-path)
  (block nil
    (let ((dpath (make-pathname :defaults problem-path :name "domain")))
      (when (probe-file dpath) (return dpath)))
    (let ((dpath (make-pathname :defaults problem-path :name
                                (format nil "~a-domain" (pathname-name problem-path)))))
      (when (probe-file dpath) (return dpath)))
    (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
           problem-path
           (make-pathname :defaults problem-path :name "domain")
           (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path))))))

(defun fd-preprocess (problem &optional domain)
  (let ((domain (or domain (find-domain problem))))
    (assert (probe-file domain))
    ($ "echo $PWD" :output t :error-output t)
    ($ #?"time ${*fd-home*}/fast-downward.py --translate ${domain} ${problem}" :output t :error-output t)
    (sas-parser:parse "output.sas")
    #+nil ($ #?"time ${*fd-home*}/fast-downward.py --preprocess output.sas" :output t :error-output t)
    #+nil ($ #?"mv output ${path}")
    #+nil (sas-parser:parse "output")))

