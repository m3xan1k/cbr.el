(require 'url)
(require 'esxml-query)

(defconst cbr-url "https://www.cbr.ru/currency_base/daily/")

(defun m3xan1k-cbr--rate-to-float (rate)
  (string-to-number
   (replace-regexp-in-string "\," "\." rate)))

(defun m3xan1k-cbr--html-to-rows (html)
  (let ((rows (esxml-query-all ".data > tbody > tr" html)))
    (cdr rows)))

(defun m3xan1k-cbr--process-row (row)
  (let* ((currency-code (elt (elt row 1) 2))
	 (currency-nominal (string-to-number (elt (elt row 2) 2)))
	 (currency-rate (m3xan1k-cbr--rate-to-float (elt (elt row 4) 2)))
	 (final-rate (/ (* currency-rate 1.0) currency-nominal)))
    (cons currency-code final-rate)))

(url-retrieve
 cbr-url
 (lambda (status &rest args)
   (goto-char url-http-end-of-headers)
   (forward-line 1)
   (let* ((html (libxml-parse-html-region (point) (point-max)))
	  (rows (m3xan1k-cbr--html-to-rows html))
	  (currency-map (make-hash-table :test 'equal)))
     (while rows
       (let* ((row (vconcat [] (esxml-query-all "td" (car rows))))
	      (code-rate-pair (m3xan1k-cbr--process-row row)))
	 (progn
	   (puthash (car code-rate-pair) (format "%.2f" (cdr code-rate-pair)) currency-map)
	   (print (equal (gethash (car code-rate-pair) currency-map) (format "%.2f" (cdr code-rate-pair))))))
       (setq rows (cdr rows))))))
