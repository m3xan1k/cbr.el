(require 'dom)
(require 'url)
(require 'esxml-query)

(defconst cbr-url "https://www.cbr.ru/currency_base/daily/")

(defun print-elements-of-list (list)
  (while list
    (message (car list))
    (setq list (cdr list))))

(url-retrieve
 cbr-url
 (lambda (status &rest args)
   (goto-char url-http-end-of-headers)
   (forward-line 1)
   (let* ((html (libxml-parse-html-region (point) (point-max)))
	  (rows (esxml-query-all ".data > tbody > tr" html))
	  (rows (cdr rows)))
     (while rows
       (let* ((data (vconcat [] (esxml-query-all "td" (car rows))))
	      (code (elt (elt data 1) 2))
	      (nominal (string-to-number (elt (elt data 2) 2)))
	      (value (string-to-number (replace-regexp-in-string "\," "\." (elt (elt data 4) 2))))
	      (result (number-to-string (/ (* value 1.0) nominal))))
	 (message (format "%s %s" code result)))
       (setq rows (cdr rows))))))
