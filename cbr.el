(require 'url)
(require 'esxml-query)

(defconst cbr-url "https://www.cbr.ru/currency_base/daily/")
(defconst currency-codes ["AUD" "AZN" "AMD" "BYN" "BGN" "BRL"
			  "HUF" "KRW" "VND" "HKD" "GEL" "DKK"
			  "AED" "USD" "EUR" "EGP" "INR" "KZT"
			  "CAD" "QAR" "KGS" "CNY" "MDL" "NZD"
			  "TMT" "NOK" "PLN" "RON" "XDR" "RSD"
			  "SGD" "TJS" "THB" "TRY" "UZS" "UAH"
			  "GBP" "CZK" "SEK" "CHF" "ZAR" "JPY"])

(defun m3xan1k-cbr--get-html ()
  "http request to cbr url and parse response body to esxml"
  (with-current-buffer (url-retrieve-synchronously cbr-url)
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (libxml-parse-html-region (point) (point-max))))

(defun m3xan1k-cbr--html-to-rows (html)
  "Returns each currency data list
HTML - parsed html by libxml.
Returns list of lists with currency data."
  (let ((rows (esxml-query-all ".data > tbody > tr" html)))
    (cdr rows)))

(defun m3xan1k-cbr--rate-to-float (rate)
  "Convert string to float.
RATE - string.
Example \"1,000\" to 1.000"
  (string-to-number
   (replace-regexp-in-string "\," "\." rate)))

(defun m3xan1k-cbr--process-row (row)
  "Extract and process currency data from row.
ROW - vector of lists.
Example [(td nil \"036\") (td nil \"AUD\") (td nil \"1\") (td nil \"Австралийский доллар\") (td nil \"59,14\")]
Return example (\"USD\" . 100.00)"
  (let* ((currency-code (upcase (elt (elt row 1) 2)))
	 (currency-nominal (string-to-number (elt (elt row 2) 2)))
	 (currency-rate (m3xan1k-cbr--rate-to-float (elt (elt row 4) 2)))
	 (final-rate (/ (* currency-rate 1.0) currency-nominal)))
    (cons currency-code final-rate)))

(defun m3xan1k-cbr-get-rate (target-code)
  "Prints currency rate in rub for given code.
TARGET-CODE - on of currency-codes"
  (interactive "sCurrency code: ")
  (if (not (seq-contains-p currency-codes (upcase target-code)))
      (print (format "No such code: %s" target-code))
    (let* ((html (m3xan1k-cbr--get-html))
	   (rows (m3xan1k-cbr--html-to-rows html))
	   (currency-map (make-hash-table :test 'equal)))
      (while rows
	(let* ((row (vconcat [] (esxml-query-all "td" (car rows))))
	       (code-rate-pair (m3xan1k-cbr--process-row row)))
	  (puthash (car code-rate-pair) (format "%.2f" (cdr code-rate-pair)) currency-map))
	(setq rows (cdr rows)))
      (print (gethash (upcase target-code) currency-map)))))
