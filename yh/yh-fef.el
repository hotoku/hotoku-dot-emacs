;;; yh-fef.el --- format elisp file -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun yh-fef-line (type s)
  "Construct line of type TYPE and value S."
  (if (eq type 'blank) '(blank . "") `(,type . ,s)))

(defun yh-fef-line-type (l)
  "Return type of L."
  (car l))

(defun yh-fef-line-value (l)
  "Return value of L."
  (cdr l))

(defun yh-fef-parse-line (line)
  "Parse 1 LINE."
  (let ((blank-line (rx string-start (zero-or-more blank) string-end))
        (section-header (rx string-start ";;;" (zero-or-more any) string-end))
        (subsection-header (rx string-start ";;" (zero-or-more any) string-end)))
    (cond ((string-match blank-line line) `(blank . ,""))
          ((string-match section-header line) `(section-header . ,line))
          ((string-match subsection-header line) `(subsection-header . ,line))
          (t `(code . ,line)))))



(defun yh-fef-blank-lines (lines)
  "Parse LINES.  Consume leading blank lines."
  (let ((num 0))
    (while (and lines
                (eq (yh-fef-line-type (car lines)) 'blank))
      (setq num (1+ num)
            lines (cdr lines)))
    (cons (if (< 0 num) (yh-fef-line 'blank "") nil) lines)))

(defun yh-fef-code-block (lines)
  "Parse LINES.  Consume leading code lines."
  (let ((ret ()))
    (while (and lines
                (not (eq (yh-fef-line-type (car lines)) 'blank)))
      (setq ret (cons (car lines) ret)
            lines (cdr lines)))
    (cons (seq-reverse ret) lines)))

(defun yh-fef-parse-lines (lines)
  "Parse multiple LINES."
  (let* ((ret ())
         (block-lines (yh-fef-blank-lines lines))
         (lines (cdr block-lines)))
    (while lines
      (setq block-lines (yh-fef-code-block lines)
            ret (cons (car block-lines) ret)
            lines (cdr block-lines)
            block-lines (yh-fef-blank-lines lines)
            lines (cdr block-lines)))
    (seq-reverse ret)))

(defun yh-fef-parse (program)
  "Parse PROGRAM."
  (let* ((lines (split-string program "\n"))
         (parsed (mapcar 'yh-fef-parse-line lines)))
    (yh-fef-parse-lines parsed)))


(provide 'yh-fef)
;;; yh-fef.el ends here
