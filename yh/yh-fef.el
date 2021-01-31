;;; yh-fef.el --- format elisp file -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:
(require 'yh)



;;; list utils
(defun yh-fef-mapcar* (f &rest xs)
  "Apply F on each value of XS."
  (when (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
          (apply 'yh-fef-mapcar* f (mapcar 'cdr xs)))))

(defun yh-fef-take-while (ls pred)
  "Take leading elements from LS that satisfies PRED."
  (and ls
       (if (funcall pred (car ls))
           (cons (car ls) (yh-fef-take-while (cdr ls) pred))
         nil)))

(defun yh-fef-drop-while (ls pred)
  "Drop leading elements from LS that satisfies PRED."
  (and ls
       (if (funcall pred (car ls))
           (yh-fef-drop-while (cdr ls) pred)
         ls)))


;;; constructors
(defun yh-fef-line (type s)
  "Construct line of type TYPE and value S."
  (let ((length (seq-length s)))
    `(line
      (:type . ,type)
      (:value . ,s)
      (:size . ,length))))

(defun yh-fef-block (lines)
  "Construct block from LINES."
  (let ((size (apply '+ (mapcar 'yh-fef-size lines)))
        (length (seq-length lines)))
    `(block
      (:size . ,(+ size (- length 1)))
      (:length . ,length)
      (:lines . ,lines))))

;;; predicates
(defun yh-fef-line-p (obj)
  "Is OBJ line ?"
  (and (listp obj)
       (eq (car obj) 'line)))

(defun yh-fef-block-p (obj)
  "Is OBJ block ?"
  (and (listp obj)
       (eq (car obj) 'block)))

(defun yh-fef-line-blank-p (line)
  "Is LINE blank ?"
  (eq (yh-fef-line-type line) 'blank))

;;; accessors
(defun yh-fef-label (obj)
  "Get label of OBJ."
  (car obj))

(defun yh-fef-data (obj)
  "Get data of OBJ."
  (cdr obj))

(defun yh-fef-size (obj)
  "Get number of characters for representing this OBJ."
  (alist-get ':size (yh-fef-data obj)))

(defun yh-fef-length (block)
  "Get number of lines of this BLOCK."
  (alist-get ':length (yh-fef-data block)))

(defun yh-fef-lines (block)
  "Get lines of BLOCK."
  (alist-get ':lines (yh-fef-data block)))

(defun yh-fef-line-type (line)
  "Get type of LINE."
  (alist-get ':type (yh-fef-data line)))

(defun yh-fef-line-value (line)
  "Get value of LINE."
  (alist-get ':value (yh-fef-data line)))



;;; parsers
(defun yh-fef-parse-line (string)
  "Parse 1 line from STRING."
  (let ((blank-line (rx string-start (zero-or-more blank) string-end))
        (section-header (rx string-start ";;;" (zero-or-more any) string-end))
        (subsection-header (rx string-start ";;" (zero-or-more any) string-end)))
    (cond ((string-match blank-line string) (yh-fef-line 'blank string))
          ((string-match section-header string) (yh-fef-line 'section-header string))
          ((string-match subsection-header string) (yh-fef-line 'subsection-header string))
          (t (yh-fef-line 'code string)))))

(defun yh-fef-parse-program (string)
  "Parse program code represented by STRING."
  (let ((splitted (split-string string "\n")))
    (mapcar 'yh-fef-parse-line splitted)))


(defun yh-fef-read-block (lines)
  "Read leading blanks or codes from LINES."
  (when lines
    (let* ((l1 (car lines))
           (t1 (yh-fef-line-type l1))
           (pred (if (eq t1 'blank) 'yh-fef-line-blank-p
                   #'(lambda (x) (not (yh-fef-line-blank-p x)))))
           (block (yh-fef-block (yh-fef-take-while lines pred)))
           (rest (yh-fef-drop-while lines pred)))
      (cons block rest))))

(defun yh-fef-split-to-blocks (lines)
  "Split LINES into blocks."
  (when lines
    (let* ((block-rest (yh-fef-read-block lines))
           (block (car block-rest))
           (lines2 (cdr block-rest)))
      (cons block (yh-fef-split-to-blocks lines2)))))







(defun yh-fef-parse-blank-lines (lines)
  "Parse LINES.  Consume leading blank lines."
  (let ((ret ()))
    (while (and lines
                (eq (yh-fef-line-type (car lines)) 'blank))
      (setq ret (cons (car lines) ret)
            lines (cdr lines)))
    (cons (seq-reverse ret) lines)))

(defun yh-fef-parse-code-block (lines)
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
         (block-lines ()))
    (setq block-lines (yh-fef-parse-blank-lines lines)
          ret (cons (car block-lines) ret))
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

(defun yh-fef-block-string (block)
  "Stringify BLOCK."
  (mapconcat 'identity (mapcar 'yh-fef-line-value block) "\n"))

(defun yh-fef-block-type (block n)
  "Return type of N th element of BLOCK."
  (yh-fef-line-type (yh-fef-nth-line block n)))

(defun yh-fef-nth-line (block n)
  "Return N th line of BLOCK."
  (seq-elt block n))

(defun yh-fef-format-blocks (blocks)
  "Format BLOCKS."
  (when blocks
    (let* ((s0 (yh-fef-block-string (car blocks)))
           (b1 (cdr blocks))
           (s1 (mapcar 'yh-fef-block-string b1))
           (seps (mapcar #'(lambda (b)
                             (if (eq (yh-fef-block-type b 0) 'section-header)
                                 "\n\n" "\n")) b1))
           (s2 (yh-mapcar 'concat seps s1)))
      (mapconcat 'identity (cons s0 s2) "\n"))))

(defun yh-fef-format (program)
  "Format PROGRAM."
  (yh-fef-format-blocks (yh-fef-parse program)))

(defun yh-fef-format-buffer ()
  "Format BUFFER."
  (interactive)
  (let ((formatted (yh-fef-format (buffer-string))))
    (erase-buffer)
    (insert formatted)
    (insert "\n")))

(provide 'yh-fef)
;;; yh-fef.el ends here