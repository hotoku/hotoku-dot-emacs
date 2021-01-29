;;; yh-fef-test.el --- test for yh-fef -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'yh-fef)
(require 'ert)


(ert-deftest yh-fef-test-line ()
  (let ((blank (yh-fef-line 'blank "   "))
        (code (yh-fef-line 'code "abc")))
    (should (equal (yh-fef-line-type blank) 'blank))
    (should (equal (yh-fef-line-value blank) ""))
    (should (equal (yh-fef-line-type code) 'code))
    (should (equal (yh-fef-line-value code) "abc"))))

(ert-deftest yh-fef-test-parse-line ()
  "Test of parsing one line."
  (should (equal (yh-fef-parse-line "     ") (yh-fef-line 'blank "")))
  (should (equal (yh-fef-parse-line ";;; header") (yh-fef-line 'section-header ";;; header")))
  (should (equal (yh-fef-parse-line ";; comment") (yh-fef-line 'subsection-header ";; comment")))
  (should (equal (yh-fef-parse-line "(+ 1 1)") (yh-fef-line 'code "(+ 1 1)"))))

(ert-deftest yh-fef-test-blank-lines ()
  (let* ((parsed (list (yh-fef-line 'blank "")
                       (yh-fef-line 'blank "")
                       (yh-fef-line 'code "abc")))
         (ret (yh-fef-blank-lines parsed))
         (blank (car ret))
         (rest (cdr ret)))
    (should (equal blank (yh-fef-line 'blank "")))
    (should (equal (car rest) (yh-fef-line 'code "abc")))))

(ert-deftest yh-fef-test-code-block ()
  (let* ((parsed (list (yh-fef-line 'code "abc")
                       (yh-fef-line 'code "def")
                       (yh-fef-line 'blank "")
                       (yh-fef-line 'code "ghi")))
         (ret (yh-fef-code-block parsed))
         (block (car ret))
         (rest (cdr ret)))
    (should (equal (seq-length block) 2))
    (should (equal (seq-elt block 0) (yh-fef-line 'code "abc")))
    (should (equal (seq-elt block 1) (yh-fef-line 'code "def")))
    (should (equal (seq-length rest) 2))))

(ert-deftest yh-fef-test-parse-lines ()
  (let* ((lines (list (yh-fef-line 'blank "")
                      (yh-fef-line 'code "abc")
                      (yh-fef-line 'code "def")
                      (yh-fef-line 'blank "")
                      (yh-fef-line 'code "ghi")
                      (yh-fef-line 'code "jkl")
                      (yh-fef-line 'blank "")
                      (yh-fef-line 'blank "")))
         (blocks (yh-fef-parse-lines lines)))
    (should (equal (seq-length blocks) 2))
    (let ((b0 (seq-elt blocks 0))
          (b1 (seq-elt blocks 1)))
      (should (equal (seq-length b0) 2))
      (should (equal (seq-length b1) 2))
      (should (equal (yh-fef-line-type (seq-elt b0 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b0 0)) "abc"))
      (should (equal (yh-fef-line-type (seq-elt b1 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b1 0)) "ghi")))))

(ert-deftest yh-fef-test-parse-lines2 ()
  (let* ((lines (list (yh-fef-line 'code "abc")
                      (yh-fef-line 'code "def")
                      (yh-fef-line 'blank "")
                      (yh-fef-line 'code "ghi")
                      (yh-fef-line 'code "jkl")))
         (blocks (yh-fef-parse-lines lines)))
    (should (equal (seq-length blocks) 2))
    (let ((b0 (seq-elt blocks 0))
          (b1 (seq-elt blocks 1)))
      (should (equal (seq-length b0) 2))
      (should (equal (seq-length b1) 2))
      (should (equal (yh-fef-line-type (seq-elt b0 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b0 0)) "abc"))
      (should (equal (yh-fef-line-type (seq-elt b1 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b1 0)) "ghi")))))

(ert-deftest yh-fef-test-parse ()
  (let* ((program "abc
def

ghi
jkl")
         (blocks (yh-fef-parse program)))
    (should (equal (seq-length blocks) 2))
    (let ((b0 (seq-elt blocks 0))
          (b1 (seq-elt blocks 1)))
      (should (equal (seq-length b0) 2))
      (should (equal (seq-length b1) 2))
      (should (equal (yh-fef-line-type (seq-elt b0 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b0 0)) "abc"))
      (should (equal (yh-fef-line-type (seq-elt b1 0)) 'code))
      (should (equal (yh-fef-line-value (seq-elt b1 0)) "ghi")))))

(ert-deftest yh-fef-test-block-string ()
  (let* ((block (list (yh-fef-line 'code "abc")
                      (yh-fef-line 'code "def")))
         (s (yh-fef-block-string block)))
    (should (equal s "abc\ndef"))))

(ert-deftest yh-fef-test-format-blocks ()
  (let* ((blocks (list (list (yh-fef-line 'section-header ";;; abc")
                             (yh-fef-line 'code "def"))
                       (list (yh-fef-line 'section-header ";;; ghi")
                             (yh-fef-line 'code "jkl")))))
    (should (equal (yh-fef-format-blocks blocks) ";;; abc\ndef\n\n\n;;; ghi\njkl"))))

(ert-deftest yh-fef-test-format ()
  (should (equal (yh-fef-format "\n\n;;; abc\ndef\n\n\n;;; ghi\njkl\n\n\n\nmno")
                 ";;; abc\ndef\n\n\n;;; ghi\njkl\n\nmno")))


(provide 'yh-fef-test)
;;; yh-fef-test.el ends here
