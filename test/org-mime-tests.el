;; counsel-etags-tests.el --- unit tests for counsel-etags -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(require 'ert)
(require 'org-mime)
(require 'message)

(defconst mail-header '("To: myname@mail.com\n"
                       "Subject: test subject\n"
                       "From: My Name <myname@yahoo.com>\n"
                       "--text follows this line--\n"))
(defconst mail-footer '("--\n"
                        "Yous somebody\n\n"
                        "--\n"
                        "Some quote\n"))

(ert-deftest test-org-mime-htmlize ()
  (let* (str)
    (with-temp-buffer
      (apply #'insert mail-header)
      (insert "* hello\n"
              "** world\n"
              "#+begin_src javascript\n"
              "console.log('hello world');"
              "#+end_src\n")
      (apply #'insert mail-footer)
      (message-mode)
      (goto-char (point-min))
      (org-mime-htmlize)
      (setq str (buffer-string)))
    (should (string-match "<#multipart" str))))

(ert-deftest test-org-mime-org-subtree-htmlize ()
  (let* (str opts)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+begin_src javascript\n"
              "console.log('hello world');"
              "#+end_src\n")
      (org-mode)
      (goto-char (point-min))
      (setq opts (org-mime-get-export-options t))
      (should opts)
      (org-mime-org-subtree-htmlize)
      (switch-to-buffer (car (message-buffers)))
      (setq str (buffer-string)))
    (should (string-match "Subject: hello" str))
    (should (string-match "<#multipart" str))))

(ert-deftest test-org-mime-org-buffer-htmlize ()
  (let* (str opts)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+begin_src javascript\n"
              "console.log('hello world');"
              "#+end_src\n")
      (org-mode)
      (goto-char (point-min))
      (setq opts (org-mime-get-export-options t))
      (should opts)
      (org-mime-org-buffer-htmlize)
      (switch-to-buffer (car (message-buffers)))
      (setq str (buffer-string)))
    (should (string-match "<#multipart" str))))

(ert-deftest test-org-mime-build-mail-other-headers ()
 (let* ((cc "cc@m.c")
        (bcc "bcc@m.c")
        (from "from@m.c")
        h)
   ;; CC
   (setq h (nth 0 (org-mime-build-mail-other-headers cc nil nil)))
   (should (string= (car h) "Cc"))

   ;; CC and BCC
   (setq h (nth 0 (org-mime-build-mail-other-headers cc bcc nil)))
   (should (string= (car h) "Bcc"))
   (should (string= (cdr h) bcc))
   (setq h (nth 1 (org-mime-build-mail-other-headers cc bcc nil)))
   (should (string= (car h) "Cc"))
   (should (string= (cdr h) cc))

   ;; CC, BCC, and FROM
   (setq h (nth 0 (org-mime-build-mail-other-headers cc bcc from)))
   (should (string= (car h) "From"))
   (should (string= (cdr h) from))
   (setq h (nth 1 (org-mime-build-mail-other-headers cc bcc from)))
   (should (string= (car h) "Bcc"))
   (should (string= (cdr h) bcc))
   (setq h (nth 2 (org-mime-build-mail-other-headers cc bcc from)))
   (should (string= (car h) "Cc"))
   (should (string= (cdr h) cc))))

;;; The ASCII export test checks for org-mode markup for the default case, where
;;; the export variable is nil or not valid, and checks for absent org-mode
;;; markup for the three valid plain text exports. The ASCII export test does
;;; not attempt to verify the exported coding type.

(ert-deftest test-org-mime-org-buffer-htmlize-ascii-plain-text ()
  (let* (str opts)
    (setq orgBuf (generate-new-buffer "*org-mode-test-buf*"))
    (with-current-buffer orgBuf
      (insert "#+OPTIONS: toc:nil num:nil\n"
              "\n#+begin_example\n"
              "$ echo nothing to see here\n"
              "#+end_example\n")
      (org-mode)
      (goto-char (point-min))
      (setq opts (org-mime-get-export-options t))
      (should opts)
      ;; default: org-mode file
      (mapcar (lambda (backend)
                (setq org-mime-export-ascii backend)
                (switch-to-buffer orgBuf)
                (org-mime-org-buffer-htmlize)
                (switch-to-buffer (car (message-buffers)))
                (setq str (buffer-string))
                (should (string-match "<#multipart" str))
                (should (string-match "#\\+begin_example" str)))
                '(nil bogus))
      ;; 'ascii, 'latin1, and 'utf-8 exports
      (mapcar (lambda (backend)
                (setq org-mime-export-ascii backend)
                (switch-to-buffer orgBuf)
                (org-mime-org-buffer-htmlize)
                (switch-to-buffer (car (message-buffers)))
                (setq str (buffer-string))
                (should (string-match "<#multipart" str))
                (should-not (string-match "#\\+begin_example" str)))
                '(ascii latin1 utf-8)))
    (kill-buffer orgBuf)))

(ert-run-tests-batch-and-exit)
