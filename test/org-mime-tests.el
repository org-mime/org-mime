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
              "console.log('hello world');\n"
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
              "console.log('hello world');\n"
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
              "console.log('hello world');\n"
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

;;; The two ASCII export tests below check for org-mode markup for the default
;;; case, where the export variable is nil or not valid, and check for absent
;;; org-mode markup for the three valid plain text exports. The ASCII export
;;; tests do not attempt to verify the exported coding type.

(ert-deftest test-org-mime-org-buffer-htmlize-ascii-plain-text ()
  (let (str opts)
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
      (mapcar (lambda (backend)
                (setq org-mime-export-ascii backend)
                (switch-to-buffer orgBuf)
                (org-mime-org-buffer-htmlize)
                (switch-to-buffer (car (message-buffers)))
                (setq str (buffer-string))
                (should (string-match "<#multipart" str))
                (if (car (memq backend '(ascii latin1 utf-8)))
                    (should-not (string-match "#\\+begin_example" str))
                  (should (string-match "#\\+begin_example" str))))
                '(nil bogus ascii latin1 utf-8)))
    (kill-buffer orgBuf)))

(ert-deftest test-org-mime-htmlize-ascii-plain-text ()
  (let (str)
    (mapcar (lambda (backend)
              (setq org-mime-export-ascii backend)
              (with-temp-buffer
                (apply #'insert mail-header)
                (insert "#+OPTIONS: toc:nil num:nil\n"
                        "\n#+begin_example\n"
                        "$ echo nothing to see here\n"
                        "#+end_example\n")
                (apply #'insert mail-footer)
                (message-mode)
                (goto-char (point-min))
                (org-mime-htmlize)
                (setq str (buffer-string))
                (should (string-match "<#multipart" str))
                (if (car (memq backend '(ascii latin1 utf-8)))
                    (should-not (string-match "#\\+begin_example" str))
                  (should (string-match "#\\+begin_example" str)))))
            '(nil bogus ascii latin1 utf-8))))

;; The two ASCII export tests below check subtree export options for the utf-8
;; ascii export. In the first test we include the Title, Table of Contents, and
;; Author as defined in the subtree properties. In the second test, we omit the
;; Title, TOC, and Author.

(ert-deftest test-org-mime-org-subtree-htmlize-ascii-opts-t ()
  (let (str opts)
    (setq org-mime-export-options nil) ;; allow subtree properties
    (setq org-mime-export-ascii 'utf-8)
    (setq orgBuf (generate-new-buffer "*org-mode-test-buf*"))
    (with-current-buffer orgBuf
      ;; the initial options are ignored in favor of subtree options
      (insert "#+OPTIONS: toc:nil author:nil title:nil\n"
              "#+AUTHOR: Anon\n"
              "* Section 1\n"
              "SECTION_ONE\n"
              "* Section 2\n"
              ":PROPERTIES:\n"
              ":EXPORT_OPTIONS: toc:t author:t title:t\n"
              ":EXPORT_AUTHOR: Alfred E. Neuman:t\n"
              ":END:\n"
              "\n#+begin_example\n"
              "$ echo nothing to see here\n"
              "#+end_example\n")
      (org-mode)
      (switch-to-buffer orgBuf)
      ;; export subtree for Section 2
      (goto-char (point-min))
      (search-forward "Section 2")
      (goto-char (+ 1 (point)))
      (org-mime-org-subtree-htmlize)
      (switch-to-buffer (car (message-buffers)))
      (setq str (buffer-string))
      (setq case-fold-search nil) ;; match case for string-match
      (should-not (string-match "#\\+begin_example" str))
      (should (string-match "<#multipart" str))
      (should (string-match "Subject: Section 2" str))
      (should (string-match "SECTION 2" str))
      (should (string-match "Alfred E. Neuman" str))
      (should (string-match "Table of Contents" str))
      (should-not (string-match "SECTION_ONE" str)))
    (kill-buffer orgBuf)))

(ert-deftest test-org-mime-org-subtree-htmlize-ascii-opts-nil ()
  (let (str opts)
    (setq org-mime-export-options nil) ;; allow subtree properties
    (setq org-mime-export-ascii 'utf-8)
    (setq orgBuf (generate-new-buffer "*org-mode-test-buf*"))
    (with-current-buffer orgBuf
      ;; the initial options are ignored in favor of subtree options
      (insert "#+OPTIONS: toc:t author:t title:t\n"
              "#+AUTHOR: Anon\n"
              "* Section 1\n"
              "SECTION_ONE\n"
              "* Section 2\n"
              ":PROPERTIES:\n"
              ":EXPORT_OPTIONS: toc:nil author:nil title:nil\n"
              ":EXPORT_AUTHOR: Alfred E. Neuman:t\n"
              ":END:\n"
              "\n#+begin_example\n"
              "$ echo nothing to see here\n"
              "#+end_example\n")
      (org-mode)
      (switch-to-buffer orgBuf)
      ;; export subtree for Section 2
      (goto-char (point-min))
      (search-forward "Section 2")
      (goto-char (+ 1 (point)))
      (org-mime-org-subtree-htmlize)
      (switch-to-buffer (car (message-buffers)))
      (setq str (buffer-string))
      (setq case-fold-search nil) ;; match case for string-match
      (should-not (string-match "#\\+begin_example" str))
      (should (string-match "<#multipart" str))
      (should (string-match "Subject: Section 2" str))
      (should-not (string-match "Alfred E. Neuman" str))
      (should-not (string-match "Table of Contents" str))
      (should-not (string-match "SECTION_ONE" str)))
    (kill-buffer orgBuf)))

(ert-run-tests-batch-and-exit)
