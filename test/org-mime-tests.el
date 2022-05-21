;; org-mime-tests.el --- unit tests for org-mime -*- lexical-binding: t; -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'ert)
(require 'org-mime)
(require 'message)

(defconst org-mime--mail-header '("To: myname@mail.com\n"
                       "Subject: test subject\n"
                       "From: My Name <myname@yahoo.com>\n"
                       "--text follows this line--\n"))
(defconst org-mime--mail-footer '("--\n"
                        "Yous somebody\n\n"
                        "--\n"
                        "Some quote\n"))

(defun run-org-mime-htmlize (&rest mail-body)
  "Create mail containing MAIL-BODY and run `org-mime-htmlize'."
  (with-temp-buffer
    (apply #'insert org-mime--mail-header)
    (apply #'insert mail-body)
    (apply #'insert org-mime--mail-footer)
    (message-mode)
    (goto-char (point-min))
    (org-mime-htmlize)
    (buffer-string)))

(ert-deftest test-org-mime-htmlize ()
  (let* ((str (run-org-mime-htmlize "* hello\n"
                          "** world\n"
                          "#+begin_src javascript\n"
                          "console.log('hello world');\n"
                          "#+end_src\n")))
    (should (string-match "<#multipart" str))))

(ert-deftest test-org-mime-export-options ()
  (let* (str)
    (setq org-mime-export-options '(:with-toc t))
    (setq str (run-org-mime-htmlize "* hello\n"))
    (should (string-match "Table of Contents" str))
    (setq org-mime-export-options '(:with-toc nil))
    (setq str (run-org-mime-htmlize "* hello\n"))
    (should (not (string-match "Table of Contents" str)))))

(ert-deftest test-org-mime-org-subtree-htmlize ()
  (let* (str)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+begin_src javascript\n"
              "console.log('hello world');\n"
              "#+end_src\n")
      (org-mode)
      (goto-char (point-min))
      (should (org-mime-get-export-options t))
      (org-mime-org-subtree-htmlize)
      (set-buffer (car (message-buffers)))
      (setq str (buffer-string)))
    (should (string-match "Subject: hello" str))
    (should (string-match "<#multipart" str))))

(ert-deftest test-org-mime-org-buffer-htmlize ()
  (let* (str props)
    (with-temp-buffer
      (insert
       "#+PROPERTY: MAIL_SUBJECT My mail subject\n"
       "#+PROPERTY: MAIL_TO Someone <someone@somewhere.tld>\n"
       "#+PROPERTY: MAIL_FROM Me <me@mine.tld>\n"
       "#+PROPERTY: MAIL_CC ccme@somewhere.tld\n"
       "#+PROPERTY: MAIL_BCC bccme@mine.tld\n"
       "* hello\n"
       "* hello\n"
       "** world\n"
       "#+begin_src javascript\n"
       "console.log('hello world');\n"
       "#+end_src\n")
      (org-mode)
      (goto-char (point-min))
      (setq props (org-mime-buffer-properties))
      (should (org-mime-get-export-options t))
      (org-mime-org-buffer-htmlize)
      (set-buffer (car (message-buffers)))
      (setq str (buffer-string)))
    (should (string= "My mail subject" (plist-get props :MAIL_SUBJECT)))
    (should (string= "Someone <someone@somewhere.tld>" (plist-get props :MAIL_TO)))
    (should (string= "Me <me@mine.tld>" (plist-get props :MAIL_FROM)))
    (should (string= "ccme@somewhere.tld" (plist-get props :MAIL_CC)))
    (should (string= "bccme@mine.tld" (plist-get props :MAIL_BCC)))
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
  (let (str
        (orgBuf (generate-new-buffer "*org-mode-test-buf*")))

    (with-current-buffer orgBuf
      (insert "#+OPTIONS: toc:nil num:nil\n"
              "\n#+begin_example\n"
              "$ echo nothing to see here\n"
              "#+end_example\n")
      (org-mode)
      (goto-char (point-min))
      (should (org-mime-get-export-options t))
      (dolist (backend '(nil bogus ascii latin1 utf-8))
        (setq org-mime-export-ascii backend)
        (set-buffer orgBuf)
        (org-mime-org-buffer-htmlize)
        (set-buffer (car (message-buffers)))
        (setq str (buffer-string))
        (should (string-match "<#multipart" str))
        (if (car (memq backend '(ascii latin1 utf-8)))
            (should-not (string-match "#\\+begin_example" str))
          (should (string-match "#\\+begin_example" str)))))
    (kill-buffer orgBuf)))

(ert-deftest test-org-mime-htmlize-ascii-plain-text ()
  (let (str)
    (mapcar (lambda (backend)
              (setq org-mime-export-ascii backend)
              (with-temp-buffer
                (apply #'insert org-mime--mail-header)
                (insert "#+OPTIONS: toc:nil num:nil\n"
                        "\n#+begin_example\n"
                        "$ echo nothing to see here\n"
                        "#+end_example\n")
                (apply #'insert org-mime--mail-footer)
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
  (let (str orgBuf)
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
      (set-buffer orgBuf)
      ;; export subtree for Section 2
      (goto-char (point-min))
      (search-forward "Section 2")
      (goto-char (+ 1 (point)))
      (org-mime-org-subtree-htmlize)
      (set-buffer (car (message-buffers)))
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
  (let (str orgBuf)
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
      (set-buffer orgBuf)
      ;; export subtree for Section 2
      (goto-char (point-min))
      (search-forward "Section 2")
      (goto-char (+ 1 (point)))
      (org-mime-org-subtree-htmlize)
      (set-buffer (car (message-buffers)))
      (setq str (buffer-string))
      (setq case-fold-search nil) ;; match case for string-match
      (should-not (string-match "#\\+begin_example" str))
      (should (string-match "<#multipart" str))
      (should (string-match "Subject: Section 2" str))
      (should-not (string-match "Alfred E. Neuman" str))
      (should-not (string-match "Table of Contents" str))
      (should-not (string-match "SECTION_ONE" str)))
    (kill-buffer orgBuf)))

(ert-deftest test-org-mime-beautify-quoted-para-breaks ()
  (let (html expected)
    (setq html (concat "<p>\n"
                       "Hello there\n"
                       "</p>\n"
                       "\n"
                       "<p>\n"
                       "&gt; this is a long-ish para that is broken\n"
                       "&gt; on two lines\n"
                       "&gt;\n"
                       "&gt; followed by a single-line para\n"
                       "</p>\n"))
    (setq expected (concat "<p>\n"
                           "Hello there\n"
                           "</p>\n"
                           "\n"
                           "<p>\n"
                           "<blockquote class=\"gmail_quote\" style=\"margin:0 0 0 .8ex;border-left:1px #ccc solid;padding-left:1ex\">\n"
                           "\n"
                           "<div>this is a long-ish para that is broken\n"
                           "on two lines\n"
                           "</div>\n"
                           "<div>\n"
                           "<br /></div>\n"
                           "<div>followed by a single-line para\n"
                           "\n"
                           "</div></blockquote>\n"
                           "</p>\n"))
    (should (equal (org-mime-beautify-quoted html)
                   expected))))


(ert-deftest test-org-mime-extract-non-org ()
  (let* ((content (concat "*hello world\n"
                          "<#part type=\"application/pdf\" filename=\"1.pdl\" disposition=attachment>\n<#/part>\n"
                          "<#secure method=pgpmime mode=sign>\n"))
         tags)

    (with-temp-buffer
      (insert content)
      (message-mode)
      (setq tags (org-mime-extract-non-org)))
    (should (string= (nth 0 (plist-get tags :part-tags))
                     "<#part type=\"application/pdf\" filename=\"1.pdl\" disposition=attachment>\n<#/part>"))

    (should (string= (nth 0 (plist-get tags :secure-tags))
                     "<#secure method=pgpmime mode=sign>"))))

(ert-deftest test-org-mime-revert-to-plain-text ()
  (with-temp-buffer
    (insert
     "--text follows this line--"
     "<#multipart type=alternative>\n"
     "<#part type=text/plain>\n"
     "test\n"
     "hello\n"
     "<#part type=text/html>\n"
     "<p>\n"
     "test\n"
     "hello\n"
     "</p>\n"
     "<#/multipart>\n")
    (message-mode)
    (org-mime-revert-to-plain-text-mail)
    (should (string= (string-trim (buffer-string))
                     "--text follows this line--\ntest\nhello"))))
(ert-run-tests-batch-and-exit)
