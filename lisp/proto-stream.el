;;; proto-stream.el --- negotiating TLS, STARTTLS and other connections
;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library is meant to provide the glue between modules that want
;; to establish a network connection to a server for protocols such as
;; IMAP, NNTP, SMTP and POP3.

;; The main problem is that there's more than a couple of interfaces
;; towards doing this.  You have normal, plain connections, which are
;; no trouble at all, but you also have TLS/SSL connections, and you
;; have STARTTLS.  Negotiating this for each protocol can be rather
;; tedious, so this library provides a single entry point, and hides
;; much of the ugliness.

;; Usage example:

;; (open-proto-stream
;;  "*nnimap*" buffer address port
;;  :type 'network
;;  :capability-command "1 CAPABILITY\r\n"
;;  :starttls-function
;;  (lambda (capabilities)
;;    (if (not (string-match "STARTTLS" capabilities))
;;        nil
;;      "1 STARTTLS\r\n")))

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tls)
(require 'starttls)
(require 'format-spec)

(defcustom proto-stream-always-use-starttls t
  "If non-nil, always try to upgrade network connections with STARTTLS."
  :version "24.1"
  :type 'boolean
  :group 'comm)

(declare-function gnutls-negotiate "gnutls"
		  (proc type &optional priority-string trustfiles keyfiles))

;;;###autoload
(defun open-proto-stream (name buffer host service &rest parameters)
  "Open a network stream to HOST.
The first four parameters have the same meaning as in
`open-network-stream'.  The function returns a list where the
first element is the stream, the second element is the greeting
the server replied with after connecting, and the third element
is a string representing the capabilities of the server (if any).

The PARAMETERS is a keyword list that can have the following
values:

:type -- either `network', `tls', `shell' or `starttls'.  If
omitted, the default is `network'.

:end-of-command -- a regexp saying what the end of a command is.
This defaults to \"\\n\".

:capability-command -- a string representing the command used to
query server for capabilities.  For instance, for IMAP this is
\"1 CAPABILITY\\r\\n\".

:starttls-function -- a function that takes one parameter, which
is the response to the capaibility command.  It should return nil
if it turns out that the server doesn't support STARTTLS, or the
command to switch on STARTTLS otherwise."
  (let ((type (or (cadr (memq :type parameters)) 'network)))
    (when (and (eq type 'starttls)
	       (fboundp 'open-gnutls-stream))
      (setq type 'network))
    (when (eq type 'ssl)
      (setq type 'tls))
    (destructuring-bind (stream greeting capabilities)
	(funcall (intern (format "proto-stream-open-%s" type) obarray)
		 name buffer host service parameters)
      (list (and stream
		 (memq (process-status stream)
		       '(open run))
		 stream)
	    greeting capabilities))))

(defun proto-stream-open-network (name buffer host service parameters)
  (let* ((start (with-current-buffer buffer (point)))
	 (stream (open-network-stream name buffer host service))
	 (capability-command (cadr (memq :capability-command parameters)))
	 (eoc (proto-stream-eoc parameters))
	 (greeting (proto-stream-get-response stream start eoc)))
    (if (not capability-command)
	(list stream greeting nil)
      (let* ((capabilities
	      (proto-stream-command stream capability-command eoc))
	     (starttls-command
	      (funcall (cadr (memq :starttls-function parameters))
		       capabilities)))
	(cond
	 ((or (not starttls-command)
	      (not proto-stream-always-use-starttls))
	  ;; If this server doesn't support STARTTLS, but we have
	  ;; requested it explicitly, then close the connection and
	  ;; return nil.
	  (if (eq (cadr (memq :type parameters)) 'starttls)
	      (progn
		(delete-process stream)
		nil)
	    ;; Otherwise, just return this plain network connection.
	    (list stream greeting capabilities)))
	 ((or (fboundp 'open-gnutls-stream)
	      (executable-find "gnutls-cli"))
	  (unless (fboundp 'open-gnutls-stream)
	    (delete-process stream)
	    (setq stream (starttls-open-stream name buffer host service))
	    (proto-stream-get-response stream start eoc))
	  (proto-stream-command stream starttls-command eoc)
	  (if (fboundp 'open-gnutls-stream)
	      (gnutls-negotiate stream nil)
	    (starttls-negotiate stream))
	  ;; Re-get the capabilities, since they may have changed
	  ;; after switching to TLS.
	  (list stream greeting
		(proto-stream-command stream capability-command eoc)))
	 ((eq (cadr (memq :type parameters)) 'starttls)
	  (delete-process stream)
	  nil)
	 (t
	  (list stream greeting capabilities)))))))

(defun proto-stream-command (stream command eoc)
  (let ((start (with-current-buffer buffer (point-max))))
    (process-send-string stream command)
    (proto-stream-get-response stream start eoc)))

(defun proto-stream-get-response (stream start end-of-command)
  (with-current-buffer (process-buffer stream)
    (save-excursion
      (goto-char start)
      (while (and (memq (process-status stream)
			'(open run))
		  (not (re-search-forward end-of-command nil t)))
	(accept-process-output stream 0 50)
	(goto-char start))
      (if (= start (point))
	  ;; The process died; return nil.
	  nil
	;; Return the data we got back.
	(buffer-substring start (point))))))

(defun proto-stream-open-tls (name buffer host service parameters)
  (with-current-buffer buffer
    (let ((start (point-max))
	  (stream
	   (funcall (if (fboundp 'open-gnutls-stream)
			'open-gnutls-stream
		      'open-tls-stream)
		    name buffer host service)))
      ;; If we're using tls.el, we have to delete the output from
      ;; openssl/gnutls-cli.
      (unless (fboundp 'open-gnutls-stream)
	(proto-stream-get-response
	 stream start (proto-stream-eoc parameters))
	(goto-char (point-min))
	(when (re-search-forward (proto-stream-eoc parameters) nil t)
	  (goto-char (match-beginning 0))
	  (delete-region (point-min) (line-beginning-position))))
      (proto-stream-capability-open start stream parameters))))

(defun proto-stream-open-shell (name buffer host service parameters)
  (proto-stream-capability-open
   (with-current-buffer buffer (point))
   (let ((process-connection-type nil))
     (start-process name buffer shell-file-name
		    shell-command-switch
		    (format-spec
		     (cadr (memq :shell-command parameters))
		     (format-spec-make
		      ?s host
		      ?p service))))
   parameters))

(defun proto-stream-capability-open (start stream parameters)
  (let ((capability-command (cadr (memq :capability-command parameters)))
	(greeting (proto-stream-get-response
		   stream start (proto-stream-eoc parameters))))
    (list stream greeting
	  (and capability-command
	       (proto-stream-command
		stream capability-command (proto-stream-eoc parameters))))))

(defun proto-stream-eoc (parameters)
  (or (cadr (memq :end-of-command parameters))
      "\r\n"))

(provide 'proto-stream)

;;; proto-stream.el ends here