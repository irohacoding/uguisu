;;; uguisu.el --- A chat program with AI -*- lexical-binding: t -*-

;; Copyright (C) 2023 IrohaCoding

;; Author: IrohaCoding <info@irohacoding.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/irohacoding/uguisu

;; This file is not part of GNU Emacs, but is distributed under
;; the same terms.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either verion 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x uguisu and write message, and then by pressing the RET key twice,
;; to submit your message. After waiting for a while, you will receive
;; a reply.

;;; Code:

(require 'json)

(defvar uguisu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'uguisu-read-print)
    (define-key map "\r" 'uguisu-ret-or-read)
    map))

(define-derived-mode uguisu-mode text-mode "Uguisu"
  "Major mode for running the Uguisu program."
  (turn-on-auto-fill))

;;;###autoload
(defun uguisu ()
  "Open *uguisu* buffer and insert greeting message."
  (interactive)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*uguisu*"))
  (unless (eq major-mode 'uguisu-mode)
    (uguisu-mode)
    (insert (concat "Hello! this is uguisu, powered by OpenAI API.\n"
                    "Write message below  line and type RET twice.\n\n\n"))))

(defun uguisu-ret-or-read (arg)
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (uguisu-read-print)
    (newline arg)))

(defun uguisu-read-print ()
  "Read input text and request it for OpenAI API."
  (interactive)
  (let ((cur-pos (1- (point)))
        (prompt))
    (save-excursion
      (re-search-backward "^$" nil t)
      (setq prompt (buffer-substring-no-properties (+ 2 (point)) cur-pos))
      (unless (string-equal prompt "\n")
        (send-request prompt)))))

(defun send-request (prompt)
  "Sends a request to the OpenAI API."
  (let ((proc (start-process "openai-api-connection" nil
                             "curl" "https://api.openai.com/v1/chat/completions"
                             "-H" "Content-Type: application/json"
                             "-H" "Accept: text/event-stream"
                             "-H" (format "Authorization: Bearer %s" openai-api-key)
                             "-d" (json-encode `(("model" . "gpt-3.5-turbo")
                                                 ("messages" . [(("role" . "user") ("content" . ,prompt))])
                                                 ("stream" . t))))))
    (set-process-filter-multibyte proc t)
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'process-filter)
    (set-process-sentinel proc #'process-sentinel)))

(defun process-filter (process content)
  "Print response in *uguisu* buffer."
  (let ((data (delete "" (split-string content "\n\n"))))
    (dotimes (i (list-length data))
      (setq json (substring (pop data) 6))
      (if (string-equal json "[DONE]")
          (insert "\n\n\n")
        (setq delta (cdr (assoc 'delta (elt (cdr (assoc 'choices (json-read-from-string json))) 0))))
        (cond ((assoc 'role delta)
               (insert "\n"))
              ((assoc 'content delta)
               (insert (cdr (assoc 'content delta)))))))))

(defun process-sentinel (process event)
  "Message process event."
  (princ (substring event 0 (1- (length event)))))

(provide 'uguisu)

;;; uguisu.el ends here
