;;; uguisu.el --- A chat program with AI -*- lexical-binding: t -*-

;; Copyright (C) 2023 IrohaCoding

;; Author: IrohaCoding <info@irohacoding.com>
;; Version: 0.2.6
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

(defgroup uguisu nil
  "Uguisu options."
  :group 'convenience)

(defcustom uguisu-ai-model "gpt-3.5-turbo"
  "AI model for uguisu."
  :type '(choice (const :tag "gpt-3.5-turbo" "gpt-3.5-turbo")
                 (const :tag "gpt-4" "gpt-4"))
  :group 'uguisu)

(defvar uguisu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'uguisu-read-print)
    (define-key map "\r" 'uguisu-ret-or-read)
    map))

(defvar uguisu-buffer "*uguisu*"
  "Buffer name of this program.")

(define-derived-mode uguisu-mode text-mode "Uguisu"
  "Major mode for running the Uguisu program."
  (turn-on-auto-fill))

;;;###autoload
(defun uguisu ()
  "Open *uguisu* buffer and insert greeting message."
  (interactive)
  (if (get-buffer uguisu-buffer)
      (switch-to-buffer-other-window uguisu-buffer)
    (split-window-below)
    (other-window 1)
    (switch-to-buffer (get-buffer-create uguisu-buffer))
    (unless (eq major-mode 'uguisu-mode)
      (uguisu-mode)
      (insert (concat "Hello! this is uguisu, powered by OpenAI API.\n"
                      "Write message below  line and type RET twice.\n\n\n")))))

(defun uguisu-ret-or-read (arg)
  (interactive "*p")
  (when (eq major-mode 'uguisu-mode)
    (if (= (preceding-char) ?\n)
        (uguisu-read-print)
      (newline arg))))

(defun uguisu-read-print ()
  "Read input text and request it for OpenAI API."
  (interactive)
  (let ((cur-pos (point))
        (prompt ""))
    (save-excursion
      (if (not (re-search-backward "^$" nil t))
          (message "Uguisu: Form feed () is not found.")
        (setq prompt (buffer-substring-no-properties (+ 2 (point)) (1- cur-pos)))
        (unless (string-equal prompt "\n")
          (if (not (executable-find "curl"))
              (insert "curl command is not found.\n\n")
            (send-request prompt)))))))

(defun send-request (prompt)
  "Sends a request to the OpenAI API."
  (let ((proc (start-process "openai-api-connection" nil
                             "curl" "https://api.openai.com/v1/chat/completions"
                             "-H" "Content-Type: application/json"
                             "-H" "Accept: text/event-stream"
                             "-H" (format "Authorization: Bearer %s" (getenv "OPENAI_API_KEY"))
                             "-d" (json-encode `(("model" . ,uguisu-ai-model)
                                                 ("messages" . [(("role" . "user") ("content" . ,prompt))])
                                                 ("stream" . t))))))
    (set-process-filter-multibyte proc t)
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'process-filter)
    (set-process-sentinel proc #'process-sentinel)))

(defun process-filter (process content)
  "Print response in *uguisu* buffer."
  (if (not (string-match "^data: " content))
      (insert-error-message content)
    (dolist (data (delete "" (split-string content "\n\n")))
      (let ((data-body ""))
        (setq data-body (substring data 6))
        (if (string-equal data-body "[DONE]")
            (insert "\n\n\n")
          (let* ((json (json-read-from-string data-body))
                 (delta (cdr (assoc 'delta (elt (cdr (assoc 'choices json)) 0)))))
            (cond ((assoc 'role delta)
                   (insert "\n"))
                  ((assoc 'content delta)
                   (insert (cdr (assoc 'content delta)))))))))))

(defun insert-error-message (content)
  "Insert error message to uguisu buffer."
  (let ((json (json-read-from-string content)))
    (insert (format "\n[Error] %s\n\n\n"
                    (cdr (elt (cdr (assoc 'error json)) 0))))))

(defun process-sentinel (process event)
  "Message process event."
  (princ (substring event 0 (1- (length event)))))

(provide 'uguisu)

;;; uguisu.el ends here
