;;; uguisu.el --- chat -*- lexical-binding: t -*-

;;; Code:

(require 'json)
(require 'request)

(defvar uguisu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'uguisu-read-print)
    (define-key map "\r" 'uguisu-ret-or-read)
    map))

(define-derived-mode uguisu-mode text-mode "Uguisu"
  "Major mode for running the Uguisu program using ChatGPT."
  (turn-on-auto-fill))

;;;###autoload
(defun uguisu ()
  "Open *uguisu* buffer."
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
  "Read input text and request it for chatgpt."
  (interactive)
  (let ((cur-pos (1- (point)))
	(prompt))
    (save-excursion
      (re-search-backward "^$" nil t)
      (setq prompt (buffer-substring-no-properties (+ 2 (point)) cur-pos))
      (unless (string-equal prompt "\n")
	(get-response prompt 'extract-content)))))

(defun get-response (prompt callback)
  "Generate a response from OpenAI API based on the given prompt."
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (params `(("model" . "gpt-3.5-turbo")
                   ("messages" . [(("role" . "user") ("content" . ,prompt))])
		   ("stream" . t)))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " chatgpt-api-key)))))
    (request
      url
      :type "POST"
      :data (json-encode params)
      :headers headers
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (funcall callback data))))))

(defun extract-content (response)
  "Extract content part from response."
  (let ((jsons (delete "" (split-string response "\n\n")))
	(messages '("\n")))
    (dotimes (i (list-length jsons))
      (setq json (substring (pop jsons) 6))
      (unless (string-equal json "[DONE]")
	(setq delta (cdr (assoc 'delta (elt (cdr (assoc 'choices (json-read-from-string json))) 0))))
	(when (assoc 'content delta)
          (push (cdr (assoc 'content delta)) messages))))
    (push "\n\n\n" messages)
    (print-message (reverse messages))))

(defun print-message (messages)
  "Print the given message to the *uguisu* buffer."
  (goto-char (point-max))
  (insert-with-timer messages))

(defun insert-with-timer (messages)
  (let ((len (list-length messages)))
    (dotimes (i len)
      (run-with-timer (* i 0.1) nil
		      (lambda ()
                        (insert (pop messages)))))))

(provide 'uguisu)

;;; uguisu.el ends here
