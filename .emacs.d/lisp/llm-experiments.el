(setq llm/refactor-directive "You are a code assistant.
Respond concisely, with code only, without any additional text, prompt or note.")
(setq llm/refactor-prompts '("Add elaborate comments."))
(defun llm/clear-markdown-code-tags (str)
	(replace-regexp-in-string "^```.*\n?" "" str))
(defun llm/refactor (bounds prompt)
  (interactive
   (list
    (cons (region-beginning) (region-end))
		(completing-read "Choose: " llm/refactor-prompts nil nil nil 'refactor-hist)))
	(llm/rewrite-and-replace bounds llm/refactor-directive prompt))
(defun llm/rewrite-and-replace (bounds directive prompt)
  (let ((gptel-model "gpt-4-turbo-preview"))
	  (gptel-request
			  (concat prompt "\n\n" (buffer-substring-no-properties (car bounds) (cdr bounds)))
		  :system directive
		  :buffer (current-buffer)
		  :context (cons (set-marker (make-marker) (car bounds))
									   (set-marker (make-marker) (cdr bounds)))
		  :callback
		  (lambda (response info)
			  (if (not response)
					  (message "ChatGPT response failed with: %s" (plist-get info :status))
				  (let* ((bounds (plist-get info :context))
							   (beg (car bounds))
							   (end (cdr bounds))
							   (buf (plist-get info :buffer)))
					  (with-current-buffer buf
						  (save-excursion
							  (goto-char beg)
							  (kill-region beg end)
							  (insert (llm/clear-markdown-code-tags response))
							  (set-marker beg nil)
							  (set-marker end nil)))))))))
