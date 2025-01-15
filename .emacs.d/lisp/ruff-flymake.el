;;; ruff-flymake.el --- Flymake ruff backend  -*- lexical-binding: t -*-

;;; Commentary:

(require 'json)
(require 'project)

(defun ruff-make-file-process (&rest args)
  "Wrapper for `make-process', but optionally uses a file name handler.

Does for `make-process' what `start-file-process' does for `start-process'."
  (let ((fh (find-file-name-handler default-directory 'make-process)))
    (if fh (apply fh 'make-process args)
      (apply 'make-process args))))

(defun ruff-location-to-pos (l)
  "Convert Ruff's location L (alist with \"row\" and \"column\") to Emacs POS."

  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (let-alist l .row)))
    (move-to-column (1- (let-alist l .column)))
    (point)))

(defun ruff-to-flymake-diagnostic (ruff-result)
  "Convert a single Ruff row RUFF-RESULT to a Flymake diagnostic."
	(let ((start_pos (ruff-location-to-pos (let-alist ruff-result .location)))
				(end_pos (ruff-location-to-pos (let-alist ruff-result .end_location))))
		(flymake-make-diagnostic (current-buffer)
														 start_pos
														 end_pos
														 :error
														 (format "%s: %s"
																		 (let-alist ruff-result .code)
																		 (let-alist ruff-result .message)))))

(defun ruff-report-result-buffer (buffer report-fn)
  "Handle ruff result in BUFFER.

Parses BUFFER as JSON, converts each row using `ruff-to-flymake-diagnostic',
then reports them by calling REPORT-FN."
		(funcall report-fn
						 (mapcar #'ruff-to-flymake-diagnostic
										(with-current-buffer buffer (json-read-from-string (buffer-string))))))

(defun ruff-flymake-backend (report-fn &rest _)
  "Ruff backend for flymake."
	(unless (buffer-modified-p)
		(let ((ruff-buffer (generate-new-buffer "*ruff*"))
					(default-directory (project-root (project-current t)))
					(file-name (file-local-name (buffer-file-name))))
			(ruff-make-file-process
			 :name "ruff"
			 :buffer ruff-buffer
			 :command (list "ruff" "check" "--output-format" "json" file-name)
			 :stderr (get-buffer-create "*ruff-stderr*")
			 :sentinel (lambda (process _event)
									 (unless (process-live-p process)
										 (unwind-protect
												 (ruff-report-result-buffer ruff-buffer report-fn)
											 (kill-buffer ruff-buffer))))))))

(define-minor-mode ruff-flymake-mode
  "Automatically format files with `ruff` on save."
  :lighter " RuffFM"
  (if ruff-flymake-mode
      (add-hook 'flymake-diagnostic-functions #'ruff-flymake-backend nil t)
    (remove-hook 'flymake-diagnostic-functions #'ruff-flymake-backend t)))

(defun ruff-format-buffer ()
  "Format the current buffer with `ruff`."
  (interactive)
  (when (buffer-file-name)
     (save-buffer)
     (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
       (shell-command-to-string (format "ruff format %s" (shell-quote-argument (buffer-file-name)))))
       (shell-command-to-string (format "ruff check --fix %s" (shell-quote-argument (buffer-file-name))))
     (revert-buffer t t t)))

(define-minor-mode ruff-format-on-save-mode
  "Automatically format files with `ruff` on save."
  :lighter " RuffFmt"
  (if ruff-format-on-save-mode
      (add-hook 'after-save-hook #'ruff-format-buffer -10 t)
    (remove-hook 'after-save-hook #'ruff-format-buffer t)))

(provide 'ruff-flymake)

;;; ruff-flymake.el ends here
