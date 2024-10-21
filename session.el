
(setq-default sessions-path "~/pallas-emacs/session/")
(setq-default sessions-name "animaker.session")

(defun sessions-dir ()
 (concat sessions-path "/" sessions-name))


(defun sessions-list ()
 (seq-remove (lambda (x) (or (string= x ".") (string= x ".."))) (directory-files sessions-path)))

(defun sessions ()
 (interactive)
 (message "Available sessions: %s" (sessions-list)))


(defun sessions-change-dir(path)
  "Change Sessions Storage Directory "
  (interactive "Session Path")
  (setq session-path 'path)
  )


(defun sessions-set-name (name)
 (interactive
  (list
   (completing-read "Enter session name: " (sessions-list))))
 (setq sessions-name name))


(defun sessions-name ()
 (interactive)
 (message "name: %s" sessions-name))


(defun sessions-save ()
 (interactive)
 (when (null sessions-name)
  (call-interactively 'sessions-set-name))
 (let ((dir (sessions-dir)))
  (mkdir dir t)
  (desktop-save dir))
 (sessions-name))


(defun sessions-close ()
 (interactive)
 (when (and sessions-name
        (y-or-n-p (concat "Save current session '" sessions-name "'?")))
  (call-interactively 'sessions-save)))

(defun sessions-open-at-start ()
 (interactive)
 (desktop-change-dir (sessions-dir)))


(defun sessions-open ()
 (interactive)
 (when sessions-name
  (call-interactively 'sessions-close))
 (call-interactively 'sessions-set-name)
 (desktop-change-dir (sessions-dir)))

(provide 'sessions)
