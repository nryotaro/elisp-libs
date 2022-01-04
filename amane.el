(defvar amane-projects-file (concat (file-name-as-directory user-emacs-directory) "amane.el")
  "The path to a file that contains the settings of CMake projects.")

(defvar amane-projects nil
  "The association list that holds the structures of cmake projects.")

(defun amane-project-file-exist-p ()
  "Check wheter amane config file that contains the paths of CMakeLists.txts."
  (file-exists-p amane-projects-file))

(defun amane-init-amane-projects-file ()
  "Initialize amane-projects-file."
  (with-temp-file amane-projects-file
    (insert (format "%s"  '(setq amane-projects nil)))))

(defun amane-add-project (cmakelists project-root build-dir)
  "Add the root CMakeLists.txt of a project."
  (interactive "fCMakeLists.txt.\nDThe root directory of the proeject.\nDThe build directory.")
  (let ((filename (file-name-nondirectory cmakelists)))
    (unless (equal filename "CMakeLists.txt")	
      (throw 'anamae-invalid-cmakelists cmakelists))
    (add-to-list 'amane-projects (cons (file-truename cmakelists)
				       (list (list (cons :root (file-truename project-root))
						   (cons :build (file-truename build-dir))))))))

(defun amane-save-projects ()
  "Save amane-projects."
  (with-temp-file amane-projects-file
    (insert (amane-pretty-string `(setq amane-projects ,amane-projects)))))


(defun amane-delete-cmakelists (cmakelists)
  "Take the path of a CMakeLists.txt, and stop builng the project with amane."
  (assoc-delete-all cmakelists amane-projects))

(defun amane-delete-all-projects ()
  "Delete all projects."
  (setq amane-projects nil))

(defun amane-pretty-string (obj)
  "Return a pretty string representing obj."
  (let* ((strs nil)
	 (stream (lambda (x)
		 (setq strs (cons x strs)))))
    (pp obj stream)
    (apply #'string (reverse strs))))

(defun amane-load-amane-projects ()
  "Read amane-projects."
  (if (amane-project-file-exist-p)
      (load-file amane-projects-file)))

(defun amane-generate-makefile ()
  (interactive)
  (let ((project (assoc (file-truename (buffer-file-name)) amane-projects)))
    (message "%s" project)
    (message "%s" (buffer-file-name))
    (message "%s" (file-truename (buffer-file-name)))
    (if project
	(let ((cmakelists (car project))
	      (root-dir (cdr (assoc :root (cdr project))))
	      (build-dir (cdr (assoc :build (cdr project)))))
	  (message (concat "cd " build-dir " && cmake " root-dir))
	(minibuffer-message
	 (shell-command-to-string (concat "cd " build-dir " && cmake " root-dir))))
      (minibuffer-message (concat "This CMakeLists.txt is not in amane-projects. Run M-x amane-add-project." )))))

(defun amane ()
  (message "amane"))

;(defvar-local a "dog")
;(defvar-local foo "fo")

;(with-current-buffer "*scratch*"
;					;(defvar-local b "c")
;  (setq-local b "3")
					;  )
					;https://ayatakesi.github.io/emacs/24.5/elisp_html/Minibuffer-Completion.html

(provide 'amane)
