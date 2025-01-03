;;; copycat.el --- Dired-based file tree with allowed extensions and collapsing -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-subtree)
(require 'all-the-icons-dired)
(require 'org)

(defgroup copycat nil
  "Dired-based file browser showing only allowed file extensions."
  :group 'files
  :prefix "copycat-")

(defcustom copycat-allowed-extensions '("py" "ts" "tsx" "js" "jsx" "yaml" "md" "org")
  "List of allowed file extensions."
  :type '(repeat string)
  :group 'copycat)

(defcustom copycat-extension-to-lang
  '(("py"   . "python")
    ("js"   . "javascript")
    ("jsx"  . "javascript")
    ("ts"   . "typescript")
    ("tsx"  . "typescript")
    ("yaml" . "yaml")
    ("yml"  . "yaml")  ;; In case you'd like .yml too
    ("md"   . "markdown")
    ("org"  . "org"))
  "Alist mapping file extensions to Org Babel language names."
  :type '(alist :key-type string :value-type string)
  :group 'copycat)

;; Store the original icons functions for fallback.
(defvar copycat--orig-icon-for-dir (symbol-function 'all-the-icons-icon-for-dir))
(defvar copycat--orig-icon-for-file (symbol-function 'all-the-icons-icon-for-file))

(defun copycat--icon-for-dir (_dir &rest _)
  "Always return the YAML icon for directories."
  (all-the-icons-icon-for-file "foo.yaml"))

(defun copycat--icon-for-file (file &rest args)
  "Return directory icon for Python files; otherwise fallback to the original icon function."
  (let ((ext (file-name-extension file)))
    (if (and ext (string= ext "py"))
        ;; Use directory icon for Python files
        (funcall copycat--orig-icon-for-dir "." :height 0.9 :v-adjust 0.0)
      (apply copycat--orig-icon-for-file file args))))

(advice-add 'all-the-icons-icon-for-dir :override #'copycat--icon-for-dir)
(advice-add 'all-the-icons-icon-for-file :around  #'copycat--icon-for-file)

(defun copycat--detect-lang (ext)
  "Return an Org Babel language name for file extension EXT.
Return an empty string if EXT is not found in `copycat-extension-to-lang`."
  (or (cdr (assoc ext copycat-extension-to-lang)) ""))

;;;###autoload
(defun copycat (dir)
  "Open a Dired buffer in DIR, showing only allowed files and allowing subtree toggling."
  (interactive "DSelect directory: ")
  (let* ((default-directory (file-name-as-directory (expand-file-name dir))))
    (dired default-directory)
    (all-the-icons-dired-mode 1)
    (dired-omit-mode 1)
    ;; Hide dotfiles by default
    (setq dired-omit-files "^\\.")
    ;; Enable dired-subtree for collapsing via <tab>.
    (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)))

;;;###autoload
(defun copycat-copy-marked ()
  "Copy the contents of marked files to the kill-ring and show them in an Org buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (null files)
        (message "No files marked")
      (let ((contents "")
            (output-buf (get-buffer-create "*copycat-output*")))
        (with-current-buffer output-buf
          (erase-buffer)
          (org-mode)
          (dolist (f files)
            (when (and (file-readable-p f)
                       (not (file-directory-p f)))
              (let* ((file-contents (with-temp-buffer
                                      (insert-file-contents-literally f)
                                      (replace-regexp-in-string "\r" "" (buffer-string))))
                     (ext  (file-name-extension f))
                     (lang (copycat--detect-lang ext)))
                (insert (format "* FILE: %s\n" (file-name-nondirectory f)))
                (if (string= lang "")
                    (insert "#+BEGIN_SRC\n")  ;; No language => fallback
                  (insert (format "#+BEGIN_SRC %s\n" lang)))
                (insert file-contents)
                (insert "#+END_SRC\n\n")
                (setq contents (concat contents
                                       (file-name-nondirectory f) "\n"
                                       file-contents "\n")))))
          (goto-char (point-min)))
        (kill-new contents)
        (pop-to-buffer output-buf)
        (message "Copied %d file(s) to clipboard" (length files))))))

(provide 'copycat)
;;; copycat.el ends here
