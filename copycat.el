;;; copycat.el --- Minimal Dired-based file tree with collapsing and file copying -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-subtree)
(require 'org)

(defgroup copycat nil
  "Dired-based file browser showing only allowed file extensions."
  :group 'files
  :prefix "copycat-")

(defcustom copycat-allowed-extensions '("py" "ts" "tsx" "js" "jsx" "yaml" "md" "org")
  "List of allowed file extensions displayed in the Dired buffer."
  :type '(repeat string)
  :group 'copycat)

(defcustom copycat-extension-to-lang
  '(("py"   . "python")
    ("js"   . "javascript")
    ("jsx"  . "javascript")
    ("ts"   . "typescript")
    ("tsx"  . "typescript")
    ("yaml" . "yaml")
    ("yml"  . "yaml")
    ("md"   . "markdown")
    ("org"  . "org"))
  "Alist mapping file extensions to Org Babel language names."
  :type '(alist :key-type string :value-type string)
  :group 'copycat)

(defcustom copycat-use-absolute-paths nil
  "If non-nil, insert absolute file paths in the Org buffer headings.
If nil, only the filename (no directories) is displayed."
  :type 'boolean
  :group 'copycat)

(defun copycat--detect-lang (ext)
  "Return an Org Babel language name for the file extension EXT.
Return an empty string if EXT is not found in `copycat-extension-to-lang`."
  (or (cdr (assoc ext copycat-extension-to-lang)) ""))

;;;###autoload
(defun copycat (dir)
  "Open a Dired buffer in DIR, showing only user-defined allowed files, with subtree toggling."
  (interactive "DSelect directory: ")
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (dired default-directory)
    ;; Hide dotfiles by default:
    (dired-omit-mode 1)
    (setq dired-omit-files "^\\.")
    ;; Enable `dired-subtree` for collapsing via <tab>:
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
                     (lang (copycat--detect-lang ext))
                     ;; Decide how we display the file path:
                     (file-heading (if copycat-use-absolute-paths
                                       (expand-file-name f)
                                     (file-name-nondirectory f))))
                (insert (format "* FILE: %s\n" file-heading))
                (if (string-empty-p lang)
                    (insert "#+BEGIN_SRC\n")  ;; No language => fallback
                  (insert (format "#+BEGIN_SRC %s\n" lang)))
                (insert file-contents)
                (insert "#+END_SRC\n\n")
                (setq contents (concat contents
                                       file-heading "\n"
                                       file-contents "\n")))))
          (goto-char (point-min)))
        (kill-new contents)
        (pop-to-buffer output-buf)
        (message "Copied %d file(s) to clipboard" (length files))))))

(provide 'copycat)
;;; copycat.el ends here
