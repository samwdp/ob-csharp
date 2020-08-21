;;; ob-cshrp.el --- org-babel functions for csharp evaluation

;;; Commentary:
;;
;; org-babel functions for rust evaluation
;;

;;; Code:
(require 'ob)

(defun org-babel-execute:csharp (body params))
(let (src-temp (org-babel-temp-file "csharp-"))
    (with-temp-file src-temp (insert (body)))
    (shell-command-to-string (format "dotnet script %s" src-temp)))

(provide 'ob-csharp)
;;; ob-csharp.el ends here
