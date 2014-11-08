
(require 'luamacs-tests)

(defun lua-eval-buffer ()
  "Evaluate the current buffer contents as lua code"
  ;;TODO: other options like `eval-buffer'
  (interactive)
  (lua-eval (buffer-string)))

(provide 'luamacs)
