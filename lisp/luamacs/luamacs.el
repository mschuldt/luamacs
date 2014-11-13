
(require 'luamacs-tests)

(defvar lua-version "5.2")

(defun lua-eval-buffer ()
  "Evaluate the current buffer contents as lua code"
  ;;TODO: other options like `eval-buffer'
  (interactive)
  (lua-eval (buffer-string)))

(lua-eval "function lisp_cons_metatable.__pairs (tbl)
   local len = #tbl
   local i = 0;
   return function ()
      local ii
      while i < len do
         ii = i
         i = i + 1
         return ii, tbl[ii]
      end
   end
end")

(lua-eval "function lisp_vector_metatable.__pairs (tbl)
   local len = #tbl
   local i = 0;
   return function ()
      local ii
      while i < len do
         ii = i
         i = i + 1
         return ii, tbl[ii]
      end
   end
end")

(provide 'luamacs)
