(lua-init)

;;FIX: this creates problems when loading from loadup.el
;;(require 'luamacs-tests)


(defvar lua-version "5.2.3")

(defvar luamacs-version (concat "5.2.3::" emacs-version)
  "lua version :: emacs version")
  
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

(setq lua.hash_table_to_list
      (lambda (ht)
        (let (keys vals)
          (maphash (lambda (k v)
                     (setq keys (cons k keys))
		     (setq vals (cons v vals)))
                   ht)
          (list keys vals))))
                           
(lua-eval "function lisp_hash_metatable.__pairs (tbl)
   local kv = hash_table_to_list(tbl);
   local keys = kv[0]
   local vals = kv[1]
   local len = #kv
   local i = 0;
   return function ()
      local ii
      while i < len do
         ii = i
         i = i + 1
         return keys[ii], vals[ii]
      end
   end
end")

(provide 'luamacs)
