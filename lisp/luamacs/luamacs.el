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

;;
;; The following is used to hack support for interactive Lua functions
;;

(lua-eval "function def_command(name, fn)
   elf.def_lua_command(name,fn)
end")


(defun def_lua_command (name fn)
  (eval `(defun ,(intern name) ()
	   "This function is defined in Lua.
It was made interactive with `def_lua_command'"
           (interactive)
           (funcall ,fn))))

;;local-set-key is one of the cases that requires an interactive function
;;so we need to handle this case separately for now
(defun lua-local-set-key (key function)
  (eval `(local-set-key ,key (lambda () (interactive) (funcall ,function)))))
(setq lua.local_set_key 'lua-local-set-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; char-after was returning some kind of lisp-object
;; so this is a temp solution for the 'charland.lua' example
(setq lua.char_after '(lambda () (let ((x (char-after)))
				   (if (eq (type-of x)
					   'integer)
				       x
				     0))))

;;this is here to support the 'charland.lua' example
(setq red_face `(face (:foreground ,(format "#%02x%02x%02x" 255 0 0))))
(setq green_face `(face (:foreground ,(format "#%02x%02x%02x" 0 255 0))))


(provide 'luamacs)
