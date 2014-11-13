(require 'ert)

(defun luamacs-run-tests ()
  (interactive)
  (ert-run-tests-interactively "luamacs")
  )

(ert-deftest luamacs-lisp-var-from-lua ()
  "test accessing and setting lisp values from lua"
  ;;strings
  (progn (setq _x "val")
         (lua-eval "_v = (emacs._x == 'val')")
         (should lua._v))
  (progn (lua-eval "emacs._x = 'fromlua'")
         (should (equal _x "fromlua")))
  ;;numbers
  (progn (setq _x 33)
         (lua-eval "_v = (emacs._x == 33)")
         (should lua._v))
  (progn (lua-eval "emacs._x = 100")
         (should (equal _x 100.0)))

  (let ((ss (lua-stacksize)))
    (dotimes (i (* (lua-stacksize) 2))
      (lua-eval "_v = emacs._x")
      lua._v
      (should (= ss (lua-stacksize)))))
  )

(ert-deftest luamacs-lua-var-from-lisp ()
  "test accessing and setting lua values from lisp"
  ;;strings
  (progn (lua-eval "_x = 'val'")
         (should (equal lua._x "val")))
  (progn (setq lua._x "new")
         (should (equal lua._x "new")))
  ;;numbers
  (progn (lua-eval "_x = 33")
         (should (equal lua._x 33.0)))
  (progn (setq lua._x 100)
         (should (equal lua._x 100.0)))
  
  ;;TODO: test stacksize
  )

;;accessing/calling/setting functions
(ert-deftest luamacs-lua-func-from-lisp ()
  "test calling lua functions from lisp"
  (progn (lua-eval "function _f() return 11 end")
         (should (equal (lua._f) 11.0)))
  (progn (lua-eval "function _f(a) return a .. '!' end")
         (should (equal (lua._f "s") "s!")))
  (progn (lua-eval "function _f(a, b) return a .. b .. '!' end")
         (should (equal (lua._f "x" "y") "xy!")))

  ;;test calling with funcall
  (progn (lua-eval "function _f() return 11 end")
         (setq _ff lua._f)
         (should (equal (funcall _ff) 11.0)))
  (progn (lua-eval "function _f(a) return a .. '!' end")
         (setq _ff lua._f)
         (should (equal (funcall _ff "s") "s!")))
  (progn (lua-eval "function _f(a, b) return a .. b .. '!' end")
         (setq _ff lua._f)
         (should (equal (funcall _ff "x" "y") "xy!")))
  (progn (lua-eval "function _f(a, b,c) return a .. b .. c  ..'!' end")
         (setq _ff lua._f)
         (should (equal (funcall _ff "x" "y" "z") "xyz!")))

  (let ((ss (lua-stacksize)))
    (lua-eval "function _f(a, b) return a .. b .. '!' end")
    (dotimes (i (* (lua-stacksize) 2)
                (setq _ff lua._f)
                (funcall _ff "x" "y"))
      (should (= ss (lua-stacksize)))))
  )


(ert-deftest luamacs-lisp-func-from-lua ()
  "test calling lisp functions from lua"
  (progn
    (lua-eval "_v = elf.concat()")
    (should (equal lua._v "")))
  (progn
    (lua-eval "_v = elf.concat('a')")
    (should (equal lua._v "a")))
  (progn
    (lua-eval "_v = elf.concat('a', 'b')")
    (should (equal lua._v "ab")))
  (progn
    (lua-eval "_f = elf.concat
               _v0 = _f()
               _v1 = _f('a')
               _v2 = _f('b', 'a')")
    (should (and (equal lua._v0 "")
                 (equal lua._v1 "a")
                 (equal lua._v2 "ba"))))

  ;;TODO: test setting functions from lua
  
  (let ((ss (lua-stacksize)))
    (lua-eval "function f(a,b) return a .. b end")
    (dotimes (i (* (lua-stacksize) 2))
      (lua.f "x" "y")
      (lua-eval "v0 = elf.concat('a', 'a')"))
    (should (= ss (lua-stacksize))))
  )

(ert-deftest luamacs-lisp-v-func-from-lua ()
  "tests calling variable bound lisp functions from lua"
  (progn 
    (setq _f0 (lambda () "ok"))
    (lua-eval "_v0 = emacs._f0()")
    (should (equal lua._v0 "ok")))
  (progn
    (setq _f1 (lambda (a) (concat a "!")))
    (lua-eval "_v1 = emacs._f1('x')")
    (should (equal lua._v1 "x!")))
  (progn
    (setq _f2 (lambda (a b) (concat a b "!")))
    (lua-eval "_v2 = emacs._f2('x', 'y')")
    (should (equal lua._v2 "xy!")))
  
  ;;TODO: test stacksize
  )


(ert-deftest luamacs-lisp-gc ()
  "Tests that lisp objects referenced from lua will not be garbage collected"
  (progn (setq _l '(1 2))
         (lua-eval "_ll = emacs._l")
         (setq _l nil)
         (garbage-collect)
         (should (equal lua._ll '(1 2)))))

(ert-deftest luamacs-lua-gc ()
  "Tests that lua objects referenced from lisp will not be garbage collected"
  (progn (lua-eval "_gctest = {x = 11}")
         (setq _t lua._gctest)
         (lua-eval "_gctest = nil")
         (lua-garbage-collect)
         (should (equal (lua-get _t "x") 11.0))))

;;TODO: test that objects that are no longer referenced get freed

(ert-deftest luamacs-lua-multiple-refs-sameness ()
  "Tests that multiple references in lisp to the same lua objects are the same"
  (progn (lua-eval "_t = {x = 1}")
         (setq _a lua._t)
         (setq _b lua._t)
         (should (eq _a _b))))

(ert-deftest luamacs-lisp-multiple-refs-sameness ()
  "Tests that multiple references in lua to the same lisp objects are the same"
  (progn (setq _v '(a b))
         (lua-eval "_a = emacs_v")
         (lua-eval "_b = emacs_v")
         (lua-eval "_v = (_a == _b)")
         (should lua._v)))

(ert-deftest luamacs-tables ()
  "test table manipulation from emacs"
  (progn ;;new-table, set, get
    (setq _t (lua-new-table))
    (lua-set _t "a" 3)
    (should (= (lua-get _t "a") 3)))
  
  (progn ;;setmetatable
    (setq _t (lua-new-table))
    (setq _mt (lua-new-table))
    (lua-set _mt "__index" _mt)
    (lua-set _mt "a" 32)
    (lua-setmetatable  _t _mt)
    (should (= (lua-get _t "a") 32)))
  
  (progn ;;length as array
    (setq _a (lua-new-table))
    (setq n 10)
    (dotimes (i n)
      (lua-set _a i (* i i)))
    (should (equal (length _a) (- n 1))))
  )

(ert-deftest luamacs-alist-to-table ()
  "tests `alist-to-table'"  
  (progn
    (setq _al '(("k1" . 111) ("k2" . "two") ("k3" . 4.42)))
    (setq _t (alist-to-table _al))
    (should (and (equal (lua-get _t "k1") 111.0)
                 (equal (lua-get _t "k2") "two")
                 (equal (lua-get _t "k3") 4.42))))

  (progn
    (setq _n 10)
    (setq _t (alist-to-table (mapcar (lambda (n)
                                       (cons n (number-to-string n)))
                                     (number-sequence 0 _n))))
    (should (equal (length _t) _n))
    (should (equal (lua-get _t 4) "4")))
  )

(ert-deftest luamacs-sequence-to-table ()
  "tests `sequence-to-table'"
  (progn ;;list to table
    (setq _t (sequence-to-table (mapcar (lambda (n) (* n n))
					(number-sequence 1 10))))
    (should (and (equal (length _t) 10)
                 (equal (lua-get _t 4) 16.0))))
  (progn ;; vector to table
    (setq _t (sequence-to-table (vconcat (mapcar (lambda (n) (* n n ))
                                                 (number-sequence 1 10)))))
    (should (and (equal (length _t) 10)
                 (equal (lua-get _t 4) 16.0))))
  )

(ert-deftest luamacs-metatable ()
  "tests that metatables and special fields work correctly"
  (progn ;;test lisp function for __index
    (setq _index (lambda (table name)
                   (format "indexed name = '%s'" name)))
    (lua-eval "_x = {}
             setmetatable(_x, {__index = emacs._index})")
    (lua-eval "_v = _x.nonexistant")
    (equal lua._v "indexed name = 'nonexistant'"))
  
  ;;seg fault
  ;; (progn ;;test lisp function for __newindex
;;     (setq _newindex (lambda (table name value)
;;                       (lua-rawset table name value)))
;;     (lua-eval "_x = {}
;; setmetatable(_x, {__newindex = emacs._newindex})")
;;     (lua-eval "_x.new = 'somevalue'")
;;     (lua-eval "_v = _x.new")
;;     (equal (lua-get lua._v "new") "somevalue")
;;     )

  (progn ;;test with metatable from emacs
    (setq _t (lua-new-table))
    (lua-set _t "__index" _t)
    (lua-set _t "a" 44)
    (lua-eval "_x = {}")
    (lua-eval "setmetatable(_x, emacs._t)")
    (should (= (lua-get lua._x "a") 44.0)))
  )

(ert-deftest luamacs-type ()
  "test that the lua 'type' function works with references to lisp objects"
  (progn
    (setq _x '(a b c))
    (lua-eval "v = emacs._x
               vt = type(v)")
    ;;(should (equal lua.vt  "lisp_object"))
    ;;they are now wrapped in tables
    (should (equal lua.vt  "table")))
  )

(ert-deftest luamacs-coroutines ()
  "tests that coroutines can be resumed from lisp"
  (progn 
    (lua-eval "function squares() 
 local x = 1
 while true do 
  coroutine.yield(x*x)
  x = x + 1
 end
end")
    (lua-eval "f = coroutine.wrap(squares)")
    (should (equal (let (x)
		     (dotimes (i 10)
		       (push (lua.f) x))
		     x)
		   '(100.0 81.0 64.0 49.0 36.0 25.0 16.0 9.0 4.0 1.0))))
  )

(ert-deftest luamacs-cons-metatable()
  "tests that cons references have the correct metatable"
  (progn (setq _x '("a"))
	 (lua-eval "v = emacs._x
mt = getmetatable(v)")
	 (should (eq lua.lisp_cons_metatable lua.mt)))
  )

(ert-deftest luamacs-vector-metatable()
  "tests that vector references have the correct metatable"
  (progn (setq _x ["a"])
	 (lua-eval "v = emacs._x
mt = getmetatable(v)")
	 (should (eq lua.lisp_vector_metatable lua.mt)))
  )

(ert-deftest luamacs-cons__index ()
  "tests that L[i] works for cons reference L"
  (progn (setq _x '("a" "b" "c"))
	 (lua-eval "v = emacs._x
y = v[1]
sea = emacs._x[2]")
	 (should (equal lua.y "b"))
	 (should (equal lua.sea "c"))
	 ))

(ert-deftest luamacs-vector__index ()
  "tests that L[i] works for cons reference L"
  (progn (setq _x ["a" "b" "c"])
	 (lua-eval "v = emacs._x
y = v[1]
sea = emacs._x[2]")
	 (should (equal lua.y "b"))
	 (should (equal lua.sea "c"))
	 ))

(ert-deftest luamacs-cons__len ()
  "tests that getting the length with #L works for cons reference L"
  (progn
    (setq _x '(2 3 4))
    (lua-eval "len = #emacs._x
v = emacs._x
len2 = #v")
    (should (= lua.len (length _x)))
    (should (= lua.len2 (length _x)))
    ))

(ert-deftest luamacs-vector__len ()
  "tests that getting the length with #L works for cons reference L"
  (progn
    (setq _x [2 3 4])
    (lua-eval "len = #emacs._x
v = emacs._x
len2 = #v")
    (should (= lua.len (length _x)))
    (should (= lua.len2 (length _x)))
    ))

(ert-deftest luamacs-cons__pairs ()
  "tests that the pairs method works on cons references"
  (progn 
    (setq _x '(2 4 6 8))
    (lua-eval "new = {}
for k,v in pairs(emacs._x) do
 print('key = ' .. k)
 print('val = ' .. v)
 new[k] = v*v
end
emacs._a = new[0]
emacs._b = new[1]
emacs._c = new[3]")
    (should (and (= _a 4)
		 (= _b 16)
		 (= _c 64)))))


(ert-deftest luamacs-vector__pairs ()
  "tests that the pairs method works on vector references"
  (progn 
    (setq _x [2 4 6 8])
    (lua-eval "new = {}
for k,v in pairs(emacs._x) do
 print('key = ' .. k)
 print('val = ' .. v)
 new[k] = v*v
end
emacs._a = new[0]
emacs._b = new[1]
emacs._c = new[3]")
    (should (and (= _a 4)
		 (= _b 16)
		 (= _c 64)))))

(provide 'luamacs-tests)

