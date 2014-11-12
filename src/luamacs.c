/* Misc Luamacs functions and lisp primitives

This file is part of Luamacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <stdio.h>
#include "lisp.h"

static int lua_fn_extract_lisp_value (lua_State *L);
  
DEFUN ("lua-eval", Flua_eval, Slua_eval, 1, 1, 0,
       doc: /* Evaluate argument as lua code. return t on success */)
  (Lisp_Object code)
{
  CHECK_STRING(code);
  return luaL_dostring(L, XSTRING(code)->data) ? Qnil : Qt;
}

DEFUN ("lua-load", Flua_load, Slua_load, 1, 1, 0,
       doc: /* load file containing lua code. return t on success, else nil */)
  (Lisp_Object file)
{
  CHECK_STRING(file);

  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  int err = luaL_dofile(L, XSTRING(file)->data);
  return err ? Qnil : Qt;
}

////////////////////////////////////////////////////////////////////////////////
/// tables

DEFUN ("lua-new-table", Flua_new_table, Slua_new_table, 0, 0, 0,
       doc: /* Create a new lua table */)
  (void)
{
  lua_newtable(L);
  Lisp_Object ret = lua_to_lisp(-1);
  return ret;
}

DEFUN ("lua-get", Flua_get, Slua_get, 2, 2, 0,
       doc: /* Is the equivalent TABLE[FIELD]
               TABLE is a lua table, FIELD is a string*/)
  (Lisp_Object table, Lisp_Object field)
{
  Lisp_Object ret;
  
  CHECK_TABLE(table);

  //TODO: gcpro?
  EXTRACT_PUSH_LUA_VAL(table);
  lisp_to_lua(L, field);
  lua_gettable(L, -2);
  ret = lua_to_lisp(-1);
  lua_pop(L, 1); //pop table
  return ret;
}

DEFUN ("lua-set", Flua_set, Slua_set, 3, 3, 0,
       doc: /* Does the equivalent of TABLE[FIELD] = VALUE
               TABLE is a lua table, FIELD is a string */)
  (Lisp_Object table, Lisp_Object field, Lisp_Object value)
{
  CHECK_TABLE(table);

  EXTRACT_PUSH_LUA_VAL(table);
  lisp_to_lua(L, field);
  lisp_to_lua(L, value);
  //lua_setfield(L, -2, XSTRING(field)->data);
  lua_settable(L, -3);
  lua_pop(L, 1); //pop table
  return value;
}

DEFUN ("lua-rawset", Flua_rawset, Slua_rawset, 3, 3, 0,
       doc: /*  Similar to `lua-set', but does a raw assignment
                (i.e., without metamethods). */)
  (Lisp_Object table, Lisp_Object field, Lisp_Object value)
{
  CHECK_TABLE(table);
  EXTRACT_PUSH_LUA_VAL(table);
  lisp_to_lua(L, field);
  lisp_to_lua(L, value);
  lua_rawset(L, -3);
  lua_pop(L, 1); //pop table
  return value;
}

DEFUN ("lua-setmetatable", Flua_setmetatable, Slua_setmetatable, 2, 2, 0,
       doc: /* Sets the metatable for the given table.
               If metatable is nil, removes the metatable of the given table.
               If the original metatable has a "__metatable" field, raises an error. */)
  (Lisp_Object table, Lisp_Object metatable)
{
  CHECK_TABLE(table);
  CHECK_TABLE(metatable);
  //TODO: raise error if the original metatable has a "__metatable" field
  EXTRACT_PUSH_LUA_VAL(table);
  EXTRACT_PUSH_LUA_VAL(metatable);
  lua_setmetatable(L, -2);
  lua_pop(L, 1); //pop table
  return Qt;
}

DEFUN ("alist-to-table", Falist_to_table, Salist_to_table, 1, 1, 0,
       doc: /* Create a new table and add the items of ALIST to it*/)
  (Lisp_Object alist)
{
  register Lisp_Object x;
  Lisp_Object ret;

  CHECK_LIST(alist);

  lua_newtable(L);
  while (! NILP(alist)){
    x = XCAR (alist);
    lisp_to_lua (L, XCAR (x));
    lisp_to_lua (L, XCDR (x));
    lua_settable(L, -3);
    alist = XCDR(alist);
  }

  return lua_to_lisp(-1);
}

DEFUN ("sequence-to-table", Fsequence_to_table, Ssequence_to_table, 1, 1, 0,
       doc: /* Create a new table and add the items in SEQUENCE to it
               with indices 1,...,len(sequence)-1
               SEQUENCE is a list or an array
            */)
  (Lisp_Object sequence)
{
  register Lisp_Object x;
  Lisp_Object ret;
  int len;
  int index = 1;
  lua_newtable(L);
  
  if (CONSP (sequence) || NILP (sequence)){
    while (! NILP(sequence)){
      lua_pushinteger(L, index);
      lisp_to_lua(L, XCAR(sequence));
      lua_settable(L, -3);
      sequence = XCDR(sequence);
      index++;
    }
  }else if (ARRAYP (sequence)){
    len = XFASTINT(Flength(sequence));
    for (int i = 0; i < len; i++){
      lua_pushinteger(L, index);
      lisp_to_lua(L, AREF(sequence, i));
      lua_settable(L, -3);
      index++;
    }
  }else{
    wrong_type_argument (Qsequencep, sequence);
  }

  return lua_to_lisp(-1);
}

////////////////////////////////////////////////////////////////////////////////
// misc lua

DEFUN ("lua-garbage-collect", Flua_garbage_collect, Slua_garbage_collect, 0, 0, 0,
       doc: /* run lua garbage collection */)
  (void)
{
  luaC_fullgc(L, 0);
  return Qt;
}

DEFUN ("lua-stacksize", Flua_stacksize, Slua_stacksize, 0, 0, 0,
       doc: /* L->stacksize */)
  (void)
{
  return make_number(L->stacksize);
}

////////////////////////////////////////////////////////////////////////////////
// debug

DEFUN ("inspect-lua-val", Finspect_lua_val, Sinspect_lua_val, 1, 1, 0,
       doc: /* Evaluate argument as lua code. return t on success, else nil */)
  (Lisp_Object obj)
{
  if (XLUA_VALUE(obj)){
    int type = ttypenv(XLUA_VALUE(obj)->o);
    //  printf("(obj->value_ == obj->o->value__) = %d\n", (int)XLUA_VALUE(obj)->value_ == (int) XLUA_VALUE(obj)->o->value_);

    printf("type = ");
    switch(type){
    case 0:
      printf("LUA_TNIL\n"); break;
    case 1:
      printf("LUA_TBOOLEAN\n"); break;
    case 2:
      printf("LUA_TLIGHTUSERDATA\n"); break;
    case 3:
      printf("LUA_TNUMBER\n"); break;
    case 4:
      printf("LUA_TSTRING\n"); break;
    case 5:
      printf("LUA_TTABLE\n"); break;
    case 6:
      printf("LUA_TFUNCTION\n"); break;
    case 7:
      printf("LUA_TUSERDATA\n"); break;
    case 8:
      printf("LUA_TTHREAD\n"); break;
    case 9:
      printf("LUA_NUMTAGS\n"); break;
    case 10:
      printf("LUA_LISP_OBJECT\n"); break;
    default:
      printf("<unknown type %d>\n", type);
    }
  }else{
    printf("<not a lua obj>\n");
  }
  return Qnil;
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// metatables for lisp object wrappers

int lua_cons__index (lua_State *L){
  printf("lua_cons__index\n");

  Lisp_Object a,b,c,d;

  lua_fn_extract_lisp_value(L);

  if (lua_isnumber(L, 2)){
    lisp_to_lua(L, Fcar(Fnthcdr(make_number(lua_tointeger(L, 2)),
                                lua_to_lisp(-1))));
  }else{
    //lisp_to_lua(L, Fassoc(lua_to_lisp(2), lua_to_lisp(-1)));
    printf("Error: index must be numeric\n"); //TODO
  }
  return 1;
  /* char msg[100]; //TODO: fix this (max var length?) */
  /* sprintf(msg, "Error: emacs.%s is unbound", name); */
  /* signal_error(msg, Qnil); */
}

int lua_cons__len (lua_State *L){
  printf("lua_cons__len\n");

  lua_fn_extract_lisp_value(L);
  
  int n = Flength(lua_to_lisp(-1));
  printf("=====length = %d\n", n);
  lua_pushinteger(L, XINT(n));
  return 1;
}

  lua_pushinteger(L, XINT(Flength(lua_to_lisp(-1))));
  return 1;
}

int lua_setup_metatables(lua_State *L){
  printf("lua_setup_metatables()\n");
  
  //cons ----------------------------
  lua_newtable(L);
  // __index
  lua_pushstring(L, "__index");
  lua_pushcfunction(L, lua_cons__index);
  lua_rawset(L, -3);
  // __newindex
  lua_pushstring(L, "__newindex");
  lua_pushcfunction(L, lua_cons__newindex);
  lua_rawset(L, -3);
  // __len
  lua_pushstring(L, "__len");
  lua_pushcfunction(L, lua_cons__len);
  lua_rawset(L, -3);

  // note: __pairs is a lua function
  lua_setglobal(L, "lisp_cons_metatable");
////////////////////////////////////////////////////////////////////////////////
static int
lua_fn_extract_lisp_value (lua_State *L){
  // for use by lua table methods
  if (!lua_istable(L, 1)){
    printf("Error: not a table\n"); //TODO
    return 0;
  }
  lua_getfield(L, 1, "_lisp");
  if (lua_isnil(L, -1)){
    printf("Error: _lisp field is nil\n"); //TODO
    return 0;
  }
}

////////////////////////////////////////////////////////////////////////////////

void
syms_of_luamacs (void)
{
  defsubr (&Slua_eval);
  defsubr (&Slua_load);    
  defsubr (&Slua_new_table);
  defsubr (&Slua_get);
  defsubr (&Slua_set);
  defsubr (&Slua_rawset);  
  defsubr (&Slua_setmetatable);
  defsubr (&Salist_to_table);
  defsubr (&Ssequence_to_table);  
  defsubr (&Slua_garbage_collect);
  defsubr (&Slua_stacksize);  
  defsubr (&Sinspect_lua_val);
}
