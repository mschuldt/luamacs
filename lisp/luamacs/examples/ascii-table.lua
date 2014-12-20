-- defines the Emacs command 'ascii_table'
--- based of an elisp version from the emacswiki

switch_to_buffer = elf['switch-to-buffer']
erase_buffer = elf['erase-buffer']
local_set_key = elf['local-set-key']
bury_buffer = elf['bury-buffer']
single_key_description = elf['single-key-description']
goto_char = elf['goto-char']

function ascii_table ()
   -- Display basic ASCII table (0 thru 128).
   switch_to_buffer("*ASCII*")
   erase_buffer()
   el['buffer-read-only']  = nil
   local_set_key("q", bury_buffer)
   local i = -1
   elf.insert("ASCII characters 0 thru 127.\n\n")
   elf.insert(" Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
   while i < 31 do
      a = 1 + i
      b = a + 32
      c = b + 32
      d = c + 32
      i = d
      -- there currently a bug that prevents us making nested calls to lisp functions
      -- so instead of a(b(c())) we have to write c_ = c(); b_ = b(c_); a(b_)
      -- hence the exceptional verboseness of the following code
      a_k = single_key_description(a)
      b_k = single_key_description(b)
      c_k = single_key_description(c)
      d_k = single_key_description(d)
      str = elf.format("%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n",
		       a, a, a_k,
		       b, b, b_k,
		       c, c, c_k,
		       d, d, d_k)
      elf.insert(str)
       i = i - 96
   end
   goto_char(1)
end

def_command('ascii_table', ascii_table)
