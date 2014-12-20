-- Welcome to Charland, a game designed to demo some features of Luamacs.
--
-- In Charland, you are a character, as is everyone else. In this world
-- your societal class is determined by your ASCII value. Unfortunately,
-- Charland is consumed by perpetual class warfar.
--
-- You must chase and kill other chars who are of lower class then yourself
-- while avoiding chars of higher classes. Chars are killed by entering
-- the location they occupy. For every char you kill, you gain points
-- equal to their ASCII value. If a higher class char catches you, you loose
-- points equal to its ACCII value. As you gain points your ascii value (and
-- therefor your class) increases. When you reach the highest possible class,
-- you win. Now you are free to terrorize the lower class characters and collect
-- even more points without possibility of loss.
--
-- First, evaluate this file with:
--  M-x lua-eval-buffer
--
-- start the game:
--  M-x charland
--
-- keys:
--  w: move up
--  a: move left
--  d: move right
--  s: move down
--  p: protest the war. The game is paused while you do so.
--     press 'p' again to resume
--
--
-- Implementation Notes.
--   The idea was to implement cooperative multitasking using coroutines,
--   alas, that did not work. This is the first non-trivial Luamacs program,
--   and a number of bugs where discovered during the course of its creation.
--   The relevant bug mysteriously prevents the lisp function 'goto-char'
--   from being called inside a coroutine.
--   Instead, after each turn, a character inserts itself into a central schedule
--   (implemented as a priority queue) with some delay value. After that
--   amount of time has passed, the main update function will call the
--   characters update function again.
--
--
-- TODO:
--  The following things are broken or unimplemented:
--    - the user can still insert text into the game field
--    - the chars need to modify their course to try catching the user
--

-- These lisp function names are not valid in lua, for convenience they
-- are given a legal local name
local goto_char = elf['goto-char']
local insert_char = elf['insert-char']
local current_buffer = elf['current-buffer']
local switch_to_buffer = elf['switch-to-buffer']
local run_with_timer = elf['run-with-timer']
local get_buffer = elf['get-buffer']
local backward_char = elf['backward-char']
local delete_char = elf['delete-char']
local buffer_name = elf['buffer-name']
local add_text_properties = elf['add-text-properties']

local cl_buffer = "*CharLand*"
local cl_paused = false

local counter = 0

min_char = 33
max_char = 126
user_start_char = 50

chars = {} --a mapping of char positions to chars
space = string.byte(" ")

Char = {}
Char.__index = Char
function Char:new(o)
   o = o or {}
   setmetatable(o, self)
   o.id = counter
   o.c = math.random(max_char - min_char - 1) + min_char
   o.update_delay = 0.1 + math.random()
   counter = counter + 1
   return o
end

function Char:set_location(x, y)
   if self.pos and chars[self.pos] then
      chars[self.pos] = nil
   end
   self.x = x
   self.y = y
   self.pos = to_char_position(x, y)
   chars[self.pos] = self
end

function Char:move_to(location)
   if cl_paused then
      return false
   end

   local cx = self.x
   local cy = self.y
   local nx = location.x
   local ny = location.y
   if ny < 1 or nx >= width - 1 or nx <= 0 or ny >= height then
      self.delete = true
      return false
   end
   from = to_char_position(cx, cy)
   goto_char(from)
   delete_char(1)
   insert_char(space)
   to = to_char_position(nx, ny)
   resident = chars[to]
   self:set_location(nx, ny)
   if resident then
      if self == user then
	 -- we got it!
	 if self.c >= resident.c then
	    score = score + resident.c
	 else
	    score = score - resident.c
	 end
	 update_score()
	 resident.delete = true
	 --TODO: remove the character from the board
      elseif resident == user then
	 -- they go us:
	 if self.c <= resident.c then
	    score = score + resident.c
	 else
	    score = score - resident.c
	 end
	 update_score()
	 self.delete = true
	 self.c = resident.c
      else
         --some char caught another char, the one with lower class dies
	 if self.c > resident.c then
	    resident.delete = true
	 else
	    self.delete = true
            self.c = resident.c
	 end
      end
   end
   goto_char(to)
   delete_char(1)
   insert_colored_char(self)
   --backward_char()
   return true
end

function to_char_position(x,y)
   return y*width + x
end

function insert_colored_char(char)
   insert_char(char.c)
   if char.c > user.c then
      add_text_properties(char.pos, char.pos+1, el.red_face)
   elseif char.pos ~= user.pos then
      add_text_properties(char.pos, char.pos+1, el.green_face)
   end
   goto_char(user.pos)
end

function Char:char_next (direction)
   local x = self.x + (direction > 0)
   local c = char_before(to_char_position(x, self.y))
   if c ~= " " then
      return c
   end
   return false
end

function Char:char_below (direction)
   local x = self.x + (direction > 0)
   local c = char_before(to_char_position(x, self.y))
   if c ~= " " then
      return c
   end
   return false
end

function Char:live ()
   -- while true do
   if self.delete then
      --delete this one and create another one to replace it
      spawn()
      --TODO: need to remove the character from the board if it reaches the edge
      return --delete by not inserting back into the schedule
   end

   dx = self.x - user.x
   dy = self.y - user.y
   --if x distance is greater and the space next to this is free:
   -- if math.abs(dx > dy) and not self:char_next(dx) then
   -- elseif self:char_below(dy) then
   -- end
   local p = self.line()
   local in_bounds = self:move_to(p)
   -- insert into the main schedule and yield to another process
   -- these don't work for some reason, even when in_bound is 'true'
   --   if in_bound then
   --   if in_bound == true then
   if in_bound ~= false then
      schedule:insert(self.cr, self.update_delay)
   end
   --    coroutine.yield()
   return
   --   end
end


function random_location()
   --todo: find empty location
   return {x = math.random(width-1), y = math.random(height-1)}
end

function make_line_fn (p1, p2)
   local m = (p2.y - p1.y)/(p2.x - p1.x)

   local x1 = p1.x
   local y1 = p1.y
   local mXx1 = m*x1
   local i = x1
   local function next()
      i = i + 1
      return {x = math.floor(i), y = math.floor(m*i - mXx1 + y1)}
   end
   return next
end

function random_line ()
   local p1 = random_location()
   local p2 = random_location()
   p1.x = 0 -- p1 is on left border
   p2.x = width-1 -- p2 is on right border
   return make_line_fn(p1, p2)
end

function spawn()
   local line = random_line()
   local start = line()
   local c = Char:new{x = start.x, y = start.y}
   chars[to_char_position(start.x, start.y)] = c
   c.line = line
   --c.cr = coroutine.wrap(function () c:live() end)
   c.cr = function () c:live() end
   schedule:insert(c.cr, c.update_delay + math.random(10))
end

width = 50
height = 30
n_chars = 50 -- number of other characters in play

user = Char:new()
user.c = user_start_char
user:set_location(math.floor(width/2), math.floor(height/2))


RIGHT={x=1, y=0}
LEFT={x=-1, y=0}
UP={x=0, y=-1}
DOWN={x=0, y=1}

function move_user(direction)
   return user:move_to{x = user.x + direction.x, y = user.y + direction.y}
end


function cl_update (cl_buffer)
   if not cl_paused and current_buffer() == cl_buffer then
   end
end

------------------------------------------------------------------
-- functions for pausing and resuming

paused_time = 0
pause_start = 0
function pause_game ()
   pause_start = os.time()
   paused = true
end
function resume_game ()
   if paused then
      paused_time = paused_time + os.time() - pause_start
      paused = false
   end
end
function toggle_pause()
   if paused then
      resume_game()
   else
      pause_game()
   end
end


function charland ()
   local initialized = get_buffer(cl_buffer)
   --   print("initialized = ")
   --   print(initialized)   -- this crashes emacs
   switch_to_buffer(cl_buffer)
   for i=1,height do
      for i=1,width-2 do
	 elf.insert(" ")
      end
      elf.insert("|\n")
   end
   for i=1,width-1 do
      elf.insert("-")
   end
   goto_char(1)
   delete_char(7)
   elf.insert("score: ")

   c = to_char_position(10,10)
   goto_char(c)

   --   if not initialized then
   local_set_key("w", function () move_user(UP) end)
   local_set_key("a", function () move_user(LEFT) end)
   local_set_key("s", function () move_user(DOWN) end)
   local_set_key("d", function () move_user(RIGHT) end)
   local_set_key("p", function () toggle_pause() end)
   -- end

   -- spawn other characters
   for i=1,n_chars do
      spawn()
   end
   update()
   update_score()
   move_user(UP)--just for the redraw
end

def_command("charland", charland)

---------------------------------------------------------------------

function time ()
   return os.time() - paused_time
end

schedule = {}
function schedule:insert(func, seconds)
   local time = time() + seconds
   local item = {t = time, f = func}
   if #self == 0 or time < self[1].t then
      table.insert(self, 1, item)
      return
   end
   if time > self[#self].t then
      table.insert(self, item)
      return
   end
   for i=1,#self do
      v = self[i]
      if v.t >= time then
	 table.insert(self, i, item)
	 return
      end
   end
end

function schedule:get_next()
   if #self > 0 then
      return table.remove(self, 1).f
   else
      return nil
   end
end

function schedule:next_time()
   --returns the time the next item is scheduled
   if #self > 0 then
      return self[1].t
   else
      return -1
   end
end

cl_update_time = 0.3
function update()
   while true do
      if buffer_name() ~= cl_buffer and not paused then
	 pause_game()
	 return
      end

      next_time = schedule:next_time()
      if next_time < 0 then
	 run_with_timer(cl_update_time, nil, update)
	 return
      end
      if next_time > time() then
         --schedule this function to be called next
         --run_with_timer(next_time - time(), nil, update)
	 run_with_timer(cl_update_time, nil, update) -- check more often to pause if needed
	 return
      end
      local x = schedule:get_next()
      x()
   end
end


----------------------------------------------------------------------

score = 0

function update_score()
   goto_char(8)
   delete_char(10)
   local s = "" .. score
   elf.insert(s)
   for _=1,(10-#s) do
      elf.insert(" ")
   end
   goto_char(user.pos)
end
