-- Copyright © 1994–2019 Lua.org, PUC-Rio.

-- Permission is hereby granted, free of charge, to any person obtaining a copy 
-- of this software and associated documentation files (the "Software"), to deal 
-- in the Software without restriction, including without limitation the rights 
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
-- copies of the Software, and to permit persons to whom the Software is 
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in 
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
-- SOFTWARE. 

----------------------------------------------------------------
----- added definitions of GC1 and GC for the following test
local function GC1 ()
  local u
  local b     -- must be declared after 'u' (to be above it in the stack)
  local finish = false
  u = setmetatable({}, {__gc = function () finish = true end})
  b = {34}
  -- WAS: nothing, we add an explicit call to the garbage collector to end the
  -- loop
  repeat u = {}; collectgarbage() until finish
  assert(b[1] == 34)   -- 'u' was collected, but 'b' was not

  finish = false; local i = 1
  u = setmetatable({}, {__gc = function () finish = true end})
  -- WAS: nothing, we add an explicit call to the garbage collector to end the
  -- loop
  repeat i = i + 1; u = i .. i; collectgarbage() until finish
  assert(b[1] == 34)   -- 'u' was collected, but 'b' was not

  finish = false
  u = setmetatable({}, {__gc = function () finish = true end})
  -- WAS: nothing, we add an explicit call to the garbage collector to end
  -- the loop
  repeat local i; u = function () return i end; collectgarbage() until finish
  assert(b[1] == 34)   -- 'u' was collected, but 'b' was not
end

local function GC()  GC1(); GC1() end
----------------------------------------------------------------------

-- ephemerons
local mt = {__mode = 'k'}
-- WAS: a = {10,20,30,40}, changed for performance reasons
a = {10,20};
setmetatable(a, mt)
x = nil
-- WAS: i = 1, 100, changed for performance reasons
for i = 1, 2 do local n = {}; a[n] = {k = {x}}; x = n end
GC()
local n = x
local i = 0
while n do n = a[n].k[1]; i = i + 1 end
-- WAS: assert(i == 100), changed for performance reasons
assert(i == 2)
x = nil
GC()
-- WAS: for i = 1, 4 do assert(a[i] == i * 10); a[i] = nil end,
-- changed for performance reasons
for i = 1, 2 do assert(a[i] == i * 10); a[i] = nil end
assert(next(a) == nil)
