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

local w
-- testing multi-level closure
function f(x)
  return function (y)
    return function (z) return w+x+y+z end
  end
end

y = f(10)
w = 1.345
assert(y(20)(30) == 60+w)

-- testing closures x repeat-until
local a = {}
local i = 1
repeat
  local x = i
  a[i] = function () i = x+1; return x end
until i > 10 or a[i]() ~= x
assert(i == 11 and a[1]() == 1 and a[3]() == 3 and i == 4)

------------------------------------------
-- TODO: goto
------------------------------------------
-- testing closures created in 'then' and 'else' parts of 'if's
--[[a = {}
for i = 1, 10 do
  if i % 3 == 0 then
    local y = 0
    a[i] = function (x) local t = y; y = x; return t end
  elseif i % 3 == 1 then
    goto L1
    error'not here'
  ::L1::
    local y = 1
    a[i] = function (x) local t = y; y = x; return t end
  elseif i % 3 == 2 then
    local t
    goto l4
    ::l4a:: a[i] = t; goto l4b
    error("should never be here!")
    ::l4::
    local y = 2
    t = function (x) local t = y; y = x; return t end
    goto l4a
    error("should never be here!")
    ::l4b::
  end
end

for i = 1, 10 do
  assert(a[i](i * 10) == i % 3 and a[i]() == i * 10)
end]]--

print'+'

-- test for correctly closing upvalues in tail calls of vararg functions
local function t ()
  local function c(a,b) assert(a=="test" and b=="OK") end
  local function v(f, ...) c("test", f() ~= 1 and "FAILED" or "OK") end
  local x = 1
  return v(function() return x end)
end
t()


------------------------------------------
-- TODO: debug
------------------------------------------
---- test for debug manipulation of upvalues
--[[local debug = require'debug'

do
  local a , b, c = 3, 5, 7
  foo1 = function () return a+b end;
  foo2 = function () return b+a end;
  do
    local a = 10
    foo3 = function () return a+b end;
  end
end

assert(debug.upvalueid(foo1, 1))
assert(debug.upvalueid(foo1, 2))
assert(not pcall(debug.upvalueid, foo1, 3))
assert(debug.upvalueid(foo1, 1) == debug.upvalueid(foo2, 2))
assert(debug.upvalueid(foo1, 2) == debug.upvalueid(foo2, 1))
assert(debug.upvalueid(foo3, 1))
assert(debug.upvalueid(foo1, 1) ~= debug.upvalueid(foo3, 1))
assert(debug.upvalueid(foo1, 2) == debug.upvalueid(foo3, 2))

assert(debug.upvalueid(string.gmatch("x", "x"), 1) ~= nil)

assert(foo1() == 3 + 5 and foo2() == 5 + 3)
debug.upvaluejoin(foo1, 2, foo2, 2)
assert(foo1() == 3 + 3 and foo2() == 5 + 3)
assert(foo3() == 10 + 5)
debug.upvaluejoin(foo3, 2, foo2, 1)
assert(foo3() == 10 + 5)
debug.upvaluejoin(foo3, 2, foo2, 2)
assert(foo3() == 10 + 3)

assert(not pcall(debug.upvaluejoin, foo1, 3, foo2, 1))
assert(not pcall(debug.upvaluejoin, foo1, 1, foo2, 3))
assert(not pcall(debug.upvaluejoin, foo1, 0, foo2, 1))
assert(not pcall(debug.upvaluejoin, print, 1, foo2, 1))
assert(not pcall(debug.upvaluejoin, {}, 1, foo2, 1))
assert(not pcall(debug.upvaluejoin, foo1, 1, print, 1))]]--

print'OK'
