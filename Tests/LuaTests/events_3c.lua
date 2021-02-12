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

---------------------------------
--- taken from events_3b.lua
--------------------------------
t = {}

-- test `partial order'

local function Set(x)
  local y = {}
  for _,k in pairs(x) do y[k] = 1 end
  return setmetatable(y, t)
end

t.__lt = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
    b[k] = nil
  end
  return next(b) ~= nil
end

t.__le = nil

assert(Set{1,2,3} < Set{1,2,3,4})
assert(not(Set{1,2,3,4} < Set{1,2,3,4}))
assert((Set{1,2,3,4} <= Set{1,2,3,4}))
assert((Set{1,2,3,4} >= Set{1,2,3,4}))
assert((Set{1,3} <= Set{3,5}))   -- wrong!! model needs a `le' method ;-)

t.__le = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
  end
  return true
end

assert(not (Set{1,3} <= Set{3,5}))   -- now its OK!
assert(not(Set{1,3} <= Set{3,5}))
assert(not(Set{1,3} >= Set{3,5}))

t.__eq = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
    b[k] = nil
  end
  return next(b) == nil
end

local s = Set{1,3,5}
assert(s == Set{3,5,1})
assert(not rawequal(s, Set{3,5,1}))
assert(rawequal(s, s))
assert(Set{1,3,5,1} == Set{3,5,1})
assert(Set{1,3,5} ~= Set{3,5,1,6})
t[Set{1,3,5}] = 1
assert(t[Set{1,3,5}] == nil)   -- `__eq' is not valid for table accesses


t.__concat = function (a,b,c)
  assert(c == nil)
  if type(a) == 'table' then a = a.val end
  if type(b) == 'table' then b = b.val end
  if A then return a..b
  else
    return setmetatable({val=a..b}, t)
  end
end

c = {val="c"}; setmetatable(c, t)
d = {val="d"}; setmetatable(d, t)

A = true
assert(c..d == 'cd')
assert(0 .."a".."b"..c..d.."e".."f"..(5+3).."g" == "0abcdef8g")

A = false
assert((c..d..c..d).val == 'cdcd')
x = c..d
assert(getmetatable(x) == t and x.val == 'cd')
x = 0 .."a".."b"..c..d.."e".."f".."g"
assert(x.val == "0abcdefg")
