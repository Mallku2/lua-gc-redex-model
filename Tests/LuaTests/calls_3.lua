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

-- test for generic load
local x = "-- a comment\0\0\0\n  x = 10 + \n23; \
     local a = function () x = 'hi' end; \
     return '\0'"
function read1 (x)
  local i = 0
  return function ()
    collectgarbage()
    i=i+1
    return string.sub(x, i, i)
  end
end

------------------------------------------
-- TODO: string.find
------------------------------------------
-- function cannotload (msg, a,b)
--   assert(not a and string.find(b, msg))
-- end

a = assert(load(read1(x), "modname", "t", _G))
assert(a() == "\0" and _G.x == 33)

------------------------------------------
-- TODO: debug and string.find
------------------------------------------
-- assert(debug.getinfo(a).source == "modname")
-- cannot read text in binary mode
-- cannotload("attempt to load a text chunk", load(read1(x), "modname", "b", {}))
-- cannotload("attempt to load a text chunk", load(x, "modname", "b"))

a = assert(load(function () return nil end))
a()  -- empty chunk

assert(not load(function () return true end))

------------------------------------------
-- TODO: table.remove
------------------------------------------
---- small bug
-- local t = {nil, "return ", "3"}
-- f, msg = load(function () return table.remove(t, 1) end)
-- assert(f() == nil)   -- should read the empty chunk

-- another small bug (in 5.2.1)
f = load(string.dump(function () return 1 end), nil, "b", {})
assert(type(f) == "function" and f() == 1)


x = string.dump(load("x = 1; return x"))
a = assert(load(read1(x), nil, "b"))
assert(a() == 1 and _G.x == 1)
------------------------------------------
-- TODO: string.find (in cannotload)
------------------------------------------
-- cannotload("attempt to load a binary chunk", load(read1(x), nil, "t"))
-- cannotload("attempt to load a binary chunk", load(x, nil, "t"))

--assert(not pcall(string.dump, print))  -- no dump of C functions

-- cannotload("unexpected symbol", load(read1("*a = 123")))
-- cannotload("unexpected symbol", load("*a = 123"))
-- cannotload("hhi", load(function () error("hhi") end))

-- any value is valid for _ENV
assert(load("return _ENV", nil, nil, 123)() == 123)


-- load when _ENV is not first upvalue
local x; XX = 123
local function h ()
  local y=x   -- use 'x', so that it becomes 1st upvalue
  return XX   -- global name
end
local d = string.dump(h)
x = load(d, "", "b")
------------------------------------------
-- TODO: debug
------------------------------------------
-- assert(debug.getupvalue(x, 2) == '_ENV')
-- debug.setupvalue(x, 2, _G)
-- assert(x() == 123)

assert(assert(load("return XX + ...", nil, nil, {XX = 13}))(4) == 17)


-- test generic load with nested functions
x = [[
  return function (x)
    return function (y)
     return function (z)
       return x+y+z
     end
   end
  end
]]

a = assert(load(read1(x)))
assert(a()(2)(3)(10) == 15)

------------------------------------------
-- TODO: debug
------------------------------------------
---- test for dump/undump with upvalues
-- local a, b = 20, 30
-- x = load(string.dump(function (x)
--   if x == "set" then a = 10+b; b = b+1 else
--   return a
--   end
-- end))
-- assert(x() == nil)
-- assert(debug.setupvalue(x, 1, "hi") == "a")
-- assert(x() == "hi")
-- assert(debug.setupvalue(x, 2, 13) == "b")
-- assert(not debug.setupvalue(x, 3, 10))   -- only 2 upvalues
-- x("set")
-- assert(x() == 23)
-- x("set")
-- assert(x() == 24)


-- test for bug in parameter adjustment
assert((function () return nil end)(4) == nil)
assert((function () local a; return a end)(4) == nil)
assert((function (a) return a end)() == nil)

print('OK')

-- TODO: deep is not defined in this module
return deep
