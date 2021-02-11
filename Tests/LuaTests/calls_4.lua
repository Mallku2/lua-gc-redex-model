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


