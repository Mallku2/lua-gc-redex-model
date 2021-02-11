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

-- ------------------------------------------
-- -- TODO: debug
-- ------------------------------------------
-- ---- test for dump/undump with upvalues
-- -- local a, b = 20, 30
-- -- x = load(string.dump(function (x)
-- --   if x == "set" then a = 10+b; b = b+1 else
-- --   return a
-- --   end
-- -- end))
-- -- assert(x() == nil)
-- -- assert(debug.setupvalue(x, 1, "hi") == "a")
-- -- assert(x() == "hi")
-- -- assert(debug.setupvalue(x, 2, 13) == "b")
-- -- assert(not debug.setupvalue(x, 3, 10))   -- only 2 upvalues
-- -- x("set")
-- -- assert(x() == 23)
-- -- x("set")
-- -- assert(x() == 24)


-- test for bug in parameter adjustment
assert((function () return nil end)(4) == nil)
assert((function () local a; return a end)(4) == nil)
assert((function (a) return a end)() == nil)

print('OK')

-- TODO: deep is not defined in this module
return deep
