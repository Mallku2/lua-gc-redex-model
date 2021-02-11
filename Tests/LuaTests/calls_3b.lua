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
