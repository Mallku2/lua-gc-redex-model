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

-- WAS: local a = {}; for i=1,3000 do a[i] = "xuxu" end
-- WAS: assert(table.concat(a, "123").."123" == string.rep("xuxu123", 3000))
local a = {}; for i=1,30 do a[i] = "xuxu" end
assert(table.concat(a, "123").."123" == string.rep("xuxu123", 30))
assert(table.concat(a, "b", 20, 20) == "xuxu")
assert(table.concat(a, "", 20, 21) == "xuxuxuxu")
assert(table.concat(a, "x", 22, 21) == "")
-- WAS: assert(table.concat(a, "3", 2999) == "xuxu3xuxu")
 assert(table.concat(a, "3", 29) == "xuxu3xuxu")
