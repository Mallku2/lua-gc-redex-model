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

-------------------------------------
-- Taken from errors_1a.lua
-------------------------------------
function doit (s)
  local f, msg = load(s)
  if f == nil then return msg end
  local cond, msg = pcall(f)
  return (not cond) and msg
end

-----------------------------------------------------
-- TODO: we check it fails, but not the error message
-----------------------------------------------------
function checkmessage (prog, msg)
  local m = doit(prog)
  assert(m) --WAS: nothing, we ensure the error is not bogus"
  -- assert(string.find(m, msg, 1, true))
end

-- tests for better error messages

checkmessage("a=1; bbbb=2; a=math.sin(3)+bbbb(3)", "global 'bbbb'")
checkmessage("a=1; local a,bbbb=2,3; a = math.sin(1) and bbbb(3)",
       "local 'bbbb'")
checkmessage("a={}; do local a=1 end a:bbbb(3)", "method 'bbbb'")
checkmessage("local a={}; a.bbbb(3)", "field 'bbbb'")
------------------------------------------
-- TODO: string.find
------------------------------------------
-- assert(not string.find(doit"a={13}; local bbbb=1; a[bbbb](3)", "'bbbb'"))
checkmessage("a={13}; local bbbb=1; a[bbbb](3)", "number")
checkmessage("a=(1)..{}", "a table value")

aaa = nil
checkmessage("aaa.bbb:ddd(9)", "global 'aaa'")
checkmessage("local aaa={bbb=1}; aaa.bbb:ddd(9)", "field 'bbb'")
checkmessage("local aaa={bbb={}}; aaa.bbb:ddd(9)", "method 'ddd'")
checkmessage("local a,b,c; (function () a = b+1 end)()", "upvalue 'b'")
assert(not doit"local aaa={bbb={ddd=next}}; aaa.bbb:ddd(nil)")

checkmessage("local _ENV = {x={}}; a = a + 1", "global 'a'")

checkmessage("b=1; local aaa='a'; x=aaa+b", "local 'aaa'")
checkmessage("aaa={}; x=3/aaa", "global 'aaa'")
checkmessage("aaa='2'; b=nil;x=aaa*b", "global 'b'")
checkmessage("aaa={}; x=-aaa", "global 'aaa'")

------------------------------------------
-- TODO: string.find
------------------------------------------
-- assert(not string.find(doit"aaa={}; x=(aaa or aaa)+(aaa and aaa)", "'aaa'"))
-- assert(not string.find(doit"aaa={}; (aaa or aaa)()", "'aaa'"))

checkmessage("print(print < 10)", "function")
checkmessage("print(print < print)", "two function")

------------------------------------------
-- TODO: debug
------------------------------------------
-- passing light userdata instead of full userdata
-- _G.D = debug
-- checkmessage([[
--   -- create light udata
--   local x = D.upvalueid(function () return debug end, 1)
--   D.setuservalue(x, {})
-- ]], "light userdata")
-- _G.D = nil


