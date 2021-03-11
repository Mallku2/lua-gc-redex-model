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

------------------------------
-- taken from errors_1.lua
----------------------------
function doit (s)
  local f, msg = load(s)
  if f == nil then return msg end
  local cond, msg = pcall(f)
  return (not cond) and msg
end

-----------------------------------------------------
-- TODO: we check that it fails, but not the error message
-----------------------------------------------------
function checksyntax (prog, extra, token, line)
  local msg = doit(prog)
  assert(msg) --WAS: nothing, we just ensure that there is actually an error
  -- if not string.find(token, "^<%a") and not string.find(token, "^char%(")
  --   then token = "'"..token.."'" end
  -- token = string.gsub(token, "(%p)", "%%%1")
  -- local pt = string.format([[^%%[string ".*"%%]:%d: .- near %s$]],
  --                          line, token)
  -- assert(string.find(msg, pt))
  -- assert(string.find(msg, msg, 1, true))
end

-- error in error handling
local res, msg = xpcall(error, error)
assert(not res and type(msg) == 'string')
print('+')

local function f (x)
  if x==0 then error('a\n')
  else
    local aux = function () return f(x-1) end
    local a,b = xpcall(aux, aux)
    return a,b
  end
end
f(3)

--   local function loop (x,y,z) return 1 + loop(x, y, z) end
 
--   local res, msg = xpcall(loop, function (m)
--     assert(string.find(m, "stack overflow"))
--     local res, msg = pcall(loop)
--     assert(string.find(msg, "error handling"))
--     assert(math.sin(0) == 0)
--     return 15
--   end)
--   assert(msg == 15)

--   res, msg = pcall(function ()
--     for i = 999900, 1000000, 1 do table.unpack({}, 1, i) end
--   end)
--   assert(string.find(msg, "too many results"))

-- end


-- non string messages
function f() error{msg='x'} end
res, msg = xpcall(f, function (r) return {msg=r.msg..'y'} end)
assert(msg.msg == 'xy')

------------------------------------------
-- TODO: string.find
------------------------------------------
-- xpcall with arguments
-- a, b, c = xpcall(string.find, error, "alo", "al")
-- assert(a and b == 1 and c == 2)
-- a, b, c = xpcall(string.find, function (x) return {} end, true, "al")
-- assert(not a and type(b) == "table" and c == nil)

print('+')
checksyntax("syntax error", "", "error", 1)
checksyntax("1.000", "", "1.000", 1)
checksyntax("[[a]]", "", "[[a]]", 1)
checksyntax("'aa'", "", "'aa'", 1)

-- test 255 as first char in a chunk
checksyntax("\255a = 1", "", "char(255)", 1)

doit('I = load("a=9+"); a=3')
assert(a==3 and I == nil)
print('+')

---------------------------------------------------
-- WAS: lim = 1000, changed for performance reasons
---------------------------------------------------
lim = 10
if _soft then lim = 100 end
for i=1,lim do
  doit('a = ')
  doit('a = 4+nil')
end


-- -- testing syntax limits
-- local function testrep (init, rep)
--   local s = "local a; "..init .. string.rep(rep, 400)
--   local a,b = load(s)
--   assert(not a and string.find(b, "levels"))
-- end
-- testrep("a=", "{")
-- testrep("a=", "(")
-- testrep("", "a(")
-- testrep("", "do ")
-- testrep("", "while a do ")
-- testrep("", "if a then else ")
-- testrep("", "function foo () ")
-- testrep("a=", "a..")
-- testrep("a=", "a^")

-- local s = ("a,"):rep(200).."a=nil"
-- local a,b = load(s)
-- assert(not a and string.find(b, "levels"))


-- -- testing other limits
-- -- upvalues
-- local lim = 127
-- local  s = "local function fooA ()\n  local "
-- for j = 1,lim do
--   s = s.."a"..j..", "
-- end
-- s = s.."b,c\n"
-- s = s.."local function fooB ()\n  local "
-- for j = 1,lim do
--   s = s.."b"..j..", "
-- end
-- s = s.."b\n"
-- s = s.."function fooC () return b+c"
-- local c = 1+2
-- for j = 1,lim do
--   s = s.."+a"..j.."+b"..j
--   c = c + 2
-- end
-- s = s.."\nend  end end"
-- local a,b = load(s)
-- assert(c > 255 and string.find(b, "too many upvalues") and
--        string.find(b, "line 5"))

-- -- local variables
-- s = "\nfunction foo ()\n  local "
-- for j = 1,300 do
--   s = s.."a"..j..", "
-- end
-- s = s.."b\n"
-- local a,b = load(s)
-- assert(string.find(b, "line 2"))

--mt.__index = oldmm

print('OK')
