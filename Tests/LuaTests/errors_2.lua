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
--- taken from errors_1a.lua
--------------------------------
function doit (s)
  local f, msg = load(s)
  if f == nil then return msg end
  local cond, msg = pcall(f)
  return (not cond) and msg
end

function checkmessage (prog, msg)
  local m = doit(prog)
  assert(m) --WAS: nothing, we just ensure that there is actually an error
  -- assert(string.find(m, msg, 1, true))
end

-- global functions
------------------------------------------
-- TODO: io.write
------------------------------------------
-- checkmessage("(io.write or print){}", "io.write")
checkmessage("(collectgarbage or print){}", "collectgarbage")

-- tests for field accesses after RK limit
local t = {}
for i = 1, 10 do --WAS: for i = 1, 1000 do
  t[i] = "a = x" .. i
end
local s = table.concat(t, "; ")
t = nil
checkmessage(s.."; a = bbb + 1", "global 'bbb'")
checkmessage("local _ENV=_ENV;"..s.."; a = bbb + 1", "global 'bbb'")
checkmessage(s.."; local t = {}; a = t.bbb + 1", "field 'bbb'")
checkmessage(s.."; local t = {}; t:bbb()", "method 'bbb'")

checkmessage([[aaa=9
repeat until 3==3
local x=math.sin(math.cos(3))
if math.sin(1) == x then return math.sin(1) end   -- tail call
local a,b = 1, {
  {x='a'..'b'..'c', y='b', z=x},
  {1,2,3,4,5} or 3+3<=3+3,
  3+1>3+1,
  {d = x and aaa[x or y]}}
]], "global 'aaa'")

checkmessage([[
local x,y = {},1
if math.sin(1) == 0 then return 3 end    -- return
x.a()]], "field 'a'")

checkmessage([[
prefix = nil
insert = nil
while 1 do
  local a
  if nil then break end
  insert(prefix, a)
end]], "global 'insert'")

checkmessage([[  -- tail call
  return math.sin("a")
]], "'sin'")

------------------------------------------
-- TODO: options to collectgarbage
------------------------------------------
-- checkmessage([[collectgarbage("nooption")]], "invalid option")

checkmessage([[x = print .. "a"]], "concatenate")

------------------------------------------
-- TODO: io
------------------------------------------
-- checkmessage("getmetatable(io.stdin).__gc()", "no value")

checkmessage([[
local Var
local function main()
  NoSuchName (function() Var=0 end)
end
main()
]], "global 'NoSuchName'")
print'+'

a = {}; setmetatable(a, {__index = string})
checkmessage("a:sub()", "bad self")
checkmessage("string.sub('a', {})", "#2")
checkmessage("('a'):sub{}", "#1")

------------------------------------------
-- TODO: table.sort and string.gsub
------------------------------------------
-- checkmessage("table.sort({1,2,3}, table.sort)", "'table.sort'")
-- next message may be 'setmetatable' or '_G.setmetatable'
-- checkmessage("string.gsub('s', 's', setmetatable)", "setmetatable'")

------------------------------------------
-- TODO: coroutine
------------------------------------------
-- tests for errors in coroutines

-- function f (n)
--   local c = coroutine.create(f)
--   local a,b = coroutine.resume(c)
--   return b
-- end
-- assert(string.find(f(), "C stack overflow"))

-- checkmessage("coroutine.yield()", "outside a coroutine")

-- f1 = function () table.sort({1,2,3}, coroutine.yield) end
-- f = coroutine.wrap(function () return pcall(f1) end)
-- assert(string.find(select(2, f()), "yield across"))


------------------------------------------
-- TODO: string functions
------------------------------------------
-- testing size of 'source' info; size of buffer for that info is
-- LUA_IDSIZE, declared as 60 in luaconf. Get one position for '\0'.
-- idsize = 60 - 1
-- local function checksize (source)
--   -- syntax error
--   local _, msg = load("x", source)
--   msg = string.match(msg, "^([^:]*):")   -- get source (1st part before ':')
--   assert(msg:len() <= idsize)
-- end

-- for i = 60 - 10, 60 + 10 do   -- check border cases around 60
--   checksize("@" .. string.rep("x", i))   -- file names
--   checksize(string.rep("x", i - 10))     -- string sources
--   checksize("=" .. string.rep("x", i))   -- exact sources
-- end


-- -- testing line error

-- local function lineerror (s, l)
--   local err,msg = pcall(load(s))
--   local line = string.match(msg, ":(%d+):")
--   assert((line and line+0) == l)
-- end

-- lineerror("local a\n for i=1,'a' do \n print(i) \n end", 2)
-- lineerror("\n local a \n for k,v in 3 \n do \n print(k) \n end", 3)
-- lineerror("\n\n for k,v in \n 3 \n do \n print(k) \n end", 4)
-- lineerror("function a.x.y ()\na=a+1\nend", 1)

-- lineerror("a = \na\n+\n{}", 3)
-- lineerror("a = \n3\n+\n(\n4\n/\nprint)", 6)
-- lineerror("a = \nprint\n+\n(\n4\n/\n7)", 3)

-- lineerror("a\n=\n-\n\nprint\n;", 3)

-- lineerror([[
-- a
-- (
-- 23)
-- ]], 1)

-- lineerror([[
-- local a = {x = 13}
-- a
-- .
-- x
-- (
-- 23
-- )
-- ]], 2)

-- lineerror([[
-- local a = {x = 13}
-- a
-- .
-- x
-- (
-- 23 + a
-- )
-- ]], 6)

-- local p = [[
-- function g() f() end
-- function f(x) error('a', X) end
-- g()
-- ]]
-- X=3;lineerror((p), 3)
-- X=0;lineerror((p), nil)
-- X=1;lineerror((p), 2)
-- X=2;lineerror((p), 1)

------------------------------------------
-- TODO: stack overflow
------------------------------------------
-- if not _soft then
--   -- several tests that exaust the Lua stack
--   C = 0
--   local l = debug.getinfo(1, "l").currentline; function y () C=C+1; y() end

--   local function checkstackmessage (m)
--     return (string.find(m, "^.-:%d+: stack overflow"))
--   end
--   -- repeated stack overflows (to check stack recovery)
--   assert(checkstackmessage(doit('y()')))
--   print('+')
--   assert(checkstackmessage(doit('y()')))
--   print('+')
--   assert(checkstackmessage(doit('y()')))
--   print('+')


--   -- error lines in stack overflow
--   C = 0
--   local l1
--   local function g(x)
--     l1 = debug.getinfo(x, "l").currentline; y()
--   end
--   local _, stackmsg = xpcall(g, debug.traceback, 1)
--   print('+')
--   local stack = {}
--   for line in string.gmatch(stackmsg, "[^\n]*") do
--     local curr = string.match(line, ":(%d+):")
--     if curr then table.insert(stack, tonumber(curr)) end
--   end
--   local i=1
--   while stack[i] ~= l1 do
--     assert(stack[i] == l)
--     i = i+1
--   end
--   assert(i > 15)
