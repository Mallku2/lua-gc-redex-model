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

-- testing errors during GC
do
collectgarbage("stop")   -- stop collection
local u = {}
local s = {}; setmetatable(s, {__mode = 'k'})
setmetatable(u, {__gc = function (o)
  local i = s[o]
  s[i] = true
  assert(not s[i - 1])   -- check proper finalization order
  if i == 8 then error("here") end   -- error during GC
end})

for i = 6, 10 do
  local n = setmetatable({}, getmetatable(u))
  s[n] = i
end

-- WAS: nothing, we add explicit calls to the garbage collector
-- since we finalize one table in each call
assert(pcall(collectgarbage))
assert(pcall(collectgarbage))
---------------------------------------------------------------

assert(not pcall(collectgarbage))

-- WAS: nothing, we add explicit calls to the garbage collector
-- since we finalize one table in each call
assert(pcall(collectgarbage))
assert(pcall(collectgarbage))
---------------------------------------------------------------

for i = 8, 10 do assert(s[i]) end

for i = 1, 5 do
  local n = setmetatable({}, getmetatable(u))
  s[n] = i
end

-- WAS: nothing, we add explicit calls to the garbage collector
-- since we finalize one table in each call
collectgarbage()
collectgarbage()
collectgarbage()
collectgarbage()
---------------------------------------------------------------

collectgarbage()
for i = 1, 10 do assert(s[i]) end

getmetatable(u).__gc = false

------------------------------------------
-- TODO: string.find
------------------------------------------
-- -- __gc errors with non-string messages
-- setmetatable({}, {__gc = function () error{} end})
-- local a, b = pcall(collectgarbage)
-- assert(not a and type(b) == "string" and string.find(b, "error in __gc"))

end
print '+'


------------------------------------------
-- TODO: userdata
------------------------------------------
-- -- testing userdata
-- if T==nil then
--   (Message or print)('\a\n >>> testC not active: skipping userdata GC tests <<<\n\a')

-- else

--   local function newproxy(u)
--     return debug.setmetatable(T.newuserdata(0), debug.getmetatable(u))
--   end

--   collectgarbage("stop")   -- stop collection
--   local u = newproxy(nil)
--   debug.setmetatable(u, {__gc = true})
--   local s = 0
--   local a = {[u] = 0}; setmetatable(a, {__mode = 'vk'})
--   for i=1,10 do a[newproxy(u)] = i end
--   for k in pairs(a) do assert(getmetatable(k) == getmetatable(u)) end
--   local a1 = {}; for k,v in pairs(a) do a1[k] = v end
--   for k,v in pairs(a1) do a[v] = k end
--   for i =1,10 do assert(a[i]) end
--   getmetatable(u).a = a1
--   getmetatable(u).u = u
--   do
--     local u = u
--     getmetatable(u).__gc = function (o)
--       assert(a[o] == 10-s)
--       assert(a[10-s] == nil) -- udata already removed from weak table
--       assert(getmetatable(o) == getmetatable(u))
--     assert(getmetatable(o).a[o] == 10-s)
--       s=s+1
--     end
--   end
--   a1, u = nil
--   assert(next(a) ~= nil)
--   collectgarbage()
--   assert(s==11)
--   collectgarbage()
--   assert(next(a) == nil)  -- finalized keys are removed in two cycles
-- end

------------------------------------------
-- TODO: os.exit
------------------------------------------
-- -- __gc x weak tables
-- local u = setmetatable({}, {__gc = true})
-- -- __gc metamethod should be collected before running
-- setmetatable(getmetatable(u), {__mode = "v"})
-- getmetatable(u).__gc = function (o) os.exit(1) end  -- cannot happen
-- u = nil
-- collectgarbage()

-- local u = setmetatable({}, {__gc = true})
-- local m = getmetatable(u)
-- m.x = {[{0}] = 1; [0] = {1}}; setmetatable(m.x, {__mode = "kv"});
-- m.__gc = function (o)
--   assert(next(getmetatable(o).x) == nil)
--   m = 10
-- end
-- u, m = nil
-- collectgarbage()
-- assert(m==10)


-- errors during collection
u = setmetatable({}, {__gc = function () error "!!!" end})
u = nil
assert(not pcall(collectgarbage))

------------------------------------------
-- TODO: performance
------------------------------------------
-- if not _soft then
--   print("deep structures")
--   local a = {}
--   for i = 1,200000 do
--     a = {next = a}
--   end
--   collectgarbage()
-- end

------------------------------------------
-- TODO: coroutine
------------------------------------------
-- -- create many threads with self-references and open upvalues
-- local thread_id = 0
-- local threads = {}

-- local function fn (thread)
--     local x = {}
--     threads[thread_id] = function()
--                              thread = x
--                          end
--     coroutine.yield()
-- end

-- while thread_id < 1000 do
--     local thread = coroutine.create(fn)
--     coroutine.resume(thread, thread)
--     thread_id = thread_id + 1
-- end

-- do
--   collectgarbage()
--   collectgarbage"stop"
--   local x = gcinfo()
--   repeat
--     for i=1,1000 do _ENV.a = {} end
--     collectgarbage("step", 1)   -- steps should not unblock the collector
--   until gcinfo() > 2 * x
--   collectgarbage"restart"
-- end


-- if T then   -- tests for weird cases collecting upvalues
--   local a = 1200
--   local f = function () return a end    -- create upvalue for 'a'
--   assert(f() == 1200)

--   -- erase reference to upvalue 'a', mark it as dead, but does not collect it
--   T.gcstate("pause"); collectgarbage("stop")
--   f = nil
--   T.gcstate("sweepstring")

--   -- this function will reuse that dead upvalue...
--   f = function () return a end
--   assert(f() == 1200)

--   -- create coroutine with local variable 'b'
--   local co = coroutine.wrap(function()
--     local b = 150
--     coroutine.yield(function () return b end)
--   end)

--   T.gcstate("pause")
--   assert(co()() == 150)  -- create upvalue for 'b'

--   -- mark upvalue 'b' as dead, but does not collect it
--   T.gcstate("sweepstring")

--   co()   -- finish coroutine, "closing" that dead upvalue

--   assert(f() == 1200)
--   collectgarbage("restart")

--   print"+"
-- end


-- if T then
--   local debug = require "debug"
--   collectgarbage("generational"); collectgarbage("stop")
--   x = T.newuserdata(0)
--   T.gcstate("propagate")    -- ensure 'x' is old
--   T.gcstate("sweepstring")
--   T.gcstate("propagate")
--   assert(string.find(T.gccolor(x), "/old"))
--   local y = T.newuserdata(0)
--   debug.setmetatable(y, {__gc = true})   -- bless the new udata before...
--   debug.setmetatable(x, {__gc = true})   -- ...the old one
--   assert(string.find(T.gccolor(y), "white"))
--   T.checkmemory()
--   collectgarbage("incremental"); collectgarbage("restart")
-- end


-- if T then
--   print("emergency collections")
--   collectgarbage()
--   collectgarbage()
--   T.totalmem(T.totalmem() + 200)
--   for i=1,200 do local a = {} end
--   T.totalmem(1000000000)
--   collectgarbage()
--   local t = T.totalmem("table")
--   local a = {{}, {}, {}}   -- create 4 new tables
--   assert(T.totalmem("table") == t + 4)
--   t = T.totalmem("function")
--   a = function () end   -- create 1 new closure
--   assert(T.totalmem("function") == t + 1)
--   t = T.totalmem("thread")
--   a = coroutine.create(function () end)   -- create 1 new coroutine
--   assert(T.totalmem("thread") == t + 1)
-- end

-- -- create an object to be collected when state is closed
-- do
--   local setmetatable,assert,type,print,getmetatable =
--         setmetatable,assert,type,print,getmetatable
--   local tt = {}
--   tt.__gc = function (o)
--     assert(getmetatable(o) == tt)
--     -- create new objects during GC
--     local a = 'xuxu'..(10+3)..'joao', {}
--     ___Glob = o  -- ressurect object!
--     setmetatable({}, tt)  -- creates a new one with same metatable
--     print(">>> closing state " .. "<<<\n")
--   end
--   local u = setmetatable({}, tt)
--   ___Glob = {u}   -- avoid object being collected before program end
-- end

-- -- create several objects to raise errors when collected while closing state
-- do
--   local mt = {__gc = function (o) return o + 1 end}
--   for i = 1,10 do
--     -- create object and preserve it until the end
--     table.insert(___Glob, setmetatable({}, mt))
--   end
-- end

-- -- just to make sure
-- assert(collectgarbage'isrunning')

print('OK')
