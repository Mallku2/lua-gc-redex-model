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

------------------------------------------------------
-- TODO: these tests do not imply the exercising of our mechanization of GC: we do
-- not treat strings as Lua's GC does
print('long strings')
x = "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
assert(string.len(x)==80)


s = ''
n = 0
k = 10 -- WAS: 300, changed for performance reasons
while n < k do s = s..x; n=n+1; j=tostring(n)  end
assert(string.len(s) == k*80)
------------------------------------------------------
------------------------------------------
-- TODO: string.gsub
------------------------------------------
-- s = string.sub(s, 1, 20000)
-- s, i = string.gsub(s, '(%d%d%d%d)', math.sin)
-- assert(i==20000/4)
-- s = nil
-- x = nil

-- assert(_G["while"] == 234)

------------------------------------------
-- TODO: collectgarbage parameters
------------------------------------------
-- local k,b = collectgarbage("count")
-- assert(k*1024 == math.floor(k)*1024 + b)

-- print("steps")

-- local bytes = gcinfo()
-- while 1 do
--   local nbytes = gcinfo()
--   if nbytes < bytes then break end   -- run until gc
--   bytes = nbytes
--   a = {}
-- end

-- print("steps (2)")

-- local function dosteps (siz)
--   assert(not collectgarbage("isrunning"))
--   collectgarbage()
--   assert(not collectgarbage("isrunning"))
--   local a = {}
--   for i=1,100 do a[i] = {{}}; local b = {} end
--   local x = gcinfo()
--   local i = 0
--   repeat   -- do steps until it completes a collection cycle
--     i = i+1
--   until collectgarbage("step", siz)
--   assert(gcinfo() < x)
--   return i
-- end

-- collectgarbage"stop"

-- if not _port then
--   -- test the "size" of basic GC steps (whatever they mean...)
--   assert(dosteps(0) > 10)
--   assert(dosteps(10) < dosteps(2))
-- end

-- -- collector should do a full collection with so many steps
-- assert(dosteps(100000) == 1)
-- assert(collectgarbage("step", 1000000) == true)
-- assert(collectgarbage("step", 1000000) == true)

-- assert(not collectgarbage("isrunning"))
-- collectgarbage"restart"
-- assert(collectgarbage("isrunning"))


-- if not _port then
--   -- test the pace of the collector
--   collectgarbage(); collectgarbage()
--   local x = gcinfo()
--   collectgarbage"stop"
--   assert(not collectgarbage("isrunning"))
--   repeat
--     local a = {}
--   until gcinfo() > 3 * x
--   collectgarbage"restart"
--   assert(collectgarbage("isrunning"))
--   repeat
--     local a = {}
--   until gcinfo() <= x * 2
-- end


print("clearing tables")
lim = 15
a = {}
-- fill a with `collectable' indices
for i=1,lim do a[{}] = i end
b = {}
for k,v in pairs(a) do b[k]=v end
-- remove all indices and collect them
for n in pairs(b) do
  a[n] = nil
  assert(type(n) == 'table' and next(n) == nil)
  collectgarbage()
end
b = nil
collectgarbage()
for n in pairs(a) do error'cannot be here' end
for i=1,lim do a[i] = i end
-- NOTE: this test is not exercising our mechanization
for i=1,lim do assert(a[i] == i) end
