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

-- 'maxn' is now deprecated, but it is easily defined in Lua
function table.maxn (t)
  local max = 0
  for k in pairs(t) do
    max = (type(k) == 'number') and math.max(max, k) or max
  end
  return max
end

assert(table.maxn{} == 0)
assert(table.maxn{["1000"] = true} == 0)
assert(table.maxn{["1000"] = true, [24.5] = 3} == 24.5)
assert(table.maxn{[1000] = true} == 1000)
assert(table.maxn{[10] = true, [100*math.pi] = print} == 100*math.pi)

table.maxn = nil

-----------------------------------------
-- TODO: math.pow
-----------------------------------------
-- -- int overflow
-- a = {}
-- for i=0,50 do a[math.pow(2,i)] = true end
-- assert(a[#a])

print('+')


-- erasing values
local t = {[{1}] = 1, [{2}] = 2, [string.rep("x ", 4)] = 3,
           [100.3] = 4, [4] = 5}

local n = 0
for k, v in pairs( t ) do
  n = n+1
  assert(t[k] == v)
  t[k] = nil
  collectgarbage()
  assert(t[k] == nil)
end
assert(n == 5)


------------------------------------------
-- TODO: table.remove
------------------------------------------
local function test (a)
  assert(not pcall(table.insert, a, 2, 20));
  table.insert(a, 10); table.insert(a, 2, 20);
  table.insert(a, 1, -1); table.insert(a, 40);
  table.insert(a, #a+1, 50)
  table.insert(a, 2, -2)
  assert(not pcall(table.insert, a, 0, 20));
  assert(not pcall(table.insert, a, #a + 2, 20));
  -- assert(table.remove(a,1) == -1)
  -- assert(table.remove(a,1) == -2)
  -- assert(table.remove(a,1) == 10)
  -- assert(table.remove(a,1) == 20)
  -- assert(table.remove(a,1) == 40)
  -- assert(table.remove(a,1) == 50)
  -- assert(table.remove(a,1) == nil)
  -- assert(table.remove(a) == nil)
  -- assert(table.remove(a, #a) == nil)
end

a = {n=0, [-7] = "ban"}
test(a)
assert(a.n == 0 and a[-7] == "ban")

a = {[-7] = "ban"};
test(a)
assert(a.n == nil and #a == 6 and a[-7] == "ban")
--WAS: assert(a.n == nil and #a == 0 and a[-7] == "ban")

a = {[-1] = "ban"}
test(a)
assert(#a == 6 and a[-1] == "ban")
--WAS: assert(#a == 0 and table.remove(a) == nil and a[-1] == "ban")


a = {[0] = "ban"}
assert(#a == 0 and a[0] == "ban")
--WAS: assert(#a == 0 and table.remove(a) == "ban" and a[0] == nil)

table.insert(a, 1, 10); table.insert(a, 1, 20); table.insert(a, 1, -1)
-- assert(table.remove(a) == 10)
-- assert(table.remove(a) == 20)
-- assert(table.remove(a) == -1)
-- assert(table.remove(a) == nil)

a = {'c', 'd'}
table.insert(a, 3, 'a')
table.insert(a, 'b')
-- assert(table.remove(a, 1) == 'c')
-- assert(table.remove(a, 1) == 'd')
-- assert(table.remove(a, 1) == 'a')
-- assert(table.remove(a, 1) == 'b')
-- assert(table.remove(a, 1) == nil)
assert(#a == 4 and a.n == nil)
-- WAS: assert(#a == 0 and a.n == nil)

a = {10,20,30,40}
-- assert(table.remove(a, #a + 1) == nil)
-- assert(not pcall(table.remove, a, 0))
assert(a[#a] == 40)
-- assert(table.remove(a, #a) == 40)
-- assert(a[#a] == 30)
-- assert(table.remove(a, 2) == 20)
-- assert(a[#a] == 30 and #a == 2)
print('+')
