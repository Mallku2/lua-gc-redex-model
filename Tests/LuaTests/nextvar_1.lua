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
print('testing tables, next, and for')

local a = {}

-- make sure table has lots of space in hash part
for i=1,20 do a[i.."+"] = true end --WAS: for i=1,100 do a[i.."+"] = true end
for i=1,20 do a[i.."+"] = nil end  --WAS: for i=1,100 do a[i.."+"]
-- fill hash part with numeric indices testing size operator
for i=1,20 do --WAS: for i=1,100 do
  a[i] = true
  assert(#a == i)
end

-- testing ipairs
local x = 0
for k,v in ipairs{10,20,30;x=12} do
  x = x + 1
  assert(k == x and v == x * 10)
end

for _ in ipairs{x=12, y=24} do assert(nil) end

-- test for 'false' x ipair
x = false
local i = 0
for k,v in ipairs{true,false,true,false} do
  i = i + 1
  x = not x
  assert(x == v)
end
assert(i == 4)

-- iterator function is always the same
assert(type(ipairs{}) == 'function' and ipairs{} == ipairs{})

if T then  --[
-- testing table sizes

------------------------------------------
-- TODO: the following functions are unused
------------------------------------------
-- local function log2 (x) return math.log(x, 2) end

-- local function mp2 (n)   -- minimum power of 2 >= n
--   local mp = 2^math.ceil(log2(n))
--   assert(n == 0 or (mp/2 < n and n <= mp))
--   return mp
-- end

------------------------------------------
-- TODO: def. of int2fb
------------------------------------------
-- local function fb (n)
--   local r, nn = T.int2fb(n)
--   assert(r < 256)
--   return nn
-- end

-- -- test fb function
-- local a = 1
-- local lim = 2^30
-- while a < lim do
--   local n = fb(a)
--   assert(a <= n and n <= a*1.125)
--   a = math.ceil(a*1.3)
-- end

------------------------------------------
-- TODO: def. of querytab
------------------------------------------
-- local function check (t, na, nh)
--   local a, h = T.querytab(t)
--   if a ~= na or h ~= nh then
--     print(na, nh, a, h)
--     assert(nil)
--   end
-- end


-- -- testing C library sizes
-- do
--   local s = 0
--   for _ in pairs(math) do s = s + 1 end
--   check(math, 0, mp2(s))
-- end

------------------------------------------
-- TODO: string.format
------------------------------------------
-- -- testing constructor sizes
-- local lim = 40
-- local s = 'return {'
-- for i=1,lim do
--   s = s..i..','
--   local s = s
--   for k=0,lim do 
--     local t = load(s..'}')()
--     assert(#t == i)
--     check(t, fb(i), mp2(k))
--     s = string.format('%sa%d=%d,', s, k, k)
--   end
-- end

------------------------------------------
-- TODO: performance (lim = 2^30), check function
------------------------------------------
-- -- tests with unknown number of elements
-- local a = {}
-- for i=1,lim do a[i] = i end   -- build auxiliary table
-- for k=0,lim do
--   local a = {table.unpack(a,1,k)}
--   assert(#a == k)
--   check(a, k, 0)
--   a = {1,2,3,table.unpack(a,1,k)}
--   check(a, k+3, 0)
--   assert(#a == k + 3)
-- end


-- -- testing tables dynamically built
-- local lim = 130
-- local a = {}; a[2] = 1; check(a, 0, 1)
-- a = {}; a[0] = 1; check(a, 0, 1); a[2] = 1; check(a, 0, 2)
-- a = {}; a[0] = 1; a[1] = 1; check(a, 1, 1)
-- a = {}
-- for i = 1,lim do
--   a[i] = 1
--   assert(#a == i)
--   check(a, mp2(i), 0)
-- end

-- a = {}
-- for i = 1,lim do
--   a['a'..i] = 1
--   assert(#a == 0)
--   check(a, 0, mp2(i))
-- end

-- a = {}
-- for i=1,16 do a[i] = i end
-- check(a, 16, 0)
-- if not _port then
--   for i=1,11 do a[i] = nil end
--   for i=30,50 do a[i] = nil end   -- force a rehash (?)
--   check(a, 0, 8)   -- only 5 elements in the table
--   a[10] = 1
--   for i=30,50 do a[i] = nil end   -- force a rehash (?)
--   check(a, 0, 8)   -- only 6 elements in the table
--   for i=1,14 do a[i] = nil end
--   for i=18,50 do a[i] = nil end   -- force a rehash (?)
--   check(a, 0, 4)   -- only 2 elements ([15] and [16])
-- end

-- -- reverse filling
-- for i=1,lim do
--   local a = {}
--   for i=i,1,-1 do a[i] = i end   -- fill in reverse
--   check(a, mp2(i), 0)
-- end

-- -- size tests for vararg
-- lim = 35
-- function foo (n, ...)
--   local arg = {...}
--   check(arg, n, 0)
--   assert(select('#', ...) == n)
--   arg[n+1] = true
--   check(arg, mp2(n+1), 0)
--   arg.x = true
--   check(arg, mp2(n+1), 1)
-- end
-- local a = {}
-- for i=1,lim do a[i] = true; foo(i, table.unpack(a)) end

end  --]
