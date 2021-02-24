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
-- TODO: debug
------------------------------------------
-- local function getenv (f)
--   local a,b = debug.getupvalue(f, 1)
--   assert(a == '_ENV')
--   return b
-- end

-- -- test for global table of loaded chunks
-- assert(getenv(load"a=3") == _G)
local c = {}; local f = load("a = 3", nil, nil, c)
-- assert(getenv(f) == c)
assert(c.a == nil)
f()
assert(c.a == 3)

------------------------------------------
-- TODO: string.format
------------------------------------------
-- -- testing limits for special instructions

-- if not _soft then
--   local a
--   local p = 4
--   for i=2,31 do
--     for j=-3,3 do
--       assert(load(string.format([[local a=%s;
--                                         a=a+%s;
--                                         assert(a ==2^%s)]], j, p-j, i))) ()
--       assert(load(string.format([[local a=%s;
--                                         a=a-%s;
--                                         assert(a==-2^%s)]], -j, p-j, i))) ()
--       assert(load(string.format([[local a,b=0,%s;
--                                         a=b-%s;
--                                         assert(a==-2^%s)]], -j, p-j, i))) ()
--     end
--     p =2*p
--   end
-- end

print'+'

------------------------------------------
-- TODO: def. of querytab
------------------------------------------
-- if rawget(_G, "querytab") then
--   -- testing clearing of dead elements from tables
--   collectgarbage("stop")   -- stop GC
--   local a = {[{}] = 4, [3] = 0, alo = 1, 
--              a1234567890123456789012345678901234567890 = 10}

--   local t = querytab(a)

--   for k,_ in pairs(a) do a[k] = nil end
--   collectgarbage()   -- restore GC and collect dead fiels in `a'
--   for i=0,t-1 do
--     local k = querytab(a, i)
--     assert(k == nil or type(k) == 'number' or k == 'alo')
--   end
-- end


-- testing lexical environments

assert(_ENV == _G)

do local _ENV = (function (...) return ... end)(_G, dummy)

do local _ENV = {assert=assert}; assert(true) end
mt = {_G = _G}
local foo,x
do local _ENV = mt
  function foo (x)
    A = x
    do local _ENV =  _G; A = 1000 end
    return function (x) return A .. x end
  end
end

------------------------------------------
-- TODO: debug in getenv
------------------------------------------
-- assert(getenv(foo) == mt)
x = foo('hi'); assert(mt.A == 'hi' and A == 1000)
assert(x('*') == mt.A .. '*')

do local _ENV = {assert=assert, A=10};
  do local _ENV = {assert=assert, A=20};
    assert(A==20);x=A
  end
  assert(A==10 and x==20)
end
assert(x==20)


print('OK')

return 5,f

end

