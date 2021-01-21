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

print("testing functions and calls")

------------------------------------------
-- TODO: debug library
------------------------------------------
-- local debug = require "debug"

-- get the opportunity to test 'type' too ;)

assert(type(1<2) == 'boolean')
assert(type(true) == 'boolean' and type(false) == 'boolean')
assert(type(nil) == 'nil' and type(-3) == 'number' and type'x' == 'string' and
       type{} == 'table' and type(type) == 'function')

assert(type(assert) == type(print))
f = nil
function f (x) return a:x (x) end
assert(type(f) == 'function')


-- testing local-function recursion
fact = false
do
  local res = 1
  local function fact (n)
    if n==0 then return res
    else return n*fact(n-1)
    end
  end
  assert(fact(5) == 120)
end
assert(fact == false)

-- testing declarations
a = {i = 10}
self = 20
function a:x (x) return x+self.i end
function a.y (x) return x+self end

assert(a:x(1)+10 == a.y(1))

a.t = {i=-100}
a["t"].x = function (self, a,b) return self.i+a+b end

assert(a.t:x(2,3) == -95)

do
  local a = {x=0}
  function a:add (x) self.x, a.y = self.x+x, 20; return self end
  assert(a:add(10):add(20):add(30).x == 60 and a.y == 20)
end

local a = {b={c={}}}

function a.b.c.f1 (x) return x+1 end
function a.b.c:f2 (x,y) self[x] = y end
assert(a.b.c.f1(4) == 5)
a.b.c:f2('k', 12); assert(a.b.c.k == 12)

print('+')

t = nil   -- 'declare' t
function f(a,b,c) local d = 'a'; t={a,b,c,d} end

f(      -- this line change must be valid
  1,2)
assert(t[1] == 1 and t[2] == 2 and t[3] == nil and t[4] == 'a')
f(1,2,   -- this one too
      3,4)
assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t[4] == 'a')

function fat(x)
  if x <= 1 then return 1
  else return x*load("return fat(" .. x-1 .. ")")()
  end
end

assert(load "load 'assert(fat(6)==720)' () ")()
a = load('return fat(5), 3')
a,b = a()
assert(a == 120 and b == 3)
print('+')


------------------------------------------
-- TODO: exit
------------------------------------------
-- function err_on_n (n)
--   if n==0 then error(); exit(1);
--   else err_on_n (n-1); exit(1);
--   end
-- end

-- do
--   function dummy (n)
--     if n > 0 then
--       assert(not pcall(err_on_n, n))
--       dummy(n-1)
--     end
--   end
-- end

-- dummy(10)

------------------------------------------
-- TODO: performance
------------------------------------------
-- function deep (n)
--   if n>0 then deep(n-1) end
-- end
-- deep(10)
-- deep(200)

-- testing tail call
-- function deep (n) if n>0 then return deep(n-1) else return 101 end end
-- assert(deep(30000) == 101)
-- a = {}
-- function a:deep (n) if n>0 then return self:deep(n-1) else return 101 end end
-- assert(a:deep(30000) == 101)

print('+')


a = nil
(function (x) a=x end)(23)
assert(a == 23 and (function (x) return x*2 end)(20) == 40)
