-- -- Copyright © 1994–2019 Lua.org, PUC-Rio.

-- -- Permission is hereby granted, free of charge, to any person obtaining a copy 
-- -- of this software and associated documentation files (the "Software"), to deal 
-- -- in the Software without restriction, including without limitation the rights 
-- -- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
-- -- copies of the Software, and to permit persons to whom the Software is 
-- -- furnished to do so, subject to the following conditions:

-- -- The above copyright notice and this permission notice shall be included in 
-- -- all copies or substantial portions of the Software.

-- -- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- -- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
-- -- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
-- -- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
-- -- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
-- -- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
-- -- SOFTWARE. 

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
