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

local f = function (i)
  if i < 10 then return 'a'
  elseif i < 20 then return 'b'
  elseif i < 30 then return 'c'
  else return 8
  end
end

assert(f(3) == 'a' and f(12) == 'b' and f(26) == 'c' and f(100) == 8)

local a, b = nil, 23
x = {f(100)*2+3 or a, a or b+2}
assert(x[1] == 19 and x[2] == 25)
x = {f=2+3 or a, a = b+2}
assert(x.f == 5 and x.a == 25)

a={y=1}
x = {a.y}
assert(x[1] == 1)

------------------------------------------
-- TODO: string.gsub
------------------------------------------
-- f = [[
-- return function ( a , b , c , d , e )
--   local x = a >= b or c or ( d and e ) or nil
--   return x
-- end , { a = 1 , b = 2 >= 1 , } or { 1 };
-- ]]

-- f = string.gsub(f, "%s+", "\n");   -- force a SETLINE between opcodes
-- f,a = load(f)();
-- assert(a.a == 1 and a.b)

-- function g (a,b,c,d,e)
--   if not (a>=b or c or d and e or nil) then return 0; else return 1; end;
-- end

-- function h (a,b,c,d,e)
--   while (a>=b or c or (d and e) or nil) do return 1; end;
--   return 0;
-- end;

------------------------------------------
-- TODO: string.gsub in f
------------------------------------------
-- assert(f(2,1) == true and g(2,1) == 1 and h(2,1) == 1)
-- assert(f(1,2,'a') == 'a' and g(1,2,'a') == 1 and h(1,2,'a') == 1)
-- assert(f(1,2,'a')
-- ~=          -- force SETLINE before nil
-- nil, "")
-- assert(f(1,2,'a') == 'a' and g(1,2,'a') == 1 and h(1,2,'a') == 1)
-- assert(f(1,2,nil,1,'x') == 'x' and g(1,2,nil,1,'x') == 1 and
--                                    h(1,2,nil,1,'x') == 1)
-- assert(f(1,2,nil,nil,'x') == nil and g(1,2,nil,nil,'x') == 0 and
--                                      h(1,2,nil,nil,'x') == 0)
-- assert(f(1,2,nil,1,nil) == nil and g(1,2,nil,1,nil) == 0 and
--                                    h(1,2,nil,1,nil) == 0)

assert(1 and 2<3 == true and 2<3 and 'a'<'b' == true)
x = 2<3 and not 3; assert(x==false)
x = 2<1 or (2>1 and 'a'); assert(x=='a')


do
  local a; if nil then a=1; else a=2; end;    -- this nil comes as PUSHNIL 2
  assert(a==2)
end

------------------------------------------
-- TODO: debug and string functions
------------------------------------------
-- function F(a)
--   assert(debug.getinfo(1, "n").name == 'F')
--   return a,2,3
-- end

-- a,b = F(1)~=nil; assert(a == true and b == nil);
-- a,b = F(nil)==nil; assert(a == true and b == nil)

----------------------------------------------------------------
-- creates all combinations of 
-- [not] ([not] arg op [not] (arg op [not] arg ))
-- and tests each one

-- function ID(x) return x end

-- function f(t, i)
--   local b = t.n
--   local res = math.fmod(math.floor(i/c), b)+1
--   c = c*b
--   return t[res]
-- end

-- local arg = {" ( 1 < 2 ) ", " ( 1 >= 2 ) ", " F ( ) ", "  nil "; n=4}

-- local op = {" and ", " or ", " == ", " ~= "; n=4}

-- local neg = {" ", " not "; n=2}

-- local i = 0
------------------------------------------
-- TODO: string.gsub
------------------------------------------
-- repeat
--   c = 1
--   local s = f(neg, i)..'ID('..f(neg, i)..f(arg, i)..f(op, i)..
--             f(neg, i)..'ID('..f(arg, i)..f(op, i)..f(neg, i)..f(arg, i)..'))'
--   local s1 = string.gsub(s, 'ID', '')
--   K,X,NX,WX1,WX2 = nil
--   s = string.format([[
--       local a = %s
--       local b = not %s
--       K = b
--       local xxx; 
--       if %s then X = a  else X = b end
--       if %s then NX = b  else NX = a end
--       while %s do WX1 = a; break end
--       while %s do WX2 = a; break end
--       repeat if (%s) then break end; assert(b)  until not(%s)
--   ]], s1, s, s1, s, s1, s, s1, s, s)
--   assert(load(s))()
--   assert(X and not NX and not WX1 == K and not WX2 == K)
--   if math.fmod(i,4000) == 0 then print('+') end
--   i = i+1
-- until i==c

print '+'

------------------------------------------------------------------
