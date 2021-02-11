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

-- silly loops
repeat until 1; repeat until true;
while false do end; while nil do end;

do  -- test old bug (first name could not be an `upvalue')
 local a; function f(x) x={a=1}; x={x=1}; x={G=1} end
end

function f (i)
  if type(i) ~= 'number' then return i,'jojo'; end;
  if i > 0 then return i, f(i-1); end;
end

x = {f(3), f(5), f(10);};
assert(x[1] == 3 and x[2] == 5 and x[3] == 10 and x[4] == 9 and x[12] == 1);
assert(x[nil] == nil)
x = {f'alo', f'xixi', nil};
assert(x[1] == 'alo' and x[2] == 'xixi' and x[3] == nil);
x = {f'alo'..'xixi'};
assert(x[1] == 'aloxixi')
x = {f{}}
assert(x[2] == 'jojo' and type(x[1]) == 'table')


local f = function (i)
  if i < 10 then return 'a';
  elseif i < 20 then return 'b';
  elseif i < 30 then return 'c';
  end;
end

assert(f(3) == 'a' and f(12) == 'b' and f(26) == 'c' and f(100) == nil)
