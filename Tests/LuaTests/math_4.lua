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

-- testing mod operator
assert(-4%3 == 2)
assert(4%-3 == -2)
assert(math.pi - math.pi % 1 == 3)
assert(math.pi - math.pi % 0.001 == 3.141)

local function testbit(a, n)
  return a/2^n % 2 >= 1
end

function eq (a,b,limit)
  if not limit then limit = 10E-10 end
  return math.abs(a-b) <= limit
end

assert(eq(math.sin(-9.8)^2 + math.cos(-9.8)^2, 1))
assert(eq(math.tan(math.pi/4), 1))
assert(eq(math.sin(math.pi/2), 1) and eq(math.cos(math.pi/2), 0))
assert(eq(math.atan(1), math.pi/4) and eq(math.acos(0), math.pi/2) and
       eq(math.asin(1), math.pi/2))
assert(eq(math.deg(math.pi/2), 90) and eq(math.rad(90), math.pi/2))
assert(math.abs(-10) == 10)
------------------------------------------
-- TODO: match.atan2
------------------------------------------
-- assert(eq(math.atan2(1,0), math.pi/2))
assert(math.ceil(4.5) == 5.0)
assert(math.floor(4.5) == 4.0)
assert(math.fmod(10,3) == 1)
assert(eq(math.sqrt(10)^2, 10))
assert(eq(math.log(2, 10), math.log(2)/math.log(10)))
assert(eq(math.log(2, 2), 1))
assert(eq(math.log(9, 3), 2))
assert(eq(math.exp(0), 1))
assert(eq(math.sin(10), math.sin(10%(2*math.pi))))
------------------------------------------
-- TODO: match.frexp, math.ldexp
------------------------------------------
-- local v,e = math.frexp(math.pi)
-- assert(eq(math.ldexp(v,e), math.pi))

assert(eq(math.tanh(3.5), math.sinh(3.5)/math.cosh(3.5)))

assert(tonumber(' 1.3e-2 ') == 1.3e-2)
assert(tonumber(' -1.00000000000001 ') == -1.00000000000001)

-- testing constant limits
-- 2^23 = 8388608
assert(8388609 + -8388609 == 0)
assert(8388608 + -8388608 == 0)
assert(8388607 + -8388607 == 0)
