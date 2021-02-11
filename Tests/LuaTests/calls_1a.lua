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

