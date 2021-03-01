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

-- testing 'tonumber'
assert(tonumber{} == nil)
assert(tonumber'+0.01' == 1/100 and tonumber'+.01' == 0.01 and
       tonumber'.01' == 0.01    and tonumber'-1.' == -1 and
       tonumber'+1.' == 1)
assert(tonumber'+ 0.01' == nil and tonumber'+.e1' == nil and
       tonumber'1e' == nil     and tonumber'1.0e+' == nil and
       tonumber'.' == nil)
assert(tonumber('-012') == -010-2)
assert(tonumber('-1.2e2') == - - -120)

assert(tonumber("0xffffffffffff") == 2^(4*12) - 1)
assert(tonumber("0x"..string.rep("f", 150)) == 2^(4*150) - 1)
assert(tonumber('0x3.' .. string.rep('0', 100)) == 3)
assert(tonumber('0x0.' .. string.rep('0', 150).."1") == 2^(-4*151))

-- testing 'tonumber' with base
assert(tonumber('  001010  ', 2) == 10)
assert(tonumber('  001010  ', 10) == 001010)
assert(tonumber('  -1010  ', 2) == -10)
assert(tonumber('10', 36) == 36)
assert(tonumber('  -10  ', 36) == -36)
assert(tonumber('  +1Z  ', 36) == 36 + 35)
assert(tonumber('  -1z  ', 36) == -36 + -35)
assert(tonumber('-fFfa', 16) == -(10+(16*(15+(16*(15+(16*15)))))))
assert(tonumber(string.rep('1', 42), 2) + 1 == 2^42)
assert(tonumber(string.rep('1', 34), 2) + 1 == 2^34)
assert(tonumber('ffffFFFF', 16)+1 == 2^32)
assert(tonumber('0ffffFFFF', 16)+1 == 2^32)
assert(tonumber('-0ffffffFFFF', 16) - 1 == -2^40)
for i = 2,36 do
  assert(tonumber('\t10000000000\t', i) == i^10)
end

function f(...)
  if select('#', ...) == 1 then
    return (...)
  else
    return "***"
  end
end

-- testing 'tonumber' fo invalid formats
assert(f(tonumber('fFfa', 15)) == nil)
assert(f(tonumber('099', 8)) == nil)
assert(f(tonumber('1\0', 2)) == nil)
assert(f(tonumber('', 8)) == nil)
assert(f(tonumber('  ', 9)) == nil)
assert(f(tonumber('  ', 9)) == nil)
assert(f(tonumber('0xf', 10)) == nil)

assert(f(tonumber('inf')) == nil)
assert(f(tonumber(' INF ')) == nil)
assert(f(tonumber('Nan')) == nil)
assert(f(tonumber('nan')) == nil)

assert(f(tonumber('  ')) == nil)
assert(f(tonumber('')) == nil)
assert(f(tonumber('1  a')) == nil)
assert(f(tonumber('1\0')) == nil)
assert(f(tonumber('1 \0')) == nil)
assert(f(tonumber('1\0 ')) == nil)
assert(f(tonumber('e1')) == nil)
assert(f(tonumber('e  1')) == nil)
assert(f(tonumber(' 3.4.5 ')) == nil)


-- testing 'tonumber' for invalid hexadecimal formats

assert(tonumber('0x') == nil)
assert(tonumber('x') == nil)
assert(tonumber('x3') == nil)
assert(tonumber('00x2') == nil)
assert(tonumber('0x 2') == nil)
assert(tonumber('0 x2') == nil)
assert(tonumber('23x') == nil)
assert(tonumber('- 0xaa') == nil)
