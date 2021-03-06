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

-- testing hexadecimal numerals

assert(0x10 == 16 and 0xfff == 2^12 - 1 and 0XFB == 251)
assert(0x0p12 == 0 and 0x.0p-3 == 0)
assert(0xFFFFFFFF == 2^32 - 1)
assert(tonumber('+0x2') == 2)
assert(tonumber('-0xaA') == -170)
assert(tonumber('-0xffFFFfff') == -2^32 + 1)

-- possible confusion with decimal exponent
assert(0E+1 == 0 and 0xE+1 == 15 and 0xe-1 == 13)


-- floating hexas

assert(tonumber('  0x2.5  ') == 0x25/16)
assert(tonumber('  -0x2.5  ') == -0x25/16)
assert(tonumber('  +0x0.51p+8  ') == 0x51)
assert(tonumber('0x0.51p') == nil)
assert(tonumber('0x5p+-2') == nil)
assert(0x.FfffFFFF == 1 - '0x.00000001')
assert('0xA.a' + 0 == 10 + 10/16)
assert(0xa.aP4 == 0XAA)
assert(0x4P-2 == 1)
assert(0x1.1 == '0x1.' + '+0x.1')


assert(1.1 == 1.+.1)
assert(100.0 == 1E2 and .01 == 1e-2)
assert(1111111111111111-1111111111111110== 1000.00e-03)
--     1234567890123456
assert(1.1 == '1.'+'.1')
assert('1111111111111111'-'1111111111111110' == tonumber"  +0.001e+3 \n\t")

assert(0.1e-30 > 0.9E-31 and 0.9E30 < 0.1e31)

assert(0.123456 > 0.123455)

assert(tonumber('+1.23E18') == 1.23*10^18)

-- testing order operators
assert(not(1<1) and (1<2) and not(2<1))
assert(not('a'<'a') and ('a'<'b') and not('b'<'a'))
assert((1<=1) and (1<=2) and not(2<=1))
assert(('a'<='a') and ('a'<='b') and not('b'<='a'))
assert(not(1>1) and not(1>2) and (2>1))
assert(not('a'>'a') and not('a'>'b') and ('b'>'a'))
assert((1>=1) and not(1>=2) and (2>=1))
assert(('a'>='a') and not('a'>='b') and ('b'>='a'))
