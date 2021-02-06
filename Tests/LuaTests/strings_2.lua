-----------------------------------------------------------------
-- TODO: string.find
-----------------------------------------------------------------
-- assert(string.find("123456789", "345") == 3)
-- a,b = string.find("123456789", "345")
-- assert(string.sub("123456789", a, b) == "345")
-- assert(string.find("1234567890123456789", "345", 3) == 3)
-- assert(string.find("1234567890123456789", "345", 4) == 13)
-- assert(string.find("1234567890123456789", "346", 4) == nil)
-- assert(string.find("1234567890123456789", ".45", -9) == 13)
-- assert(string.find("abcdefg", "\0", 5, 1) == nil)
-- assert(string.find("", "") == 1)
-- assert(string.find("", "", 1) == 1)
-- assert(not string.find("", "", 2))
-- assert(string.find('', 'aaa', 1) == nil)
-- assert(('alo(.)alo'):find('(.)', 1, 1) == 4)
-- print('+')

assert(string.len("") == 0)
assert(string.len("\0\0\0") == 3)
assert(string.len("1234567890") == 10)

assert(#"" == 0)
assert(#"\0\0\0" == 3)
assert(#"1234567890" == 10)

-----------------------------------------------------------------
-- TODO: string.byte, string.char
-----------------------------------------------------------------
-- assert(string.byte("a") == 97)
-- assert(string.byte("\xe4") > 127)
-- assert(string.byte(string.char(255)) == 255)
-- assert(string.byte(string.char(0)) == 0)
-- assert(string.byte("\0") == 0)
-- assert(string.byte("\0\0alo\0x", -1) == string.byte('x'))
-- assert(string.byte("ba", 2) == 97)
-- assert(string.byte("\n\n", 2, -1) == 10)
-- assert(string.byte("\n\n", 2, 2) == 10)
-- assert(string.byte("") == nil)
-- assert(string.byte("hi", -3) == nil)
-- assert(string.byte("hi", 3) == nil)
-- assert(string.byte("hi", 9, 10) == nil)
-- assert(string.byte("hi", 2, 1) == nil)
-- assert(string.char() == "")
-- assert(string.char(0, 255, 0) == "\0\255\0")
-- assert(string.char(0, string.byte("\xe4"), 0) == "\0\xe4\0")
-- assert(string.char(string.byte("\xe4l\0óu", 1, -1)) == "\xe4l\0óu")
-- assert(string.char(string.byte("\xe4l\0óu", 1, 0)) == "")
-- assert(string.char(string.byte("\xe4l\0óu", -10, 100)) == "\xe4l\0óu")
-- print('+')

-----------------------------------------------------------------
-- TODO: string.upper, string.lower
-----------------------------------------------------------------
-- assert(string.upper("ab\0c") == "AB\0C")
-- assert(string.lower("\0ABCc%$") == "\0abcc%$")
assert(string.rep('teste', 0) == '')
assert(string.rep('tés\00tê', 2) == 'tés\0têtés\000tê')
assert(string.rep('', 10) == '')

-- repetitions with separator
assert(string.rep('teste', 0, 'xuxu') == '')
assert(string.rep('teste', 1, 'xuxu') == 'teste')
assert(string.rep('\1\0\1', 2, '\0\0') == '\1\0\1\0\0\1\0\1')
assert(string.rep('', 10, '.') == string.rep('.', 9))
---------------------------------------------------------
-- TODO: performance
---------------------------------------------------------
-- if not _no32 then
--   assert(not pcall(string.rep, "aa", 2^30))
--   assert(not pcall(string.rep, "", 2^30, "aa"))
-- end

assert(string.reverse"" == "")
assert(string.reverse"\0\1\2\3" == "\3\2\1\0")
assert(string.reverse"\0001234" == "4321\0")

--WAS: for i=0,30 do assert(string.len(string.rep('a', i)) == i) end
for i=0,10 do assert(string.len(string.rep('a', i)) == i) end

assert(type(tostring(nil)) == 'string')
assert(type(tostring(12)) == 'string')
assert(''..12 == '12' and type(12 .. '') == 'string')
-----------------------------------------------------------------
-- TODO: string.find
-----------------------------------------------------------------
-- assert(string.find(tostring{}, 'table:'))
-- assert(string.find(tostring(print), 'function:'))
assert(tostring(1234567890123) == '1234567890123')
assert(#tostring('\0') == 1)
assert(tostring(true) == "true")
assert(tostring(false) == "false")
print('+')
