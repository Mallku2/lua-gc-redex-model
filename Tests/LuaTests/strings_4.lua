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

if not _no32 then
  assert(table.concat({}, "x", 2^31-1, 2^31-2) == "")
  assert(table.concat({}, "x", -2^31+1, -2^31) == "")
  assert(table.concat({}, "x", 2^31-1, -2^31) == "")
  assert(table.concat({[2^31-1] = "alo"}, "x", 2^31-1, 2^31-1) == "alo")
end

assert(not pcall(table.concat, {"a", "b", {}}))

a = {"a","b","c"}
assert(table.concat(a, ",", 1, 0) == "")
assert(table.concat(a, ",", 1, 1) == "a")
assert(table.concat(a, ",", 1, 2) == "a,b")
assert(table.concat(a, ",", 2) == "b,c")
assert(table.concat(a, ",", 3) == "c")
assert(table.concat(a, ",", 4) == "")

-------------------------------------------------------------------------
-- TODO: locales
-------------------------------------------------------------------------
-- if not _port then

-- local locales = { "ptb", "ISO-8859-1", "pt_BR" }
-- local function trylocale (w)
--   for i = 1, #locales do
--     if os.setlocale(locales[i], w) then return true end
--   end
--   return false
-- end

-- if not trylocale("collate")  then
--   print("locale not supported")
-- else
--   assert("alo" < "álo" and "álo" < "amo")
-- end

-- if not trylocale("ctype") then
--   print("locale not supported")
-- else
--   assert(load("a = 3.4"));  -- parser should not change outside locale
--   assert(not load("á = 3.4"));  -- even with errors
--   assert(string.gsub("áéíóú", "%a", "x") == "xxxxx")
--   assert(string.gsub("áÁéÉ", "%l", "x") == "xÁxÉ")
--   assert(string.gsub("áÁéÉ", "%u", "x") == "áxéx")
--   assert(string.upper"áÁé{xuxu}ção" == "ÁÁÉ{XUXU}ÇÃO")
-- end

-- os.setlocale("C")
-- assert(os.setlocale() == 'C')
-- assert(os.setlocale(nil, "numeric") == 'C')

-- end

print('OK')
