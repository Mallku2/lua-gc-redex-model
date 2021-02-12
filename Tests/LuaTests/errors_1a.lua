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
print("testing errors")

------------------------------------------
-- TODO: debug
------------------------------------------
--local debug = require"debug"

-- avoid problems with 'strict' module (which may generate other error messages)
local mt = getmetatable(_G) or {}
local oldmm = mt.__index
mt.__index = nil

function doit (s)
  local f, msg = load(s)
  if f == nil then return msg end
  local cond, msg = pcall(f)
  return (not cond) and msg
end

-----------------------------------------------------
-- TODO: we check if it fails, but not the error message
-----------------------------------------------------
function checkmessage (prog, msg)
  local m = doit(prog)
  assert(m) --WAS: nothing, we just ensure there is an actual error
  -- assert(string.find(m, msg, 1, true))
end

function checksyntax (prog, extra, token, line)
  local msg = doit(prog)
  assert(msg) --WAS: nothing, we just ensure there is an actual error
  -- if not string.find(token, "^<%a") and not string.find(token, "^char%(")
  --   then token = "'"..token.."'" end
  -- token = string.gsub(token, "(%p)", "%%%1")
  -- local pt = string.format([[^%%[string ".*"%%]:%d: .- near %s$]],
  --                          line, token)
  -- assert(string.find(msg, pt))
  -- assert(string.find(msg, msg, 1, true))
end


-- test error message with no extra info
assert(doit("error('hi', 0)") == 'hi')

-- test error message with no info
assert(doit("error()") == nil)

-- test common errors/errors that crashed in the past
if not _no32 then
  assert(doit("table.unpack({}, 1, n=2^30)"))
end
assert(doit("a=math.sin()"))
assert(not doit("tostring(1)") and doit("tostring()"))
assert(doit"tonumber()")
assert(doit"repeat until 1; a")
assert(doit"return;;")
assert(doit"assert(false)")
assert(doit"assert(nil)")
assert(doit("function a (... , ...) end"))
assert(doit("function a (, ...) end"))
assert(doit("local t={}; t = t[#t] + 1"))

checksyntax([[
  local a = {4

]], "'}' expected (to close '{' at line 1)", "<eof>", 3)
