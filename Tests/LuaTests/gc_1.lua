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
print('testing garbage collection')

collectgarbage()

------------------------------------------
-- TODO: collectgarbage parameters not included
------------------------------------------

-- assert(collectgarbage("isrunning"))

-- local function gcinfo () return collectgarbage"count" * 1024 end


-- -- test weird parameters
-- do
--   -- save original parameters
--   local a = collectgarbage("setpause", 200)
--   local b = collectgarbage("setstepmul", 200)
--   local t = {0, 2, 10, 90, 500, 5000, 30000, 2^31 - 2}
--   for i = 1, #t do
--     local p = t[i]
--     for j = 1, #t do
--       local m = t[j]
--       collectgarbage("setpause", p)
--       collectgarbage("setstepmul", m)
--       collectgarbage("step", 0)
--       collectgarbage("step", 10000)
--     end
--   end
--   -- restore original parameters
--   collectgarbage("setpause", a)
--   collectgarbage("setstepmul", b)
--   collectgarbage()
-- end


_G["while"] = 234

limit = 5000

------------------------------------------
-- NOTE: GC1 and GC are not used in this module.
-- Their definition is put in gc_3.lua
------------------------------------------

------------------------------------------
-- TODO: performance: limit = 5000, string.gsub
------------------------------------------

-- contCreate = 0

-- print('tables')
-- while contCreate <= limit do
--   local a = {}; a = nil
--   contCreate = contCreate+1
-- end

-- a = "a"

-- contCreate = 0
-- print('strings')
-- while contCreate <= limit do
--   a = contCreate .. "b";
--   a = string.gsub(a, '(%d%d*)', string.upper)
--   a = "a"
--   contCreate = contCreate+1
-- end


-- contCreate = 0

-- a = {}

------------------------------------------
-- TODO: string.format not included
------------------------------------------
-- print('functions')
-- function a:test ()
--   while contCreate <= limit do
--     load(string.format("function temp(a) return 'a%d' end", contCreate))()
--     assert(temp() == string.format('a%d', contCreate))
--     contCreate = contCreate+1
--   end
-- end

-- a:test()

-- collection of functions without locals, globals, etc.
do local f = function () end end


print("functions with errors")
prog = [[
do
  a = 10;
  function foo(x,y)
    a = sin(a+0.456-0.23e-12);
    return function (z) return sin(%x+z) end
  end
  local x = function (w) a=a+w; end
end]]

do
  local step = 13 -- WAS: local step = 1, changed for performance reasons
  if _soft then step = 13 end
  for i=1, string.len(prog), step do
    for j=i, string.len(prog), step do
      pcall(load(string.sub(prog, i, j)))
    end
  end
end

foo = nil
