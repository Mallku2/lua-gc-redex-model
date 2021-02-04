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
print('testing metatables')

X = 20; B = 30

_ENV = setmetatable({}, {__index=_G})

collectgarbage()

X = X+10
assert(X == 30 and _G.X == 20)
B = false
assert(B == false)
B = nil
assert(B == 30)

assert(_G.X == 20)

assert(getmetatable{} == nil)
assert(getmetatable(4) == nil)
assert(getmetatable(nil) == nil)
a={}; setmetatable(a, {__metatable = "xuxu",
                    __tostring=function(x) return x.name end})
assert(getmetatable(a) == "xuxu")
assert(tostring(a) == nil)
-- cannot change a protected metatable
assert(pcall(setmetatable, a, {}) == false)
a.name = "gororoba"
assert(tostring(a) == "gororoba")

do
  local a;
  a = setmetatable({}, {__index = setmetatable({},
                     {__index = setmetatable({},
                     {__index = function (_,n) return a[n-3]+4, "lixo" end})})})
  a[0] = 20
  for i=0,10 do
    assert(a[i*3] == 20 + i*4)
  end
end


do  -- newindex
  local foi
  local a = {}
  for i=1,10 do a[i] = 0; a['a'..i] = 0; end
  setmetatable(a, {__newindex = function (t,k,v) foi=true; rawset(t,k,v) end})
  foi = false; a[1]=0; assert(not foi)
  foi = false; a['a1']=0; assert(not foi)
  foi = false; a['a11']=0; assert(foi)
  foi = false; a[11]=0; assert(foi)
  foi = false; a[1]=nil; assert(not foi)
  foi = false; a[1]=nil; assert(foi)
end
