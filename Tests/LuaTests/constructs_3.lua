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

print 'testing short-circuit optimizations'

_ENV.GLOB1 = 1
_ENV.GLOB2 = 2

local basiccases = {
  {"nil", nil},
  {"false", false},
  {"true", true},
  {"10", 10},
  {"(_ENV.GLOB1 < _ENV.GLOB2)", true},
  {"(_ENV.GLOB2 < _ENV.GLOB1)", false},
}


local binops = {
  {" and ", function (a,b) if not a then return a else return b end end},
  {" or ", function (a,b) if a then return a else return b end end},
}

local mem = {basiccases}    -- for memoization

local function allcases (n)
  if mem[n] then return mem[n] end
  local res = {}
  -- include all smaller cases
  for _, v in ipairs(allcases(n - 1)) do
    res[#res + 1] = v
  end
  for i = 1, n - 1 do
    for _, v1 in ipairs(allcases(i)) do
      for _, v2 in ipairs(allcases(n - i)) do
        for _, op in ipairs(binops) do
            res[#res + 1] = {
              "(" .. v1[1] .. op[1] .. v2[1] .. ")",
              op[2](v1[2], v2[2])
            }
        end
      end
    end
    print('+')
  end
  mem[n] = res   -- memoize
  return res
end

-- do not do too many combinations for soft tests
_soft = true  --WAS: nothing, setting to test fewer cases
local level = _soft and 1 or 4 --WAS: local level = _soft and 3 or 4 
-- TODO: for level >= 2 the test takes hours under our mechanization in Redex 
for _, v in pairs(allcases(level)) do
  print("-")
  local res = load("return " .. v[1])()
  assert(res == v[2])
end
------------------------------------------------------------------

print'OK'
