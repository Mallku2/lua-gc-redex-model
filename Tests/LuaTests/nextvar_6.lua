
-- testing __pairs and __ipairs metamethod
a = {}
do
  local x,y,z = pairs(a)
  assert(type(x) == 'function' and y == a and z == nil)
end

local function foo (e,i)
  assert(e == a)
  if i <= 10 then return i+1, i+2 end
end

local function foo1 (e,i)
  i = i + 1
  assert(e == a)
  if i <= e.n then return i,a[i] end
end

setmetatable(a, {__pairs = function (x) return foo, x, 0 end})

local i = 0
for k,v in pairs(a) do
  i = i + 1
  assert(k == i and v == k+1)
end

a.n = 5
a[3] = 30

a = {n=10}
setmetatable(a, {__len = function (x) return x.n end,
                 __ipairs = function (x) return function (e,i)
                             if i < #e then return i+1 end
                           end, x, 0 end})
i = 0
for k,v in ipairs(a) do
  i = i + 1
  assert(k == i and v == nil)
end
assert(i == a.n)

print"OK"
