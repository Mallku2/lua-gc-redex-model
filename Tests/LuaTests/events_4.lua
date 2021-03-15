-- concat metamethod x numbers (bug in 5.1.1)
c = {}
local x
setmetatable(c, {__concat = function (a,b)
  assert(type(a) == "number" and b == c or type(b) == "number" and a == c)
  return c
end})
assert(c..5 == c and 5 .. c == c)
assert(4 .. c .. 5 == c and 4 .. 5 .. 6 .. 7 .. c == c)


-- test comparison compatibilities
local t1, t2, c, d
t1 = {};  c = {}; setmetatable(c, t1)
d = {}
t1.__eq = function () return true end
t1.__lt = function () return true end
setmetatable(d, t1)
assert(c == d and c < d and not(d <= c))
t2 = {}
t2.__eq = t1.__eq
t2.__lt = t1.__lt
setmetatable(d, t2)
assert(c == d and c < d and not(d <= c))



-- test for several levels of calls
local i
local tt = {
  __call = function (t, ...)
    i = i+1
    if t.f then return t.f(...)
    else return {...}
    end
  end
}

local a = setmetatable({}, tt)
local b = setmetatable({f=a}, tt)
local c = setmetatable({f=b}, tt)

i = 0
x = c(3,4,5)
assert(i == 3 and x[1] == 3 and x[3] == 5)


print'+'

local _g = _G
_ENV = setmetatable({}, {__index=function (_,k) return _g[k] end})


a = {}
rawset(a, "x", 1, 2, 3)
assert(a.x == 1 and rawget(a, "x", 3) == 1)

print '+'

-- testing metatables for basic types
--local debug = require'debug'
mt = {}
mt.__index = function (a,b) return a+b end --WAS: changed the order of this line
debug.setmetatable(10, mt)
assert(getmetatable(-2) == mt)
assert((10)[3] == 13)
assert((10)["3"] == 13)
debug.setmetatable(23, nil)
assert(getmetatable(-2) == nil)

mt.__index = function (a,b) return a or b end --WAS: changed the order of this line
debug.setmetatable(true, mt)
assert(getmetatable(false) == mt)
assert((true)[false] == true)
assert((false)[false] == false)
debug.setmetatable(false, nil)
assert(getmetatable(true) == nil)

mt.__add = function (a,b) return (a or 0) + (b or 0) end --WAS: changed the order of this line
debug.setmetatable(nil, mt)
assert(getmetatable(nil) == mt)
assert(10 + nil == 10)
assert(nil + 23 == 23)
assert(nil + nil == 0)
debug.setmetatable(nil, nil)
assert(getmetatable(nil) == nil)

debug.setmetatable(nil, {})


-- loops in delegation
a = {}; setmetatable(a, a); a.__index = a; a.__newindex = a
assert(not pcall(function (a,b) return a[b] end, a, 10))
assert(not pcall(function (a,b,c) a[b] = c end, a, 10, true))

-- bug in 5.1
T, K, V = nil
grandparent = {}
grandparent.__newindex = function(t,k,v) T=t; K=k; V=v end

parent = {}
parent.__newindex = parent
setmetatable(parent, grandparent)

child = setmetatable({}, parent)
child.foo = 10      --> CRASH (on some machines)
assert(T == parent and K == "foo" and V == 10)

print 'OK'

return 12
