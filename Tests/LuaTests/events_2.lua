local a, t = {10,20,30; x="10", y="20"}, {}
assert(setmetatable(a,t) == a)
assert(getmetatable(a) == t)
assert(setmetatable(a,nil) == a)
assert(getmetatable(a) == nil)
assert(setmetatable(a,t) == a)


function f (t, i, e)
  assert(not e)
  local p = rawget(t, "parent")
  return (p and p[i]+3), "dummy return"
end

t.__index = f

a.parent = {z=25, x=12, [4] = 24}
assert(a[1] == 10 and a.z == 28 and a[4] == 27 and a.x == "10")

collectgarbage()

a = setmetatable({}, t)
function f(t, i, v) rawset(t, i, v-3) end
setmetatable(t, t)   -- causes a bug in 5.1 !
t.__newindex = f
a[1] = 30; a.x = "101"; a[5] = 200
assert(a[1] == 27 and a.x == 98 and a[5] == 197)


local c = {}
a = setmetatable({}, t)
t.__newindex = c
a[1] = 10; a[2] = 20; a[3] = 90
assert(c[1] == 10 and c[2] == 20 and c[3] == 90)

setmetatable(t, nil)
function f (t, ...) return t, {...} end
t.__call = f

do
  local x,y = a(table.unpack{'a', 1})
  assert(x==a and y[1]=='a' and y[2]==1 and y[3]==nil)
  x,y = a()
  assert(x==a and y[1]==nil)
end


local b = setmetatable({}, t)
setmetatable(b,t)

function f(op)
  return function (...) cap = {[0] = op, ...} ; return (...) end
end
t.__add = f("add")
t.__sub = f("sub")
t.__mul = f("mul")
t.__div = f("div")
t.__mod = f("mod")
t.__unm = f("unm")
t.__pow = f("pow")
t.__len = f("len")

assert(b+5 == b)
assert(cap[0] == "add" and cap[1] == b and cap[2] == 5 and cap[3]==nil)
assert(b+'5' == b)
assert(cap[0] == "add" and cap[1] == b and cap[2] == '5' and cap[3]==nil)
assert(5+b == 5)
assert(cap[0] == "add" and cap[1] == 5 and cap[2] == b and cap[3]==nil)
assert('5'+b == '5')
assert(cap[0] == "add" and cap[1] == '5' and cap[2] == b and cap[3]==nil)
b=b-3; assert(getmetatable(b) == t)
assert(5-a == 5)
assert(cap[0] == "sub" and cap[1] == 5 and cap[2] == a and cap[3]==nil)
assert('5'-a == '5')
assert(cap[0] == "sub" and cap[1] == '5' and cap[2] == a and cap[3]==nil)
assert(a*a == a)
assert(cap[0] == "mul" and cap[1] == a and cap[2] == a and cap[3]==nil)
assert(a/0 == a)
assert(cap[0] == "div" and cap[1] == a and cap[2] == 0 and cap[3]==nil)
assert(a%2 == a)
assert(cap[0] == "mod" and cap[1] == a and cap[2] == 2 and cap[3]==nil)
assert(-a == a)
assert(cap[0] == "unm" and cap[1] == a)
assert(a^4 == a)
assert(cap[0] == "pow" and cap[1] == a and cap[2] == 4 and cap[3]==nil)
assert(a^'4' == a)
assert(cap[0] == "pow" and cap[1] == a and cap[2] == '4' and cap[3]==nil)
assert(4^a == 4)
assert(cap[0] == "pow" and cap[1] == 4 and cap[2] == a and cap[3]==nil)
assert('4'^a == '4')
assert(cap[0] == "pow" and cap[1] == '4' and cap[2] == a and cap[3]==nil)
assert(#a == a)
assert(cap[0] == "len" and cap[1] == a)
