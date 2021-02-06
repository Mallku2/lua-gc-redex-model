-- new-style varargs

function oneless (a, ...) return ... end

function f (n, a, ...)
  local b
  assert(arg == nil)
  if n == 0 then
    local b, c, d = ...
    return a, b, c, d, oneless(oneless(oneless(...)))
  else
    n, b, a = n-1, ..., a
    assert(b == ...)
    return f(n, a, ...)
  end
end

a,b,c,d,e = assert(f(10,5,4,3,2,1))
assert(a==5 and b==4 and c==3 and d==2 and e==1)

a,b,c,d,e = f(4)
assert(a==nil and b==nil and c==nil and d==nil and e==nil)


-- varargs for main chunks
f = load[[ return {...} ]]
x = f(2,3)
assert(x[1] == 2 and x[2] == 3 and x[3] == nil)


f = load[[
  local x = {...}
  for i=1,select('#', ...) do assert(x[i] == select(i, ...)) end
  assert(x[select('#', ...)+1] == nil)
  return true
]]

assert(f("a", "b", nil, {}, assert))
assert(f())

a = {select(3, table.unpack{10,20,30,40})}
assert(#a == 2 and a[1] == 30 and a[2] == 40)
a = {select(1)}
assert(next(a) == nil)
a = {select(-1, 3, 5, 7)}
assert(a[1] == 7 and a[2] == nil)
a = {select(-2, 3, 5, 7)}
assert(a[1] == 5 and a[2] == 7 and a[3] == nil)
pcall(select, 10000)
pcall(select, -10000)

print('OK')
