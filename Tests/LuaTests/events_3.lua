-- test for rawlen
t = setmetatable({1,2,3}, {__len = function () return 10 end})
assert(#t == 10 and rawlen(t) == 3)
assert(rawlen"abc" == 3)
------------------------------------------
-- TODO: io.stdin
------------------------------------------
-- assert(not pcall(rawlen, io.stdin))
assert(not pcall(rawlen, 34))
assert(not pcall(rawlen))

t = {}
t.__lt = function (a,b,c)
  collectgarbage()
  assert(c == nil)
  if type(a) == 'table' then a = a.x end
  if type(b) == 'table' then b = b.x end
 return a<b, "dummy"
end

function Op(x) return setmetatable({x=x}, t) end

local function test ()
  assert(not(Op(1)<Op(1)) and (Op(1)<Op(2)) and not(Op(2)<Op(1)))
  assert(not(1 < Op(1)) and (Op(1) < 2) and not(2 < Op(1)))
  assert(not(Op('a')<Op('a')) and (Op('a')<Op('b')) and not(Op('b')<Op('a')))
  assert(not('a' < Op('a')) and (Op('a') < 'b') and not(Op('b') < Op('a')))
  assert((Op(1)<=Op(1)) and (Op(1)<=Op(2)) and not(Op(2)<=Op(1)))
  assert((Op('a')<=Op('a')) and (Op('a')<=Op('b')) and not(Op('b')<=Op('a')))
  assert(not(Op(1)>Op(1)) and not(Op(1)>Op(2)) and (Op(2)>Op(1)))
  assert(not(Op('a')>Op('a')) and not(Op('a')>Op('b')) and (Op('b')>Op('a')))
  assert((Op(1)>=Op(1)) and not(Op(1)>=Op(2)) and (Op(2)>=Op(1)))
  assert((1 >= Op(1)) and not(1 >= Op(2)) and (Op(2) >= 1))
  assert((Op('a')>=Op('a')) and not(Op('a')>=Op('b')) and (Op('b')>=Op('a')))
  assert(('a' >= Op('a')) and not(Op('a') >= 'b') and (Op('b') >= Op('a')))
end

test()

t.__le = function (a,b,c)
  assert(c == nil)
  if type(a) == 'table' then a = a.x end
  if type(b) == 'table' then b = b.x end
 return a<=b, "dummy"
end

test()  -- retest comparisons, now using both `lt' and `le'


-- test `partial order'

local function Set(x)
  local y = {}
  for _,k in pairs(x) do y[k] = 1 end
  return setmetatable(y, t)
end

t.__lt = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
    b[k] = nil
  end
  return next(b) ~= nil
end

t.__le = nil

assert(Set{1,2,3} < Set{1,2,3,4})
assert(not(Set{1,2,3,4} < Set{1,2,3,4}))
assert((Set{1,2,3,4} <= Set{1,2,3,4}))
assert((Set{1,2,3,4} >= Set{1,2,3,4}))
assert((Set{1,3} <= Set{3,5}))   -- wrong!! model needs a `le' method ;-)

t.__le = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
  end
  return true
end

assert(not (Set{1,3} <= Set{3,5}))   -- now its OK!
assert(not(Set{1,3} <= Set{3,5}))
assert(not(Set{1,3} >= Set{3,5}))

t.__eq = function (a,b)
  for k in pairs(a) do
    if not b[k] then return false end
    b[k] = nil
  end
  return next(b) == nil
end

local s = Set{1,3,5}
assert(s == Set{3,5,1})
assert(not rawequal(s, Set{3,5,1}))
assert(rawequal(s, s))
assert(Set{1,3,5,1} == Set{3,5,1})
assert(Set{1,3,5} ~= Set{3,5,1,6})
t[Set{1,3,5}] = 1
assert(t[Set{1,3,5}] == nil)   -- `__eq' is not valid for table accesses


t.__concat = function (a,b,c)
  assert(c == nil)
  if type(a) == 'table' then a = a.val end
  if type(b) == 'table' then b = b.val end
  if A then return a..b
  else
    return setmetatable({val=a..b}, t)
  end
end

c = {val="c"}; setmetatable(c, t)
d = {val="d"}; setmetatable(d, t)

A = true
assert(c..d == 'cd')
assert(0 .."a".."b"..c..d.."e".."f"..(5+3).."g" == "0abcdef8g")

A = false
assert((c..d..c..d).val == 'cdcd')
x = c..d
assert(getmetatable(x) == t and x.val == 'cd')
x = 0 .."a".."b"..c..d.."e".."f".."g"
assert(x.val == "0abcdefg")
