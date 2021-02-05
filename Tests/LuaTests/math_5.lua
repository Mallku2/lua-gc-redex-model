-- testing implicit convertions

local a,b = '10', '20'
assert(a*b == 200 and a+b == 30 and a-b == -10 and a/b == 0.5 and -b == -20)
assert(a == '10' and b == '20')


if not _port then
  print("testing -0 and NaN")
  local mz, z = -0, 0
  assert(mz == z)
  assert(1/mz < 0 and 0 < 1/z)
  local a = {[mz] = 1}
  assert(a[z] == 1 and a[mz] == 1)
  local inf = math.huge * 2 + 1
  mz, z = -1/inf, 1/inf
  assert(mz == z)
  assert(1/mz < 0 and 0 < 1/z)
  local NaN = inf - inf
  assert(NaN ~= NaN)
  assert(not (NaN < NaN))
  assert(not (NaN <= NaN))
  assert(not (NaN > NaN))
  assert(not (NaN >= NaN))
  assert(not (0 < NaN) and not (NaN < 0))
  local NaN1 = 0/0
  assert(NaN ~= NaN1 and not (NaN <= NaN1) and not (NaN1 <= NaN))
  local a = {}
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
  a[1] = 1
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
  -- string with same binary representation as 0.0 (may create problems
  -- for constant manipulation in the pre-compiler)
  local a1, a2, a3, a4, a5 = 0, 0, "\0\0\0\0\0\0\0\0", 0, "\0\0\0\0\0\0\0\0"
  assert(a1 == a2 and a2 == a4 and a1 ~= a3)
  assert(a3 == a5)
end

------------------------------------------
-- TODO: math.random
------------------------------------------
-- if not _port then
--   print("testing 'math.random'")
--   math.randomseed(0)

--   local function aux (x1, x2, p)
--     local Max = -math.huge
--     local Min = math.huge
--     for i = 0, 20000 do
--       local t = math.random(table.unpack(p))
--       Max = math.max(Max, t)
--       Min = math.min(Min, t)
--       if eq(Max, x2, 0.001) and eq(Min, x1, 0.001) then
--         goto ok
--       end
--     end
--     -- loop ended without satisfing condition
--     assert(false)
--    ::ok::
--     assert(x1 <= Min and Max<=x2)
--   end

--   aux(0, 1, {})
--   aux(-10, 0, {-10,0})
-- end

-- for i=1,10 do
--   local t = math.random(5)
--   assert(1 <= t and t <= 5)
-- end


print('OK')
