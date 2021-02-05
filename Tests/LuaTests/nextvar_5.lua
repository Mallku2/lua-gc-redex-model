a = {}
for i=1,10 do --WAS: for i=1,1000 do
  a[i] = i; a[i-1] = nil
end
assert(next(a,nil) == 10 and next(a,10) == nil)
--WAS: assert(next(a,nil) == 1000 and next(a,1000) == nil)

assert(next({}) == nil)
assert(next({}, nil) == nil)

for a,b in pairs{} do error"not here" end
for i=1,0 do error'not here' end
for i=0,1,-1 do error'not here' end
a = nil; for i=1,1 do assert(not a); a=1 end; assert(a)
a = nil; for i=1,1,-1 do assert(not a); a=1 end; assert(a)

if not _port then
  print("testing precision in numeric for")
  local a = 0; for i=0, 1, 0.1 do a=a+1 end; assert(a==11)
  a = 0; for i=0, 0.999999999, 0.1 do a=a+1 end; assert(a==10)
  a = 0; for i=1, 1, 1 do a=a+1 end; assert(a==1)
  a = 0; for i=1e10, 1e10, -1 do a=a+1 end; assert(a==1)
  a = 0; for i=1, 0.99999, 1 do a=a+1 end; assert(a==0)
  a = 0; for i=99999, 1e5, -1 do a=a+1 end; assert(a==0)
  a = 0; for i=1, 0.99999, -1 do a=a+1 end; assert(a==1)
end

-- conversion
a = 0; for i="10","1","-2" do a=a+1 end; assert(a==5)


collectgarbage()


-- testing generic 'for'

local function f (n, p)
  local t = {}; for i=1,p do t[i] = i*10 end
  return function (_,n)
           if n > 0 then
             n = n-1
             return n, table.unpack(t)
           end
         end, nil, n
end

local x = 0
for n,a,b,c,d in f(5,3) do
  x = x+1
  assert(a == 10 and b == 20 and c == 30 and d == nil)
end
assert(x == 5)
