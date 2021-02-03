function f(b)
  local x = 1;
  repeat
    local a;
    if b==1 then local b=1; x=10; break
    elseif b==2 then x=20; break;
    elseif b==3 then x=30;
    else local a,b,c,d=math.sin(1); x=x+1;
    end
  until x>=12;
  return x;
end;

assert(f(1) == 10 and f(2) == 20 and f(3) == 30 and f(4)==12)
