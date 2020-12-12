local xs = {}

for a in io.open("inputs/day01.txt", "r"):lines() do table.insert(xs,tonumber(a)) end

local n = #xs
local p1,p2 = nil,nil

for i=1,n do
   for j=i+1,n do
      local xi,xj = xs[i], xs[j]
      if xi+xj == 2020 then
         p1 = xi*xj
      end
      for k=j+1,n do
         if xs[k]+xi+xj == 2020 then
            p2 = xi*xj*xs[k]
         end
      end
   end
end

print(p1)
print(p2)
