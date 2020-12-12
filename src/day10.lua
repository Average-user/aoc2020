local xs,max = {},0

for line in io.open("../inputs/day10.txt", "r"):lines() do
   local a = tonumber(line)
   if a > max then max = a end
   table.insert(xs,a)
end

table.insert(xs,0)
table.sort(xs)
table.insert(xs,max+3)

local n,ones,threes = #xs,0,0

for i=1,n-1 do
   local d = xs[i+1]-xs[i]
   if d == 1 then
      ones = ones+1
   elseif d == 3 then
      threes = threes + 1
   end
end

print(ones*threes)

local acc = {}
for i=1,n do acc[i] = 0 end
acc[1] = 1

for i=2,n do
   local s = 0
   for j=i-3,i-1 do
      if j >= 1 and xs[i] <= xs[j]+3 then
         s = s+acc[j]
      end
   end
   acc[i] = s
end

print(acc[n])


