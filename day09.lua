local x = {}
for a in io.open("inputs/day09.txt", "r"):lines() do table.insert(x,tonumber(a)) end

local n,p1 = #x, -1

for i=26,n do
   local b,xi = false, x[i]
   for j=i-25,i-1 do
      for k=j+1,i-1 do
         if xi == x[j] + x[k] then
            b = true
         end
      end
   end
   if not b then
      p1 = xi
      break
   end
end
print(p1)

local a,b = -1,-1

for i=1,n do
   local s,t = 0,false
   for j=i,n do
      s = s+x[j]
      if s == p1 then
         a = i
         b = j
         t = true
         break
      end
   end
   if t then break end
end

local min,max = x[a],0

for i=a,b do
   local q = x[i]
   if q > max then max = q end
   if q < min then min = q end
end
print(min+max)
         

   
