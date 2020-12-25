local input = {}
for a in io.open("../inputs/day25.txt", "r"):lines() do
   table.insert(input,tonumber(a))
end

local cardk,doork,found,i,v,cardloop = input[1],input[2],false,1,1,nil
repeat
   v = (v*7)%20201227
   if v == cardk then
      found = true
      cardloop = i
   end
   i = i+1
until found

local v = 1
for i=1,cardloop do
   v = (v*doork)%20201227
end

print(v)
