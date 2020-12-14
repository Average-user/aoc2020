import re
from collections import defaultdict 

mem = {}
for l in open("../inputs/day14.txt","r").readlines():
    if re.match("mask",l):
        mask = re.match("mask = (.+)",l).group(1)
        ms = 0
        mc = 0
        for b in mask:
            ms = ms*2
            mc = mc*2
            if b == '0':
                mc += 1
            elif b == '1':
                ms += 1
    else:
        m = re.search(r"mem\[([0-9]*)\] = ([0-9]*)",l)
        k = int(m.group(1))
        v = int(m.group(2))
        mem[k] = (v | ms) & ~mc

print(reduce(lambda a,b: a+b,mem.values()))

mem = {}
def all(n,k,v):
    if n == 36:
        mem[k] = v
        return None
    if mz[n]:
        k = k & ~(1 << n)
        all(n+1, k | (1 << n), v)
    all(n+1,k,v)

for l in open("../inputs/day14.txt","r").readlines():
    if re.match("mask",l):
        mask = re.match("mask = (.+)",l).group(1)
        ms = 0
        mc = 0
        mz = defaultdict(lambda: False)
        n = 35
        for b in mask:
            ms = ms*2
            mc = mc*2
            if b == '0':
                mc += 1
            elif b == '1':
                ms += 1
            elif b == "X":
                mz[n] = True
            n = n-1
    else:
        m = re.search(r"mem\[([0-9]*)\] = ([0-9]*)",l)
        k = int(m.group(1))
        v = int(m.group(2))
        k = k | ms
        all(0,k,v)

print(reduce(lambda a,b: a+b,mem.values()))
