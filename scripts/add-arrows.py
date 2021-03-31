import sys

path = sys.argv[1]

with open(path) as fp:
   for line in fp:
       depth = 0
       nzeros = 1
       for c in line:
           if c == '(':
               depth += 1
           if c == ')':
               depth -= 1
           if depth == 0:
               nzeros += 1
           print(c, end='')
           if nzeros == 2:
               print(" =>", end='')
