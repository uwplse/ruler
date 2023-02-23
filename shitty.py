rules = []
num_rules = 0
with open("rules.txt") as f:
  lines = f.readlines()
  num_rules = len(lines)
  uni = 0
  bi = 0
  for line in lines:
    if "==>" in line:
      uni += 1
      rules.append(line.strip())
    if "<=>" in line:
      bi += 1
      [lhs, rhs] = line.split(" <=> ")
      rules.append(lhs.strip() + " ==> " + rhs.strip())
      rules.append(rhs.strip() + " ==> " + lhs.strip())

assert(len(rules) == uni + bi*2)

with open("out.rules", "w") as f:
  f.writelines(line + '\n' for line in rules)