import glob

paths = glob.glob("src/*.cpp")

rewrite = "rewrite("

unsupported = [
  "broadcast",
  "ramp",
  "fold",
  "pos_inf",
  "neg_inf",
  "IRMatcher",
  "intrin",
  "halving_add",
  "shift_right",
  "cast",
  "const",
  "ty",
  "%",
]

def parse_rewrite(full_string, start_idx):
  part = 0
  lhs = ""
  rhs = ""
  cond = ""
  paren_lvl = 1
  idx = start_idx + len(rewrite)
  while idx < len(full_string):
    char = full_string[idx]
    idx += 1
    if char == "(":
      paren_lvl += 1
    if char == ")":
      paren_lvl -= 1
      if paren_lvl == 0:
        break
    if char == "," and paren_lvl == 1:
      part += 1
    else:
      if part == 0:
        lhs += char
      elif part == 1:
        rhs += char
      elif part == 2:
        cond += char
  return (lhs, rhs, cond)

def strip_comments(lines):
  txt = ""
  for line in lines:
    if "//" not in line:
      txt += line.strip()
    else:
      txt += line[:line.find("//")].strip()
  return txt

def parse_file(filename):
  conds = []
  non_conds = []
  with open(filename) as f:
    lines = f.readlines()
    txt = strip_comments(lines)

  for idx in range(len(txt)):
    window = txt[idx:idx+len(rewrite)]
    if window == rewrite:
      (lhs, rhs, cond) = parse_rewrite(txt, idx)
      if cond == "":
        non_conds.append(lhs + " ==> " + rhs)
      else:
        conds.append(lhs + " ==> " + rhs + " if " + cond)
  return (conds, non_conds)

all_cs = []
all_ncs = []
for path in paths:
  (cs, ncs) = parse_file(path)
  all_cs += cs
  all_ncs += ncs
  if len(cs) > 0 or len(ncs) > 0:
    print(path, len(cs) + len(ncs))

for op in unsupported:
  all_cs = [rw for rw in all_cs if op not in rw]
  all_ncs = [rw for rw in all_ncs if op not in rw]

with open("out-c.txt", "w") as f:
  for c in all_cs:
    f.write(c + "\n")

with open("out-nc.txt", "w") as f:
  for c in all_ncs:
    f.write(c + "\n")