import json
import sys
import matplotlib.pyplot as plt
import numpy as np 

jsonp = sys.argv[1]
field = sys.argv[2]

plt.rcParams['font.size'] = '9'

with open(jsonp, 'r') as f:
    ss = json.load(f)

pairs = dict([(s['config'], s['data']) for s in ss])

if field == "time":
    # print(pairs)
    for k in pairs:
        times = pairs.get(k)
        new_times = []
        for t in times:
            new_times.append(float(t) / 1000.0)
        pairs[k] = new_times

# h = ('H[-T]', pairs['main-t'])
# e = ('E[-T]', pairs['enumo-t'])
# en = ('E[-NT]', pairs['enumo-n-t'])
# f = ('F[-T]', pairs['ruler-no-ff-t'])
# fn = ('F[-NT]', pairs['ruler-no-ff-n-t'])
# r = ('R[-T]', pairs['ruler-t'])
# rn = ('R[-NT]', pairs['ruler-n-t'])
# listify = [h, e, en, f, fn, r, rn]

h = ('Herbie', pairs['main-t'])
e = ('Enumo', pairs['enumo-t'])
f = ('Enumo[-FF]', pairs['ruler-no-ff-t'])
o = ('Enumo[Rational]', pairs['enumo-only-rat'])
r = ('Ruler', pairs['ruler-t'])
listify = [h, e, f, o, r]

labs = []
vals = []
for c, ds in listify:
    labs.append(c)
    vals.append(ds)

fig, ax = plt.subplots(figsize=(6, 10))
ax.boxplot(vals)

# manually set before deadline
if str(field) == "output_parens":
    yname = "AST Size"
    ax.set_ylim([0, 4000])
elif str(field) == "time":
    yname = "Time (s)"
    ax.set_ylim([0, 6000])
elif str(field) == "avg_bits_err_improve":
    yname = "Average bits of error improved"
    ax.set_ylim([0, 3000])
else:
    yname = str(field)
    ax.set_ylim(bottom=0)

title = yname + " over 30 seeds"
#ax.set_title(title, fontsize=8)
ax.set_xlabel('Rules used', labelpad=10)
ax.set_ylabel(yname)
ax.set_xticklabels(labs, fontsize=8)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
plt.savefig('by-config-all-tests-{}-boxplot.png'.format(field))
