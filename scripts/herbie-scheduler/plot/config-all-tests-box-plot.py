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

s025 = ('S[2.5]', pairs['sat-2500'])
s050 = ('S[5]', pairs['sat-5000'])
s100 = ('S[10]', pairs['sat-10000'])
s200 = ('S[20]', pairs['sat-20000'])

d025 = ('H[2.5]', pairs['default-2500'])
d050 = ('H[5]', pairs['default-5000'])
d100 = ('H[10]', pairs['default-10000'])
d200 = ('H[20]', pairs['default-20000'])

listify = [s025, s050, s100, s200, d025, d050, d100, d200]

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
    ax.set_ylim([0, 500])
elif str(field) == "time":
    yname = "Time (s)"
    ax.set_ylim([0, 5000])
elif str(field) == "avg_bits_err_improve":
    yname = "Average bits of error improved"
    ax.set_ylim([0, 750])
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
