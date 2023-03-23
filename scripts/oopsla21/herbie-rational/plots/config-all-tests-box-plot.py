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
            new_times.append(float(t) / 1000000.0)
        pairs[k] = new_times

hno = ('None', pairs['herbie-no-simpl'])
ho = ('Herbie', pairs['herbie-only'])
ro = ('Ruler', pairs['ruler-only'])
hr = ('Both', pairs['herbie-ruler'])

# hno = ('herbie-no-simpl', pairs['herbie-no-simpl'])
# ho = ('herbie-only', pairs['herbie-only'])
# ro = ('ruler-only', pairs['ruler-only'])
# hr = ('herbie-ruler', pairs['herbie-ruler'])

listify = [hno, ho, ro, hr]
labs = []
vals = []
for c, ds in listify:
    labs.append(c)
    vals.append(ds)

fig, ax = plt.subplots(figsize=(3, 5))
ax.boxplot(vals)

# manually set before deadline
if str(field) == "output_parens":
    yname = "AST Size"
    ax.set_ylim([0, 1000])
elif str(field) == "time":
    yname = "Time (s)"
    ax.set_ylim([0, 2])
elif str(field) == "avg_bits_err_improve":
    yname = "Average bits of error improved"
    ax.set_ylim([0, 600])
else:
    yname = str(field)
    ax.set_ylim(bottom=0)

title = yname + " over 30 seeds"
#ax.set_title(title, fontsize=8)
ax.set_xlabel('Rules used for simplification', labelpad=10)
ax.set_ylabel(yname)
ax.set_xticklabels(labs)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
