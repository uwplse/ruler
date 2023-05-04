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

h = ('Herbie', pairs['main'])
e = ('Renumo', pairs['enumo'])
f = ('Renumo-FF', pairs['enumo-no-ff'])
o = ('Renumo-R', pairs['enumo-rat'])
r = ('Ruler', pairs['ruler'])
# hl = ('Herbie[L]', pairs['main-lim'])
# el = ('Enumo[L]', pairs['enumo-lim'])
# ol = ('Enumo[L, Rat]', pairs['enumo-rat-lim'])

# listify = [h, e, f, o, r, hl, el]
listify = [h, e, f, o, r]

labs = []
vals = []
for c, ds in listify:
    labs.append(c)
    vals.append(ds)

fig, ax = plt.subplots(figsize=(6, 5))
ax.boxplot(vals)

# manually set before deadline
if str(field) == "output_parens":
    yname = "AST Size"
    ax.set_ylim([0, 5000])
elif str(field) == "time":
    yname = "Time (s)"
    ax.set_ylim([0, 5000])
elif str(field) == "avg_bits_err_improve":
    yname = "Average bits of error improved"
    ax.set_ylim([0, 3000])
else:
    yname = str(field)
    ax.set_ylim(bottom=0)

title = yname + " over 30 seeds"
#ax.set_title(title, fontsize=8)
ax.set_xticklabels(labs)

ax.set_xlabel('Rules used', labelpad=10, fontsize=10)
ax.set_ylabel(yname, fontsize=10)
plt.xticks(fontsize=10)
plt.yticks(fontsize=10)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
plt.savefig('by-config-all-tests-{}-boxplot.png'.format(field))
