import json
import sys
import matplotlib.pyplot as plt
import numpy as np 

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ss = json.load(f)

data = [s['data'] for s in ss]
pairs = dict([(s['config'], s['data']) for s in ss])

hno = ('herbie-no-simpl', pairs['herbie-no-simpl'])
ho = ('herbie-only', pairs['herbie-only'])
ro = ('ruler-only', pairs['ruler-only'])
hr = ('herbie-ruler', pairs['herbie-ruler'])

listify = [hno, ho, ro, hr]
labs = []
vals = []
for c, ds in listify:
    labs.append(c)
    vals.append(ds)

fig, ax = plt.subplots()
ax.boxplot(vals)

if str(field) == "output_parens":
    yname = "ast size"
    ax.set_ylim([0, 1000])
elif str(field) == "time":
    yname = "time"
    ax.set_ylim([0, 2000000])
elif str(field) == "avg_bits_err_improve":
    yname = "average bits of error improved"
    ax.set_ylim([0, 500])
else:
    yname = str(field)
    ax.set_ylim(bottom=0)

title = "config vs " + yname + " over 30 seeds"
ax.set_title(title)
ax.set_xlabel('config')
ax.set_ylabel(yname)
ax.set_xticklabels(labs)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
