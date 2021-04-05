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

title = "config vs " + str(field) + " over seeds, summed over all tests"
ax.set_title(title)
ax.set_ylim(bottom=0)
ax.set_xlabel('config')
ax.set_ylabel(str(field))
ax.set_xticklabels(labs)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
