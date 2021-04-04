import json
import sys
import matplotlib.pyplot as plt
import numpy as np 

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ss = json.load(f)

labs = [s['config'] for s in ss]
data = [s['data'] for s in ss]

fig, ax = plt.subplots()
ax.boxplot(data)

title = "config vs " + str(field) + " over seeds, summed over all tests"
ax.set_title(title)
ax.set_xlabel('config')
ax.set_ylabel(str(field))
ax.set_xticklabels(labs)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
