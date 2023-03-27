import json
import sys
import matplotlib.pyplot as plt
import numpy as np 

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ss = json.load(f)

cfgs = [s['config'] for s in ss]
labels = [ss[0]['tests'][i]['name'] for i in range(len(ss[0]['tests']))]
num_tests = len(labels)

fig, ax = plt.subplots(len(cfgs), num_tests, figsize=(50, 20), sharey=True)
title = "Test wise config vs " + str(field) + " over seeds"
fig.suptitle(title)

for c_id in range(len(cfgs)):
    for t_id in range(num_tests):
        seeds = ss[c_id]['tests'][t_id]['data']
        ax[c_id][t_id].boxplot(seeds)
        ax[c_id][t_id].set_xlabel(labels[t_id], rotation=45)
        ax[c_id][t_id].set_xticks([])
        ax[c_id][t_id].set_ylabel(str(field))

sub_titles = []
for l in cfgs:
    for n in range(num_tests):
        sub_titles.append(l)

for i, ax in enumerate(ax.flat):
    if sub_titles[i] == "herbie-only":
        ax.set_title("ho")
    elif sub_titles[i] == "ruler-only":
        ax.set_title("ro")
    elif sub_titles[i] == "herbie-ruler":
        ax.set_title("hr")
    elif sub_titles[i] == "herbie-no-simpl":
        ax.set_title("h_no")
    else:
        print("No other configs")

# plt.tight_layout()
plt.savefig('by-config-per-test-{}-boxplot.pdf'.format(field))
