import json
import sys
import matplotlib.pyplot as plt
import numpy as np 

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ss = json.load(f)

# benchmark ids
labels = [ss[0]['tests'][i]['name'] for i in range(len(ss[0]['tests']))]

ho = []
hr = []
ro = []
h_no = []

for s in ss:
    if s['config'] == "main":
        for t in s['tests']:
            ho.append(np.median(t['data']))
    elif s['config'] == "slide-rule":
        for t in s['tests']:
            ro.append(np.median(t['data']))
    elif s['config'] == "oopsla21":
        for t in s['tests']:
            hr.append(np.median(t['data']))
    else:
        print('only 3 configs')

# from matplotlib website
x = np.arange(len(labels))
width = 0.2

fig, ax = plt.subplots(figsize=(25, 20))
rects1 = ax.bar(x - (width * 2), ho, width, label='main')
rects3 = ax.bar(x, hr, width, label='slide-rule')
rects4 = ax.bar(x + width, ro, width, label='oopsla21')

ax.set_ylabel(field)
ax.set_title('{} vs benchmarks for each config'.format(field))
ax.set_xticks(x)
ax.set_xticklabels(labels, rotation=45, ha='right')
ax.legend()


plt.savefig('by-config-per-test-{}-bar.pdf'.format(field))
#plt.show()
