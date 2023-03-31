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

# h = ('H', pairs['main'])
# hn = ('H[-N]', pairs['main-n'])
# ht = ('H[-T]', pairs['main-t'])
# hnt = ('H[-NT]', pairs['main-n-t'])
# e = ('E', pairs['enumo'])
# en = ('E[-N]', pairs['enumo-n'])
# et = ('E[-T]', pairs['enumo-t'])
# ent = ('E[-NT]', pairs['enumo-n-t'])
# r = ('R', pairs['ruler'])
# rn = ('R[-N]', pairs['ruler-n'])
# rt = ('R[-T]', pairs['ruler-t'])
# rnt = ('R[-NT]', pairs['ruler-n-t'])
# n = ('N', pairs['no-rules'])
# listify = [h, hn, ht, hnt, e, en, et, ent, r, rn, rt, rnt, n]

# h = ('H', pairs['main'])
# hn = ('H[-N]', pairs['main-n'])
# e = ('E', pairs['enumo'])
# en = ('E[-N]', pairs['enumo-n'])
# r = ('R', pairs['ruler'])
# rn = ('R[-N]', pairs['ruler-n'])
# n = ('N', pairs['no-rules'])
# listify = [h, hn, e, en, r, rn, n]

# ht = ('H[-T]', pairs['main-t'])
# hnt = ('H[-NT]', pairs['main-n-t'])
# et = ('E[-T]', pairs['enumo-t'])
# ent = ('E[-NT]', pairs['enumo-n-t'])
# rt = ('R[-T]', pairs['ruler-t'])
# rnt = ('R[-NT]', pairs['ruler-n-t'])
# n = ('N', pairs['no-rules'])
# listify = [ht, hnt, et, ent,  rt, rnt, n]

# h = ('H', pairs['main'])
# e = ('E', pairs['enumo'])
# en = ('E[-N]', pairs['enumo-n'])
# f = ('F', pairs['ruler-no-ff'])
# fn = ('F[-N]', pairs['ruler-no-ff-n'])
# r = ('R', pairs['ruler'])
# rn = ('R[-N]', pairs['ruler-n'])
# n = ('N', pairs['no-rules'])
# listify = [h, e, en, f, fn, r, rn, n]

h = ('H[-T]', pairs['main-t'])
e = ('E[-T]', pairs['enumo-t'])
en = ('E[-NT]', pairs['enumo-n-t'])
f = ('F[-T]', pairs['ruler-no-ff-t'])
fn = ('F[-NT]', pairs['ruler-no-ff-nt'])
r = ('R[-T]', pairs['ruler-t'])
rn = ('R[-NT]', pairs['ruler-n-t'])
listify = [h, e, en, f, fn, r, rn]

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
    ax.set_ylim([0, 5000])
elif str(field) == "time":
    yname = "Time (s)"
    ax.set_ylim([0, 6000])
elif str(field) == "avg_bits_err_improve":
    yname = "Average bits of error improved"
    ax.set_ylim([0, 4000])
else:
    yname = str(field)
    ax.set_ylim(bottom=0)

title = yname + " over 30 seeds"
#ax.set_title(title, fontsize=8)
ax.set_xlabel('Rules used', labelpad=10)
ax.set_ylabel(yname)
ax.set_xticklabels(labs)

plt.tight_layout()
plt.savefig('by-config-all-tests-{}-boxplot.pdf'.format(field))
plt.savefig('by-config-all-tests-{}-boxplot.png'.format(field))
