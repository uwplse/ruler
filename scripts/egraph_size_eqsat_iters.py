import json
import matplotlib.pyplot as plt
import numpy as np

def mk_eqsat_eclass_plot(p):
    mk_eqsat_size_plot(p, "eclasses")

def mk_eqsat_enode_plot(p):
    mk_eqsat_size_plot(p, "enodes")

def mk_eqsat_size_plot(p, name):
    width = 15
    times = []
    nsamples = []
    with open(p) as f:
        data = json.load(f)
        data = data["data_points"]
        for d in data:
            nsamples.append(d['eqsat_iter'])
            times.append(d[name])
    plt.plot(nsamples, times, linewidth=1, marker='*', markersize=7, label="1 iter, 13 ops, batched w cond")
    plt.xlabel("Eqsat iterations")
    plt.ylabel("Number of " + name)
    plt.title("Growth of " + name + " with eqsat iterations")
    plt.legend()
    plt.show()


mk_eqsat_eclass_plot("../out/eqsat_egraph_size.json")
mk_eqsat_enode_plot("../out/eqsat_egraph_size.json")