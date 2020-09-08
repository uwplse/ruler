import json
import matplotlib.pyplot as plt
import numpy as np

def mk_cvec_time_plot(p):
    width = 15
    times = []
    nsamples = []
    with open(p) as f:
        data = json.load(f)
        for d in data:
            nsamples.append(d['nsamples'])
            times.append(d['time']['secs'] + (d['time']['nanos'] / 1000000000))
    # plt.bar(nsamples, times, width, align="center")
    # plt.plot(nsamples, times, linewidth=1, marker='*', markersize=7, label="1 iter, 13 ops, one at a time w/o cond")
    # plt.plot(nsamples, times, linewidth=1, marker='*', markersize=7, label="1 iter, 13 ops, one at a time w cond")
    # plt.plot(nsamples, times, linewidth=1, marker='*', markersize=7, label="1 iter, 13 ops, batched w/o cond")
    plt.plot(nsamples, times, linewidth=1, marker='*', markersize=7, label="1 iter, 13 ops, batched w cond")
    plt.xlabel("Sample length")
    plt.ylabel("Time(sec)")
    plt.title("Sample size vs Time")
    plt.legend()
    plt.show()

mk_cvec_time_plot("../out/sample_vs_time.json")