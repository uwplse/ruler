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
            times.append(d['time']['secs'])
    # plt.bar(nsamples, times, width, align="center")
    plt.plot(nsamples, times)
    plt.xlabel("Sample length")
    plt.ylabel("Time")
    plt.title("Sample size vs Time")
    plt.show()

mk_cvec_time_plot("../out/sample_vs_time.json")