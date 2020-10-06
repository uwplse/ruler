import json
import matplotlib.pyplot as plt
import numpy as np

def mk_eqsat_time_plot(p):
    width = 15
    times = []
    eqsat = []
    with open(p) as f:
        data = json.load(f)
        #data = data["data_points"]
        for d in data:
            eqsat.append(d["eqsat_on"])
            print(eqsat)
            times.append(d['time']['secs'] + (d['time']['nanos'] / 1000000000))
    plt.bar(eqsat, times, linewidth=1)
    plt.xlabel("Eqsat on or off")
    plt.ylabel("Time (seconds)")
    plt.title("Eqsat")
    plt.legend()
    plt.show()


mk_eqsat_time_plot("../out/eqsat_time.json")