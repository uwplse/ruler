import matplotlib.pyplot as plt
import numpy as np
import json
from functools import reduce
from itertools import groupby
from colorsys import hsv_to_rgb
from random import randint, uniform


data = json.load(open("output/parsed.json"))
bool_data = list(filter(lambda x: x['domain'] == "bool", data))
bv4ns_data = list(filter(lambda x: x['domain'] == "bv4ns", data))
# print(bool_data)
def make_choose_eqs_time_rules_plot(domain, data):
    make_choose_eqs_plot(domain, data, lambda x: x["learned"]["time"], lambda x: x["learned"]["rules"])

def make_choose_eqs_plot(domain, data, compare, compare2):
    # Collect data
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))
    minimize = list(filter(lambda x: x['type'] == 'minimize', data))

    x = list(map(lambda x: int(x['mrat_m']) , (orat + mrat)))
    x = list(set(x))
    x.sort()
    x = [str(d) for d in x]
    x.append("min.")

    # average over each item, for each comparison
    orat_y = list(map(lambda x: float(compare(x)), orat))
    orat_y = sum(orat_y) / len(orat_y)
    orat_y2 = list(map(lambda x: float(compare2(x)), orat))
    orat_y2 = sum(orat_y2) / len(orat_y2)

    min_y = list(map(lambda x: float(compare(x)), minimize))
    min_y = sum(min_y) / len(min_y)
    min_y2 = list(map(lambda x: float(compare2(x)), minimize))
    min_y2 = sum(min_y2) / len(min_y2)

    # average mrat by each m
    mrat_ys = []
    mrat_y2s = []
    grouped = {}
    for elem in mrat:
        key = elem["mrat_m"]
        grouped.setdefault(key, []).append(elem)
    grouped = list(grouped.values())

    for lst in grouped:
        res = list(map(lambda x: float(compare(x)), lst))
        mrat_ys.append(sum(res) / len(res))
        res2 = list(map(lambda x: float(compare2(x)), lst))
        mrat_y2s.append(sum(res2) / len(res2))

    y = [orat_y] + mrat_ys + [min_y]
    y2 = [orat_y2] + mrat_y2s + [min_y2]

    fig, (compare1, compare2) = plt.subplots(1,2)

    width = 0.4

    compare1.bar(x, y, width, color='lightblue')
    compare1.set(xlabel="choose_eqs setting", ylabel="s")
    compare1.set_title("Time")

    compare2.bar(x, y2, width, color='lightsalmon')
    compare2.set(xlabel="choose_eqs setting", ylabel="Rules Learned")
    compare2.set_title("Rules Learned")

    # minimize.plot()
    fig.suptitle(domain)
    plt.savefig('output/by_config_rules_learned.pdf')

    # plt.show()

def make_choose_eqs_line_plot(data):
    # Collect data
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))
    minimize = list(filter(lambda x: x['type'] == 'minimize', data))

    
    # Aggregate all values from the same iteration across runs
    orat_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], orat)))
    minimize_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], minimize)))
    mrat.sort(key=lambda x: x["mrat_m"])
    mrat_egraphs_by_m = [list(v) for k,v in groupby(mrat, lambda x: x["mrat_m"])]
    mrat_egraphs_by_m = [dict([('m', lst[0]["mrat_m"]), \
        ('egraph', aggregate_egraphs_list(list(map(lambda x: x["egraphs"], lst))))]) for lst in mrat_egraphs_by_m]
    print(mrat_egraphs_by_m)

    # Use number of e-classes
    selector = lambda x: x['e']

    fig, ax = plt.subplots(1) 
    plt.scatter(list(range(0, len(orat_egraphs))), [selector(x) for x in orat_egraphs], color="powderblue", label="orat")
    plt.plot(list(range(0, len(orat_egraphs))), [selector(x) for x in orat_egraphs], color="powderblue")
    
    # Generate similar colours for different mrat m
    h_0 = 0.6 # blue in HSV
    increment = 0.7 / len(mrat_egraphs_by_m) 
    print(increment)

    for (i, m) in enumerate(mrat_egraphs_by_m): #range(max([len(x) for x in mrat_egraphs_by_m])):
        print(m)
        data = m["egraph"]
        
        h = h_0 + 0.07 * i
        s = 0.2 + increment * i
        v = 0.3 + increment * i
        r, g, b = hsv_to_rgb(h, s, v)

        plt.scatter(list(range(0, len(data))), [selector(x) for x in data], color=(r, g, b, 1), label="mrat m=" + m["m"])
        plt.plot(list(range(0, len(data))), [selector(x) for x in data], color=(r, g, b, 1))

    plt.scatter(list(range(0, len(minimize_egraphs))), [selector(x) for x in minimize_egraphs], color="gold", marker="*", s=20, label="minimize")
    plt.plot(list(range(0, len(minimize_egraphs))), [selector(x) for x in minimize_egraphs], color="gold")
    
    # minimize.plot()
    fig.suptitle("Number of eclasses by iterations")
    # plt.title("Number of eclasses by iterations")
    plt.legend()
    plt.savefig('output/by_config_eclasses_per_iter.pdf')
    #plt.show()

# Aggregate all values from same iteration across runs
def aggregate_egraphs_list(our_list):
    num_runs = len(our_list[0])
    print(num_runs)

    total = []

    for i in range(num_runs):
        all = [x[i] for x in our_list]
        one = reduce(lambda acc, x:
            dict([('e', float(x["e"]) + acc["e"]), \
            ('n', float(x["n"]) + acc["n"])]), \
            all,
            {'e': 0.0, 'n': 0.0})
        total.append(one)


    return total


def make_phase_time_plot(data):
    
    phase_times = list(filter(lambda x: x['type'] == 'phase-times', data))
    times_only = list(map(lambda x: x['phases'], phase_times))
    print(times_only)

    # sum time spent in each phase across all iterations
    # becomes a list of time values, each corresponding to one run
    agg_times = (list(map(lambda times: \
        reduce(lambda acc, x: \
        dict([('run_rewrites', float(x["run_rewrites"]) + acc["run_rewrites"]), \
        ('rule_discovery', float(x["rule_discovery"]) + acc["rule_discovery"]), \
        ('rule_minimization', float(x["rule_minimization"]) + acc["rule_minimization"])]), \
        times, 
        {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0}),\
        times_only)))

    run_rewrites = list(map(lambda x: x['run_rewrites'], agg_times))
    rule_discovery = list(map(lambda x: x['rule_discovery'], agg_times))
    rule_minimization = list(map(lambda x: x['rule_minimization'], agg_times))

    legends = list(map(lambda x: x['run'], phase_times))
    width = 0.8

    fig, (ax, axg) = plt.subplots(1,2, sharey=True)
    axg.yaxis.set_tick_params(labelbottom=True)

    ax.bar(legends, run_rewrites, width, label="run_rewrites", color="burlywood")
    ax.bar(legends, rule_discovery, width, label="rule_discovery", bottom=run_rewrites, color="skyblue")
    ax.bar(legends, rule_minimization, width, label="rule_minimzation", bottom=[sum(x) for x in zip(run_rewrites, rule_discovery)], color="firebrick")

    # get iter locations
    x = np.arange(len(run_rewrites))

    axg.bar(x - width/3, run_rewrites, width/3, label="run_rewrites", color="burlywood")
    axg.bar(x, rule_discovery, width/3, label="rule_discovery", color="skyblue")
    axg.bar(x + width/3, rule_minimization, width/3, label="rule_minimization", color="firebrick")

    ax.legend()
    fig.suptitle("Time spent in each phase (by run)")
    plt.savefig('output/phase_times.pdf')
    #plt.show()

    # sum over all 

def compare_run_rewrites(data):

    y1 = lambda x: float(x["learned"]["time"])
    y2 = lambda x: float(x["learned"]["rules"])
    # Use e here because number of e-classes
    y3 = lambda x: sum([int(d["e"]) for d in x["egraphs"]]) / len(x["egraphs"])

    run_rewrites = list(filter(lambda x: x['type'] == 'no-run-rewrites', data))
    no_run_rewrites = list(filter(lambda x: x['type'] == 'minimize', data)) # regular data

    x = ["No RR", "RR"]
    rr_y1 = list(map(lambda d: y1(d) , run_rewrites))
    rr_y1 = sum(rr_y1) / len(rr_y1)
    rr_y2 = list(map(lambda d: y2(d) , run_rewrites))
    rr_y2 = sum(rr_y2) / len(rr_y2)
    rr_y3 = list(map(lambda d: y3(d) , run_rewrites))
    print(rr_y3)
    rr_y3 = sum(rr_y3) / len(rr_y3)
    
    no_rr_y1 = list(map(lambda d: y1(d) , no_run_rewrites))
    no_rr_y1 = sum(no_rr_y1) / len(no_rr_y1)
    no_rr_y2 = list(map(lambda d: y2(d) , no_run_rewrites))
    no_rr_y2 = sum(no_rr_y2) / len(no_rr_y2)
    no_rr_y3 = list(map(lambda d: y3(d) , no_run_rewrites))
    print(no_rr_y3)
    no_rr_y3 = sum(no_rr_y3) / len(no_rr_y3)

    fig, (time, rules, egraphs) = plt.subplots(1,3)

    width = 0.4

    # average them together

    # add a minimize bar
    time.bar(x, [rr_y1, no_rr_y1], width, color='thistle')
    # time.set(xlabel="X", ylabel="Y")
    time.set_title("Time (s)")
    rules.bar(x, [rr_y2, no_rr_y2], width, color='cadetblue')
    # rules.set(xlabel="X", ylabel="Y")
    rules.set_title("Rules")
    egraphs.bar(x, [rr_y3, no_rr_y3], width, color='gold')
    # egraphs.set(xlabel="X", ylabel="Y")
    egraphs.set_title("Egraph size (eclasses)")

    fig.suptitle("Applying run_rewrites vs not applying run_rewrites")
    # minimize.plot()
    # plt.show()
    plt.savefig('output/run_rewrites.pdf')

make_choose_eqs_time_rules_plot("Bool", bool_data)
# make_choose_eqs_time_rules_plot("4-bit Bitvector no-shift", bv4ns_data)
make_phase_time_plot(bool_data)
compare_run_rewrites(bool_data)
make_choose_eqs_line_plot(bool_data)
