import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from functools import reduce
from itertools import groupby
import json

data = json.load(open("parsed.json"))
bool_data = list(filter(lambda x: x['domain'] == "bool", data))
bv4ns_data = list(filter(lambda x: x['domain'] == "bv4ns", data))
# print(bool_data)
def make_choose_eqs_time_plot(data):
    make_choose_eqs_plot(data, lambda x: x["real"])

def make_choose_eqs_plot(data, compare, compare2):
    # todo there must be some way to deal with the iterable 
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    # sum! all iterations and avg across runs
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))

    x = list(map(lambda x: int(x['mrat_m']) , (orat + mrat)))
    x.sort()
    x = [str(d) for d in x]
    y = list(map(lambda x: float(compare(x)), (orat + mrat)))
    y2 = list(map(lambda x: float(compare2(x)), (orat + mrat)))

    fig, (mrat, minimize) = plt.subplots(1,2)

    width = 0.4

    # add a minimize bar
    mrat.bar(x, y, width, color='blue')
    minimize.bar(x, y2, width, color='red')

    # minimize.plot()
    plt.show()

def make_choose_eqs_line_plot(data):
    # todo there must be some way to deal with the iterable 
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    # sum! all iterations and avg across runs
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))
    minimize = list(filter(lambda x: x['type'] == 'minimize', data))

    # want to find all the runs for different types
    orat_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], orat)))
    minimize_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], minimize)))
    mrat.sort(key=lambda x: x["mrat_m"])
    mrat_egraphs_by_m = [list(v) for k,v in groupby(mrat, lambda x: x["mrat_m"])]
    mrat_egraphs_by_m = [dict([('m', lst[0]["mrat_m"]), \
        ('e', aggregate_egraphs_list(list(map(lambda x: x["egraphs"], lst))))]) for lst in mrat_egraphs_by_m]
    print(mrat_egraphs_by_m)
    # so now we have aggregated each iteration over all runs
    # we plot the iterations

    # but for mrat, need to maintain which is which... 
    # do it by item in list 

    # plt.scatter(dates,values)
    # plt.plot(dates, values)

    total = max(len(orat_egraphs), len(minimize_egraphs), max([len(x) for x in mrat_egraphs_by_m]))
    x = list(range(1, total))

    print(total)
    
    plt.scatter(list(range(1, len(orat_egraphs))), orat_egraphs, color="r")
    plt.plot(list(range(1, len(orat_egraphs))), orat_egraphs, color="r")
    
    plt.scatter(list(range(1, len(minimize_egraphs))), minimize_egraphs, color="b")
    plt.plot(list(range(1, len(minimize_egraphs))), minimize_egraphs, color="b")

    for m in range(max([len(x) for x in mrat_egraphs_by_m])):
        plt.scatter(list(range(1, len(m))), m, color="y")
        plt.plot(list(range(1, len(m))), m, color="y")

    # minimize.plot()
    plt.show()

def aggregate_egraphs_list(our_list): 
    # our list has each iteration
    # don't want map...
    # want to take the first item
    num_runs = len(our_list[0])
    print(num_runs)

    total = []

    for i in range(num_runs):
        all = [x[i] for x in our_list]
        one = reduce(lambda acc, x:
            dict([('e', float(x["e"]) + acc["e"]), \
            ('cv', float(x["cv"]) + acc["cv"]), \
            ('n', float(x["n"]) + acc["n"])]), \
            all,
            {'e': 0.0, 'cv': 0.0, 'n': 0.0})
        print(one)
        total.append(one)
    print(total)


    return total


def make_phase_time_plot(data):
    
    # for each series, I want to sum across all three phases
    phase_times = list(filter(lambda x: x['type'] == 'phase-times', data))
    times_only = list(map(lambda x: x['phases'], phase_times))
    print(times_only)

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
    width = 0.4

    fig, ax = plt.subplots()
    print(len(run_rewrites))
    print(len(rule_discovery))
    print(len(rule_minimization))
    ax.bar(legends, run_rewrites, width, label="run_rewrites")
    ax.bar(legends, rule_discovery, width, label="rule_discovery", bottom=run_rewrites)
    ax.bar(legends, rule_minimization, width, label="rule_minimzation", bottom=[sum(x) for x in zip(run_rewrites, rule_discovery)])

    ax.legend()
    plt.show()
    # sum over all 

# make_choose_eqs_time_plot(bool_data)
# make_choose_eqs_time_plot(bv4ns_data)
# make_phase_time_plot(bool_data)
make_choose_eqs_plot(bool_data, lambda x: x["learned"]["time"], lambda x: x["learned"]["rules"])
make_choose_eqs_plot(bv4ns_data, lambda x: x["learned"]["time"], lambda x: x["learned"]["rules"])
# make_choose_eqs_line_plot(bool_data)

# filter by type and plot accordingly

# x = np.array([5,7,8,7,2,17,2,9,4,11,12,9,6])
# y = np.array([99,86,87,88,111,86,103,87,94,78,77,85,86])

# plt.scatter(x, y)
# plt.show()