import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from functools import reduce
from itertools import groupby
import json
from colorsys import hsv_to_rgb
from random import randint, uniform


data = json.load(open("parsed.json"))
bool_data = list(filter(lambda x: x['domain'] == "bool", data))
bv4ns_data = list(filter(lambda x: x['domain'] == "bv4ns", data))
# print(bool_data)
def make_choose_eqs_time_rules_plot(data):
    make_choose_eqs_plot(data, lambda x: x["learned"]["time"], lambda x: x["learned"]["rules"])

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
    mrat.set(xlabel="choose_eqs setting", ylabel="s")
    mrat.set_title("Time")
    minimize.bar(x, y2, width, color='red')
    minimize.set(xlabel="choose_eqs setting", ylabel="Rules Learned")
    minimize.set_title("Rules Learned")

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
        ('egraph', aggregate_egraphs_list(list(map(lambda x: x["egraphs"], lst))))]) for lst in mrat_egraphs_by_m]
    print(mrat_egraphs_by_m)
    # so now we have aggregated each iteration over all runs
    # we plot the iterations

    # but for mrat, need to maintain which is which... 
    # do it by item in list 

    # plt.scatter(dates,values)
    # plt.plot(dates, values)

    # total = max(len(orat_egraphs), len(minimize_egraphs), max([len(x) for x in mrat_egraphs_by_m]))
    # x = list(range(1, total))

    # print(total)

    selector = lambda x: x['e']
    
    plt.scatter(list(range(0, len(orat_egraphs))), [selector(x) for x in orat_egraphs], color="r")
    plt.plot(list(range(0, len(orat_egraphs))), [selector(x) for x in orat_egraphs], color="r", label="orat")
    
    plt.scatter(list(range(0, len(minimize_egraphs))), [selector(x) for x in minimize_egraphs], color="b")
    plt.plot(list(range(0, len(minimize_egraphs))), [selector(x) for x in minimize_egraphs], color="b", label="minimize")


    h = 0.27 # green hue in HSV space
    increment = 0.7 / len(mrat_egraphs_by_m) 
    print(increment)

    for (i, m) in enumerate(mrat_egraphs_by_m): #range(max([len(x) for x in mrat_egraphs_by_m])):
        print(m)
        data = m["egraph"]
        
        s = 0.2 + increment * i
        v = 0.3 + increment * i
        r, g, b = hsv_to_rgb(h, s, v)
        # Convert to 0-1 range for HTML output
        # r, g, b = [x*255 for x in (r, g, b)]

        plt.scatter(list(range(0, len(data))), [selector(x) for x in data], color=(r, g, b, 1))
        plt.plot(list(range(0, len(data))), [selector(x) for x in data], color=(r, g, b, 1), label="mrat m=" + m["m"])

    # minimize.plot()
    plt.legend()
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
        total.append(one)


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

def compare_run_rewrites(data):

    y1 = lambda x: float(x["learned"]["time"])
    y2 = lambda x: float(x["learned"]["rules"])
    # TODO: e or n?
    y3 = lambda x: sum([int(d["e"]) for d in x["egraphs"]]) / len(x["egraphs"])

    run_rewrites = list(filter(lambda x: x['type'] == 'no-run-rewrites', data))
    no_run_rewrites = list(filter(lambda x: x['type'] == 'minimize', data)) # regular data

    # rr_x = list(map(lambda d: x(d) , run_rewrites))
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


    # minimize.plot()
    plt.show()

# make_choose_eqs_time_rules_plot(bool_data)
# make_choose_eqs_time_rules_plot(bv4ns_data)
# make_phase_time_plot(bool_data)
compare_run_rewrites(bool_data)

# make_choose_eqs_line_plot(bool_data)
