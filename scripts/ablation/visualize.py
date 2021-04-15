import matplotlib.pyplot as plt
import numpy as np
import json
from functools import reduce
from itertools import groupby
from colorsys import hsv_to_rgb
from random import randint, uniform
from math import log10, floor


data = json.load(open("output/parsed.json"))
bv4_data = list(filter(lambda x: x['domain'] == "bv4", data))
bv32_data = list(filter(lambda x: x['domain'] == "bv32", data))
rat_data = list(filter(lambda x: x['domain'] == "rational", data))
# print(bool_data)
def make_choose_eqs_time_rules_plot(domain, data, boxplot=False):
    make_choose_eqs_plot(domain, data, lambda x: x["learned"]["time"], lambda x: x["learned"]["rules"], boxplot)

def make_choose_eqs_plot(domain, data, compare, compare2, boxplot):
    # Collect data
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))
    default_conf = list(filter(lambda x: x['type'] == 'default', data))

    x = list(map(lambda x: int(x['mrat_m']) , (orat + mrat)))
    x = list(set(x))
    x.sort()
    x = [str(d) for d in x]
    x.append("def.")

    # average over each item, for each comparison
    orat_y_l= list(map(lambda x: float(compare(x)), orat))
    orat_y = sum(orat_y_l) / len(orat_y_l)
    orat_y2 = list(map(lambda x: float(compare2(x)), orat))
    orat_y2 = sum(orat_y2) / len(orat_y2)

    min_y_l = list(map(lambda x: float(compare(x)), default_conf))
    min_y = sum(min_y_l) / len(min_y_l)
    min_y2 = list(map(lambda x: float(compare2(x)), default_conf))
    min_y2 = sum(min_y2) / len(min_y2)

    # average mrat by each m
    mrat_ys = []
    mrat_ys_l = []
    mrat_y2s = []
    grouped = {}
    mrat.sort(key=lambda x: int(x["mrat_m"]))
    for elem in mrat:
        key = elem["mrat_m"]
        grouped.setdefault(key, []).append(elem)
    grouped = list(grouped.values())

    for lst in grouped:
        res = list(map(lambda x: float(compare(x)), lst))
        mrat_ys.append(sum(res) / len(res))
        mrat_ys_l.append(res)
        res2 = list(map(lambda x: float(compare2(x)), lst))
        mrat_y2s.append(sum(res2) / len(res2))

    print([x[0]["mrat_m"] for x in grouped])
    # print(json.dumps(data, indent=4, sort_keys=True))

    y = [orat_y] + mrat_ys + [min_y]
    y2 = [orat_y2] + mrat_y2s + [min_y2]
    y_l = [orat_y_l] + mrat_ys_l + [min_y_l]
 
    fig, (compare1, compare2) = plt.subplots(1,2)

    width = 0.4

    if boxplot:
        boxplot = compare1.boxplot(y_l, patch_artist=True)
        for patch in boxplot['boxes']:
            patch.set(facecolor="lightblue")  
        compare1.set_xticklabels(x)
    else:
        compare1.bar(x, y, width, color='lightblue')

    compare1.set(xlabel="choose_eqs setting", ylabel="Time (seconds)")
    compare1.set_title("Time")

    compare2.bar(x, y2, width, color='lightsalmon')
    compare2.set(xlabel="choose_eqs setting", ylabel="Rules Learned")
    compare2.set_title("Rules Learned")

    # minimize.plot()
    fig.suptitle(domain)
    plt.tight_layout();
    plt.savefig('output/by-config-rules-learned.pdf')

    # plt.show()

def make_choose_eqs_line_plot(data):
    # Collect data
    orat = list(filter(lambda x: x['type'] == 'orat', data))
    for item in orat: 
        item['mrat_m'] = 1
    
    mrat = list(filter(lambda x: x['type'] == 'mrat', data))
    default_conf = list(filter(lambda x: x['type'] == 'default', data))

    
    # Aggregate all values from the same iteration across runs
    orat_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], orat)))
    default_conf_egraphs = aggregate_egraphs_list(list(map(lambda x: x["egraphs"], default_conf)))
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

    plt.scatter(list(range(0, len(default_conf_egraphs))), [selector(x) for x in default_conf_egraphs], color="gold", marker="*", s=20, label="default")
    plt.plot(list(range(0, len(default_conf_egraphs))), [selector(x) for x in default_conf_egraphs], color="gold")
    
    # minimize.plot()
    fig.suptitle("Number of eclasses by iterations")
    # plt.title("Number of eclasses by iterations")
    plt.legend()
    plt.savefig('output/by-config-eclasses-per-iter.pdf')
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

def compare_phase_times(data, dataset_names, legend_outside=False):
    # TODO: sort in order of the dataset_names
    names = dataset_names
    phase_times = list(filter(lambda x: x['type'] == 'phase-times', data))
    phase_times.sort(key=lambda x: dataset_names.index(x["domain"]))

    print(phase_times)
    # filter for only the ones we asked for 
    # TODO: maybe just make it all?
    phase_times = filter(lambda x: x['domain'] in dataset_names, phase_times)

    phases_by_name = [list(v) for k,v in groupby(phase_times, lambda x: x["domain"])]

    if (len(phases_by_name) != len(dataset_names)):
        raise ValueError("You are plotting datasets not present in parsed.json")
    
    # aggregate to get one value per item...
    sum = lambda acc, x: dict([('run_rewrites', float(x["run_rewrites"]) + acc["run_rewrites"]), \
        ('rule_discovery', float(x["rule_discovery"]) + acc["rule_discovery"]), \
        ('rule_minimization', float(x["rule_minimization"]) + acc["rule_minimization"]),\
        ('rule_validation', float(x["rule_validation"]) + acc["rule_validation"])])
    avg = lambda res, len: dict([('run_rewrites', res["run_rewrites"] / len), \
        ('rule_discovery', res['rule_discovery'] / len), \
            ('rule_minimization', res['rule_minimization'] / len), \
                ('rule_validation', res['rule_validation'] / len)])

    
    # first sum all the items in the inner loop
    # then, avg with the outer loop
    agg_phases = []
    reduce_base = {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0, 'rule_validation': 0.0}
    for run in phases_by_name:
        runs_avg = reduce_base
        for iter in run:
            iter_info = iter['phases']
            inner_sum = reduce(sum, iter_info, reduce_base)
            runs_avg["run_rewrites"] = inner_sum["run_rewrites"]
            runs_avg["rule_discovery"] = inner_sum["rule_discovery"]
            runs_avg["rule_minimization"] = inner_sum["rule_minimization"]
            runs_avg["rule_validation"] = inner_sum["rule_validation"]
            print(runs_avg)
        runs_avg = avg(runs_avg, len(run))
        print(runs_avg)

        agg_phases.append(runs_avg)
        
    print(agg_phases)
    
    fig, ax = plt.subplots(1)
    
    run_rewrites = list(map(lambda x: x['run_rewrites'], agg_phases))
    rule_discovery = list(map(lambda x: x['rule_discovery'], agg_phases))
    rule_minimization = list(map(lambda x: x['rule_minimization'], agg_phases))
    rule_validation = list(map(lambda x: x['rule_validation'], agg_phases))

    # cancel out small validation
    rule_validation = [0 if (x < 10e-5) else x for x in rule_validation]

    x = np.arange(len(names))

    width = 0.6

    ax.bar(x, run_rewrites, width/4, label="run_rewrites", color="burlywood")
    ax.bar(x + width/4, rule_discovery, width/4, label="rule_discovery", color="skyblue")
    ax.bar(x + width/2, rule_minimization, width/4, label="rule_minimization", color="firebrick")
    ax.bar(x + width * 0.75, rule_validation, width/4, label="rule_validation", color="indigo")

    ax.set_xticks(x + width * 3 / 8)
    ax.set_xticklabels(names)

    # TODO: bools, etc. have a nonzero validation rn because the logging is not 
    # properly done inside new choose_eqs. Need to fix (inside partition)
    ax.set_yscale('log')

    # https://stackoverflow.com/questions/28931224/adding-value-labels-on-a-matplotlib-bar-chart
    rects = ax.patches
    labels = [[x["run_rewrites"], x["rule_discovery"], x["rule_minimization"], x["rule_validation"]] for x in agg_phases]
    labels = run_rewrites + rule_discovery + rule_minimization + rule_validation
    # https://stackoverflow.com/questions/3410976/how-to-round-a-number-to-significant-figures-in-python
    round_to_n = lambda x, n: x if x == 0 else round(x, -int(floor(log10(abs(x)))) + (n - 1))

    # Flatten
    labels = [round_to_n(item,2) for item in labels]
    
    print(labels)

    for rect, label in zip(rects, labels):
        # rects are probably in order
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width() / 2, height, label, ha='center', va='bottom', size=6)

    if legend_outside:
        plt.legend(bbox_to_anchor=(1.04,0.5), loc="center left", borderaxespad=0)
        plt.subplots_adjust(right=0.7)
        fig.set_size_inches(9,5)
        plt.savefig("output/by-domain-phase-times-legend-outside.pdf")
    else:
        plt.legend()
        plt.savefig("output/by-domain-phase-times.pdf")

    

    # plt.show()
    # inner_sum = [reduce(sum, iter, reduce_base) for iter in [run for run in phases_by_name]]
    # runs_avg = [avg(run, len(run)) for run in phases_by_name]

    # print(runs_avg)


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
        ('rule_minimization', float(x["rule_minimization"]) + acc["rule_minimization"]), \
        ('rule_validation', float(x["rule_validation"]) + acc["rule_validation"])]), \
        times, 
        {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0, 'rule_validation': 0.0}),\
        times_only)))

    run_rewrites = list(map(lambda x: x['run_rewrites'], agg_times))
    rule_discovery = list(map(lambda x: x['rule_discovery'], agg_times))
    rule_minimization = list(map(lambda x: x['rule_minimization'], agg_times))
    rule_validation = list(map(lambda x: x['rule_validation'], agg_times))

    legends = list(map(lambda x: x['run'], phase_times))
    width = 0.8

    fig, (ax, axg) = plt.subplots(1,2, sharey=True)
    axg.yaxis.set_tick_params(labelbottom=True)

    ax.bar(legends, run_rewrites, width, label="run_rewrites", color="burlywood")
    ax.bar(legends, rule_discovery, width, label="rule_discovery", bottom=run_rewrites, color="skyblue")
    ax.bar(legends, rule_minimization, width, label="rule_minimzation", bottom=[sum(x) for x in zip(run_rewrites, rule_discovery)], color="firebrick")
    ax.bar(legends, rule_validation, width, label="rule_validation", bottom=[sum(x) for x in zip(run_rewrites, rule_discovery, rule_minimization)], color="indigo")

    # get iter locations
    x = np.arange(len(run_rewrites))

    axg.bar(x - width/2, run_rewrites, width/4, label="run_rewrites", color="burlywood")
    axg.bar(x - width / 4, rule_discovery, width/4, label="rule_discovery", color="skyblue")
    axg.bar(x + width/4, rule_minimization, width/4, label="rule_minimization", color="firebrick")
    axg.bar(x + width/2, rule_validation, width/4, label="rule_validation", color="indigo")

    # TODO: fix the legend now

    ax.legend()
    fig.suptitle("Time spent in each phase (by run)")
    plt.savefig('output/phase-times.pdf')
    #plt.show()

    # sum over all 

def compare_run_rewrites(data):

    y1 = lambda x: float(x["learned"]["time"])
    y2 = lambda x: float(x["learned"]["rules"])
    # Use e here because number of e-classes
    y3 = lambda x: sum([int(d["e"]) for d in x["egraphs"]]) / len(x["egraphs"])

    run_rewrites = list(filter(lambda x: x['type'] == 'no-run-rewrites', data))
    no_run_rewrites = list(filter(lambda x: x['type'] == 'default', data)) # regular data

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
    egraphs.set_title("Num E-classes")

    fig.suptitle("Applying run_rewrites vs not applying run_rewrites")
    # minimize.plot()
    # plt.show()
    plt.tight_layout()
    plt.savefig('output/run-rewrites.pdf')

# make_choose_eqs_time_rules_plot("bv4", bv4_data, boxplot=False)
# compare_run_rewrites(bv4_data)

# make_choose_eqs_time_rules_plot("bv32", bv32_data, boxplot=False)
# compare_run_rewrites(bv32_data)

# make_choose_eqs_time_rules_plot("Rationals", rat_data, boxplot=False)
# compare_run_rewrites(rat_data)

compare_phase_times(data, ["bv4", "bv32", "rational"])
compare_phase_times(data, ["bv4", "bv32", "rational"], legend_outside=True)
# compare_phase_times(data, ["bool", "bv4ns", "bv8", "bv16", "bv32", "float", "rational"])
