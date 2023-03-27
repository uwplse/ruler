import matplotlib.pyplot as plt
import numpy as np
import json
import sys
from functools import reduce
from itertools import groupby
from colorsys import hsv_to_rgb
from random import randint, uniform
from math import log10, floor


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

    # Collect labels for configurations (number of rules picked)
    x = list(map(lambda x: int(x['mrat_m']) , (orat + mrat)))
    x = list(set(x))
    x.sort()
    x = [str(d) for d in x]
    x.append("def.")

    # average both metrics over all runs
    orat_y_l= list(map(lambda x: float(compare(x)), orat))
    orat_y = sum(orat_y_l) / len(orat_y_l)
    orat_y2 = list(map(lambda x: float(compare2(x)), orat))
    orat_y2 = sum(orat_y2) / len(orat_y2)

    # same for default configuration
    min_y_l = list(map(lambda x: float(compare(x)), default_conf))
    min_y = sum(min_y_l) / len(min_y_l)
    min_y2 = list(map(lambda x: float(compare2(x)), default_conf))
    min_y2 = sum(min_y2) / len(min_y2)

    # same for mrat, but we need it for each m
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
        # print([compare(x) for x in lst])
        res = list(map(lambda x: float(compare(x)), lst))
        mrat_ys.append(sum(res) / len(res))
        avg = sum(res) / len(res)
        var = sum((x-avg)**2 for x in res) / len(res)
        # print(var / avg)

        mrat_ys_l.append(res)
        res2 = list(map(lambda x: float(compare2(x)), lst))
        mrat_y2s.append(sum(res2) / len(res2))
        avg2 = sum(res2) / len(res2)
        var2 = sum((x-avg2)**2 for x in res2) / len(res2)
        # print(var2 / avg2)

    # print([x[0]["mrat_m"] for x in grouped])
    # print(mrat_ys_l)
    # print(json.dumps(data, indent=4, sort_keys=True))

    # put all y-values together
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

    compare1.set(xlabel="choose_eqs setting", ylabel="Time (s)")
    compare1.set_title("Time")

    compare2.bar(x, y2, width, color='lightsalmon')
    compare2.set(xlabel="choose_eqs setting", ylabel="Rules Learned")
    compare2.set_title("Rules Learned")

    # minimize.plot()
    fig.suptitle(domain)
    plt.tight_layout();
    plt.savefig(output_path + domain + '-by-config-rules-learned.pdf')

    # plt.show()

# Aggregate all values from same iteration across runs
# Specifically statistics about egraph size
def aggregate_egraphs_list(our_list):
    num_runs = len(our_list[0])
    # print(num_runs)

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

# Look at how long each phase takes across different domains
def compare_phase_times(data, dataset_names, legend_outside=False, sigdigs=False):
    names = dataset_names
    phase_times = list(filter(lambda x: x['type'] == 'phase-times', data))
    phase_times.sort(key=lambda x: dataset_names.index(x["domain"]))

    # print(phase_times)
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
    agg_phases_domain = {'run_rewrites': [], 'rule_discovery': [], 'rule_minimization': [], 'rule_validation': []}

    # Goal is to get one value for each phase per domain
    # Since there are a bunch of entries (each phase happens multiple times in a run)
    reduce_base = {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0, 'rule_validation': 0.0}
    for run in phases_by_name:
        runs_avg = reduce_base
        agg_phases_l = {'run_rewrites': [], 'rule_discovery': [], 'rule_minimization': [], 'rule_validation': []}
        for iter in run:
            iter_info = iter['phases']
            inner_sum = reduce(sum, iter_info, reduce_base)
            # print(inner_sum)
            runs_avg["run_rewrites"] = inner_sum["run_rewrites"]
            runs_avg["rule_discovery"] = inner_sum["rule_discovery"]
            runs_avg["rule_minimization"] = inner_sum["rule_minimization"]
            runs_avg["rule_validation"] = inner_sum["rule_validation"]

            # agg_phases_l["run_rewrites"].append(inner_sum["run_rewrites"])
            # agg_phases_l["rule_discovery"].append(inner_sum["rule_discovery"])
            # agg_phases_l["rule_minimization"].append(inner_sum["rule_minimization"])
            # agg_phases_l["rule_validation"].append(inner_sum["rule_validation"])
            # print(runs_avg)
        runs_avg = avg(runs_avg, len(run))
        # print(runs_avg)

        agg_phases.append(runs_avg)
        
        # agg_phases_domain["run_rewrites"].append(agg_phases_l["run_rewrites"])
        # agg_phases_domain["rule_discovery"].append(agg_phases_l["rule_discovery"])
        # agg_phases_domain["rule_minimization"].append(agg_phases_l["rule_minimization"])
        # agg_phases_domain["rule_validation"].append(agg_phases_l["rule_validation"])
        
        
    # print(agg_phases)
    # print(agg_phases_domain)
    
    fig, ax = plt.subplots(1)
    
    run_rewrites = list(map(lambda x: x['run_rewrites'], agg_phases))
    rule_discovery = list(map(lambda x: x['rule_discovery'], agg_phases))
    rule_minimization = list(map(lambda x: x['rule_minimization'], agg_phases))
    rule_validation = list(map(lambda x: x['rule_validation'], agg_phases))

    # run_rewrites_domain = list(map(lambda x: x['run_rewrites'], agg_phases_domain))
    # rule_discovery_domain = list(map(lambda x: x['rule_discovery'], agg_phases_domain))
    # rule_minimization_domain = list(map(lambda x: x['rule_minimization'], agg_phases_domain))
    # rule_validation_domain = list(map(lambda x: x['rule_validation'], agg_phases_domain))

    # cancel out small validation
    rule_validation = [0 if (x < 10e-5) else x for x in rule_validation]

    x = np.arange(len(names))

    width = 0.6

    ax.bar(x, run_rewrites, width/4, label="run_rewrites", color="palegoldenrod")
    ax.bar(x + width/4, rule_discovery, width/4, label="rule_discovery", color="skyblue")
    ax.bar(x + width/2, rule_minimization, width/4, label="rule_minimization", color="peru")
    ax.bar(x + width * 0.75, rule_validation, width/4, label="rule_validation", color="rebeccapurple")

    ax.set_xticks(x + width * 3 / 8)
    ax.set_xticklabels(names)
    ax.set(ylabel="Time (s)")

    # TODO: bools, etc. have a nonzero validation rn because the logging is not 
    # properly done inside new choose_eqs. Need to fix (inside partition)
    ax.set_yscale('log')

    # Grouped barchart, so this is important
    # https://stackoverflow.com/questions/28931224/adding-value-labels-on-a-matplotlib-bar-chart
    rects = ax.patches
    labels = [[x["run_rewrites"], x["rule_discovery"], x["rule_minimization"], x["rule_validation"]] for x in agg_phases]
    labels = run_rewrites + rule_discovery + rule_minimization + rule_validation
    # https://stackoverflow.com/questions/3410976/how-to-round-a-number-to-significant-figures-in-python
    # SIGDIGS
    # Also we want to add some labels to the top of each bar, so format with sigdigs
    if sigdigs:
        round_to_n = lambda x, n: x if x == 0 else round(x, -int(floor(log10(abs(x)))) + (n - 1))
    else:
        # hundredths
        round_to_n = lambda x, n: round(x,n)

    # Flatten
    labels = [round_to_n(item,2) for item in labels]
    
    # print(labels)

    for rect, label in zip(rects, labels):
        # rects are probably in order
        height = rect.get_height()
        
        if legend_outside:
            ax.text(rect.get_x() + rect.get_width() / 2, height, label, ha='center', va='bottom', size=6)
        else:
            ax.text(rect.get_x() + rect.get_width() / 2, height, label, ha='center', va='bottom', size=8)

    # May not be enough space to fit the legend inside
    if legend_outside:
        plt.legend(bbox_to_anchor=(1.04,0.5), loc="center left", borderaxespad=0)
        plt.subplots_adjust(right=0.7)
        fig.set_size_inches(9,5)
        plt.savefig(output_path + "by-domain-phase-times-legend-outside.pdf")
    else:
        plt.legend()
        fig.set_size_inches(10,5)
        plt.savefig(output_path + "by-domain-phase-times.pdf")

    

    # plt.show()
    # inner_sum = [reduce(sum, iter, reduce_base) for iter in [run for run in phases_by_name]]
    # runs_avg = [avg(run, len(run)) for run in phases_by_name]

    # print(runs_avg)

def compare_run_rewrites(data, domain):

    y1 = lambda x: float(x["learned"]["time"])
    y2 = lambda x: float(x["learned"]["rules"])
    # Use e here because we're looking at number of e-classes
    y3 = lambda x: sum([int(d["e"]) for d in x["egraphs"]]) / len(x["egraphs"])

    # TODO: lol these variable names are swapped but the labels swap them back
    # FIX WHEN I HAVE TIME
    run_rewrites = list(filter(lambda x: x['type'] == 'no-run-rewrites', data))
    no_run_rewrites = list(filter(lambda x: x['type'] == 'default', data)) # regular data

    x = ["No RR", "RR"]
    rr_y1 = list(map(lambda d: y1(d) , run_rewrites))
    rr_y1 = sum(rr_y1) / len(rr_y1)
    rr_y2 = list(map(lambda d: y2(d) , run_rewrites))
    rr_y2 = sum(rr_y2) / len(rr_y2)
    rr_y3 = list(map(lambda d: y3(d) , run_rewrites))
    # print(rr_y3)
    rr_y3 = sum(rr_y3) / len(rr_y3)
    
    no_rr_y1 = list(map(lambda d: y1(d) , no_run_rewrites))
    no_rr_y1 = sum(no_rr_y1) / len(no_rr_y1)
    no_rr_y2 = list(map(lambda d: y2(d) , no_run_rewrites))
    no_rr_y2 = sum(no_rr_y2) / len(no_rr_y2)
    no_rr_y3 = list(map(lambda d: y3(d) , no_run_rewrites))
    # print(no_rr_y3)
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

    fig.suptitle(domain)
    # minimize.plot()
    # plt.show()
    plt.tight_layout()
    plt.savefig(output_path + domain + '-run-rewrites.pdf')

def compare_phase_times_run_rewrites(domain, data, legend_outside=False):
    # TODO: sort in order of the dataset_names
    names = ["default", "no-run-rewrites"]
    phase_times = data
    phase_times.sort(key=lambda x: names.index(x["type"]))

    # print(phase_times)
    # filter for only the ones we asked for 
    # TODO: maybe just make it all?
    phase_times = filter(lambda x: x['type'] in names, phase_times)

    phases_by_name = [list(v) for k,v in groupby(phase_times, lambda x: x["type"])]

    if (len(phases_by_name) != len(names)):
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

    # Again, collect all time information
    for run in phases_by_name:
        runs_avg = {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0, 'rule_validation': 0.0}
        # print(len(run))
        for iter in run:
            iter_info = iter['phases']
            inner_sum = reduce(sum, iter_info, {'run_rewrites': 0.0, 'rule_discovery': 0.0, 'rule_minimization': 0.0, 'rule_validation': 0.0})
            # print(inner_sum)
            runs_avg["run_rewrites"] += inner_sum["run_rewrites"]
            runs_avg["rule_discovery"] += inner_sum["rule_discovery"]
            runs_avg["rule_minimization"] += inner_sum["rule_minimization"]
            runs_avg["rule_validation"] += inner_sum["rule_validation"]
        # TODO: get variance
        runs_avg = avg(runs_avg, len(run))
        # print(runs_avg)

        agg_phases.append(runs_avg)
        
    # print(agg_phases)
    
    fig, ax = plt.subplots(1)
    
    run_rewrites = list(map(lambda x: x['run_rewrites'], agg_phases))
    rule_discovery = list(map(lambda x: x['rule_discovery'], agg_phases))
    rule_minimization = list(map(lambda x: x['rule_minimization'], agg_phases))
    rule_validation = list(map(lambda x: x['rule_validation'], agg_phases))

    # cancel out small validation (just noise from function calls, etc. from logging)
    rule_validation = [0 if (x < 10e-2) else x for x in rule_validation]
    run_rewrites = [0 if (x < 10e-5) else x for x in run_rewrites]

    x = np.arange(len(names))

    width = 0.6

    # Grouped barchart
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
    
    # print(labels)

    for rect, label in zip(rects, labels):
        # rects are probably in order
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width() / 2, height, label, ha='center', va='bottom', size=6)

    if legend_outside:
        plt.legend(bbox_to_anchor=(1.04,0.5), loc="center left", borderaxespad=0)
        plt.subplots_adjust(right=0.7)
        fig.set_size_inches(9,5)
        plt.savefig(output_path + domain + "-by-domain-phase-times-legend-outside.pdf")
    else:
        plt.legend()
        plt.savefig(output_path + domain + "-by-domain-phase-times.pdf")

def compare_run_rewrites_with_timeout(domain, data, max, set_max=False):

    y1 = lambda x: float(x["learned"]["time"])
    y2 = lambda x: float(x["learned"]["rules"])
    # Use e here because number of e-classes
    y3 = lambda x: sum([int(d["e"]) for d in x["egraphs"]]) / len(x["egraphs"])

    no_run_rewrites = list(filter(lambda x: x['type'] == 'no-run-rewrites', data))
    run_rewrites = list(filter(lambda x: x['type'] == 'default', data)) # regular data

    x = ["No RR", "RR"]
    rr_y1 = list(map(lambda d: y1(d) , run_rewrites))
    rr_y1 = sum(rr_y1) / len(rr_y1)
    rr_y2 = list(map(lambda d: y2(d) , run_rewrites))
    rr_y2 = sum(rr_y2) / len(rr_y2)
    rr_y3 = list(map(lambda d: y3(d) , run_rewrites))
    # print(rr_y3)
    rr_y3 = sum(rr_y3) / len(rr_y3)
    
    # no_rr_y1 = 0
    # no_rr_y2 = 0
    # no_rr_y3 = 0

    no_rr_y1 = 60 * 60 * 24
    no_rr_y2 = 1841
    no_rr_y3 = 23000

    fig, (time, rules, egraphs) = plt.subplots(1,3)

    width = 0.4

    # average them together

    # print(rr_y1)
    # add a minimize bar
    time.bar(x, [no_rr_y1, rr_y1], width, color=['lightgray', 'thistle'])
    # time.set(xlabel="X", ylabel="Y")
    time.set_title("Time (s)")
    rules.bar(x, [no_rr_y2, rr_y2], width, color=['lightgray', 'cadetblue'])
    # rules.set(xlabel="X", ylabel="Y")
    rules.set_title("Rules")
    egraphs.bar(x, [no_rr_y3, rr_y3], width, color=['lightgray', 'gold'])
    # egraphs.set(xlabel="X", ylabel="Y")


    for plot in [time, rules, egraphs]:
        labels = ["TIMEOUT", ""]
        rects = plot.patches
        for rect, label in zip(rects, labels):
            # rects are probably in order
            height = rect.get_height()
            plot.text(rect.get_x() + rect.get_width() / 2 + 0.03, height/8*5, label, ha='center', va='bottom', size=20, color="red", rotation=90)

    # print(plt.xticks())
    if set_max:
        time.set_ylim([0, max[0]])
        rules.set_ylim([0, max[1]])
        egraphs.set_ylim([0, max[2]])

    egraphs.set_title("Num E-classes")

    fig.suptitle("Rationals")
    # minimize.plot()
    # plt.show()
    plt.tight_layout()
    plt.savefig(output_path + 'rr-' + domain + '-timeout.pdf')

output_path = ""

def main():
    global output_path

    if len(sys.argv) == 1:
        path = "submitted-data/compare/parsed.json"
        path_rr = "submitted-data/no-rr/parsed.json"
        output_path = "output/"
    else:
        path = sys.argv[1] + "compare/parsed.json"
        path_rr = sys.argv[1] + "no-rr/parsed.json"
        output_path = sys.argv[1]
        
    data = json.load(open(path))
    data_rr = json.load(open(path_rr))
    filter_data = lambda name: list(filter(lambda x: x['domain'] == name, data))
    filter_data_rr = lambda name: list(filter(lambda x: x['domain'] == name, data_rr))

    make_choose_eqs_time_rules_plot("bv4", filter_data("bv4"), boxplot=False)
    # compare_run_rewrites(filter_data("bv4"), "bv4")

    make_choose_eqs_time_rules_plot("bv32", filter_data("bv32"), boxplot=False)
    # compare_run_rewrites(filter_data("bv32"), "bv32")

    make_choose_eqs_time_rules_plot("rational", filter_data("rational"), boxplot=False)
    # compare_run_rewrites(filter_data("rational"), "rational")

    compare_phase_times(data, ["bv4", "bv32", "rational"])

    # get data... all domains
    # compare_phase_times_run_rewrites("bv4", filter_data_rr("bv4"), legend_outside=True)
    # compare_phase_times_run_rewrites("bv32", filter_data_rr("bv32"), legend_outside=True)
    compare_run_rewrites(filter_data_rr("bv4"), "rr-bv4")
    compare_run_rewrites(filter_data_rr("bv32"), "rr-bv32")
    compare_run_rewrites_with_timeout("rational", filter_data_rr("rational"), [1000, 150, 10000], set_max=False)


main()