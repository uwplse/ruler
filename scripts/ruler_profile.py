import json
import csv

def time(d, nm):
    return d[nm]['secs'] + (d[nm]['nanos'] / 1000000000)

def ruler_profiler(p):
    with open(p) as f:
        data = json.load(f)
        csv_file = open('../out/ruler_profile.csv', 'w')
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(['iter', 'nloops', 'adding_exps', 'cloning_pristine', 'clean_rules', 'tainted_eqsat', 'update_pristine', 'cvec_grouping', 'learn_a_rule', 'eclasses', 'enodes'])
        for d in data:
            csv_writer.writerow([d['niter'], d['nloop'], time(d, 'adding_exprs'), time(d, 'cloning_pristine'), time(d, 'clean_rules'), time(d, 'tainted_eqsat'), time(d, 'update_pristine'), time(d, 'cvec_grouping'), time(d, 'learn_a_rule'), d['eclasses'], d['enodes']])
        csv_file.close()

ruler_profiler("../out/ruler_profile.json")