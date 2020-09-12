import json
import csv


def time(d, nm):
    return (float(d[nm]['secs']) + (float(d[nm]['nanos']) / float(1000000000)))


def ruler_profiler(p):
    with open(p) as f:
        data = json.load(f)
        csv_file = open('../out/ruler_profile.csv', 'w')
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow([
            'iter', 'nloops', 'adding_exps', 'cloning_pristine', 'clean_rules',
            'before_eqsat_ec', 'before_eqsat_en', 'tainted_eqsat',
            'update_pristine', 'before_cvec_ec', 'before_cvec_en',
            'cvec_grouping', 'learn_a_rule', 'after_cvec_ec', 'after_cvec_en',
            'learned_rule'
        ])
        for d in data:
            csv_writer.writerow([
                d['niter'], d['nloop'],
                time(d, 'adding_exprs'),
                time(d, 'cloning_pristine'),
                time(d, 'clean_rules'), d['before_eqsat_eclasses'],
                d['before_eqsat_enodes'],
                time(d, 'tainted_eqsat'),
                time(d, 'update_pristine'), d['before_cvec_eclasses'],
                d['before_cvec_enodes'],
                time(d, 'cvec_grouping'),
                time(d, 'learn_a_rule'), d['after_cvec_eclasses'],
                d['after_cvec_enodes'], d['learned_rule']
            ])
        csv_file.close()


ruler_profiler("../out/ruler_profile.json")