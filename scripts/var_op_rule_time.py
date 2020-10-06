import json
import csv

def mk_csv(p):
    with open(p) as f:
        data = json.load(f)
        csv_file = open('../out/var_op_rule_vs_time.csv', 'w')
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(['iter', 'num_vars', 'num_ops', 'num_rules', 'time'])
        for d in data:
            time = d['time']['secs'] + (d['time']['nanos'] / 1000000000)
            csv_writer.writerow([d['iter'], d['num_vars'], d['num_ops'], d['num_rules'], time])
        csv_file.close()

mk_csv("../out/var_rule_vs_time.json")