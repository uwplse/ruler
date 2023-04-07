import sys
import json

def mk_table(domain, data):
    const_values = sorted(list(set(d["num_consts"] for d in data)))
    sample_values = sorted(list(set(d["samples"] for d in data)))
    fuzz_values = sorted(list(set(d["v_fuzz"] for d in data)))

    def get(c, s, f):
        for d in data:
            if d["num_consts"] == c and d["samples"] == s and d["v_fuzz"] == f:
                return d
        assert false

    assert const_values[0] == 0
    assert sample_values[0] == 0
    assert fuzz_values[0] == '0'

    table = []

    for c in const_values[1:]:
        row_data = [get(c, 0, f) for f in fuzz_values]
        row = ["$C$ & {}".format(c)]
        row.extend(format_cell(cell) for cell in row_data)
        table.append(row)

    for s in sample_values[1:]:
        row_data = [get(0, s, f) for f in fuzz_values]
        row = ["$R$ & {}".format(s)]
        row.extend(format_cell(cell) for cell in row_data)
        table.append(row)

    return table


def format_cell(cell):
    stat = cell['status'].lower()
    if stat == 'crash':
        return ' & '
    elif stat == 'success':
        rules = cell['num_rules']
        unsound = cell['post_unsound']
        unknown = cell['post_unknown']
        sound = rules - (unsound + unknown)
        if unsound == 0: unsound = '-'
        if unknown == 0: unknown = '-'
        return '{}/{} & {:.1f}s'.format(sound, unsound, cell['time'])
    else:
        assert false

def print_table(table):
    def get_len(i, row):
        if i < len(row):
            return len(row[i])
        else:
            return 0

    n_cols = len(table[0])
    widths = [max(get_len(i, row) for row in table) for i in range(n_cols)]
    for row in table:
        s = " & ".join(str(x).ljust(w) for x,w in zip(row, widths))
        print('   ', s, '\\\\')

def main(filename):
    with open(filename) as f:
        data = json.load(f)

    by_domain = {}
    fuzz_values = sorted(list(set(d["v_fuzz"] for d in data)))

    for run in data:
        domain = by_domain.setdefault(run["domain"], [])
        domain.append(run)

    domains = sorted(list(by_domain.keys()))

    tables = {domain: mk_table(domain, data)
              for domain, data in by_domain.items()}


    n_cols = len(tables[domains[0]][0])
    widths = [max(len(row[i]) for table in tables.values()
                              for row in table)
              for i in range(n_cols)]

    col_alignment = '{|' + '|'.join(['lr'] * n_cols) + '|}'
    print('\\begin{table} \small')
    print('  \\begin{tabular}', col_alignment)
    print('    \\multicolumn{2}{c}{cvec}')
    for f in fuzz_values:
        print('    & \\multicolumn{{2}}{{c}}{{{}}}'.format(f))
    print('    \\\\')
    for domain in domains:
        table = tables[domain]
        print('    \\multicolumn{{{}}}{{l}}{{}} \\\\'.format(n_cols * 2))
        print('    \\multicolumn{{{}}}{{l}}{{{}}} \\\\'.format(n_cols * 2, domain))
        print('    \\hline')
        for row in table:
            s = " & ".join(str(x).ljust(w) for x,w in zip(row, widths))
            print('   ', s, '\\\\')
        print('    \\hline')
    print('  \\end{tabular}')
    print('  \\caption{}')
    print('\\end{table}')

if __name__ == "__main__":
    main(sys.argv[1])

# \begin{center}
# \begin{tabular}{ |c|c|c| }
#  \hline
#  cell1 & cell2 & cell3 \\
#  cell4 & cell5 & cell6 \\
#  cell7 & cell8 & cell9 \\
#  \hline
# \end{tabular}
# \end{center}
