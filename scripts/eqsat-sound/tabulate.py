import sys
import json
from itertools import groupby

# NOTE: post_unknown and post_unsound are for postpass
# NOTE: v_smt_unknown is for smt validation during Ruler

#consts=[1, 2, 3, 5]
bv_cvec = [27, 343, 1331, 6859]
rat_cvec = [1, 27, 125, 729]

def num_const_to_cvec (domain, c):
    if (domain == "rational"):
        if c == 0:
            return 0
        if c == 1:
            return rat_cvec[0]
        elif c == 2:
            return rat_cvec[1]
        elif c == 3:
            return rat_cvec[2]
        elif c == 5:
            return rat_cvec[3]
        else:
            print("value of c is not supported." + str(c))
    elif (domain == "32" or domain == "4" or domain == "less-const"):
        if c == 0:
            return 0
        if c == 1:
            return bv_cvec[0]
        elif c == 2:
            return bv_cvec[1]
        elif c == 3:
            return bv_cvec[2]
        elif c == 5:
            return bv_cvec[3]
        else:
            print("value of c is not supported." + str(c))

def to_table(filename): 
    with open(filename) as f:
        data = json.load(f)

        # get the items we need
        # and put into a list of dicts
        entries = []

# TODO perhaps this processing is unnecessary
        for info in data:
            entry = {}
            entry["status"] = info['status']
            entry["domain"] = str(info['domain'])
            entry["time"] = str(info["time"])
            entry["rules"] = info["num_rules"]
            entry["unsound"] = str(info["post_unsound"])
            entry["unknown"] =  str(info["post_unknown"])
            entry["cvec"] = num_const_to_cvec(info["domain"], info["num_consts"])
            entry["samples"] = info["samples"]
            entry["fuzz"] = info["v_fuzz"]
            entry["smt"] = info["v_fuzz"] == "smt"
            entries.append(entry)

        # now we must begin to construct something of a table
        # the columns of the table are the fuzz/smt, so we should
        # collect/group
        # should I group by...?

        print("\n % rational")
        make_table(list(filter(lambda x: x["domain"] == "rational", entries)))
        print("\n % bv less consts")
        make_table(list(filter(lambda x: x["domain"] == "less-const", entries)))
        print("\n % bv 32")
        make_table(list(filter(lambda x: x["domain"] == "32", entries)))
        print("\n % bv 4")
        make_table(list(filter(lambda x: x["domain"] == "4", entries)))
        

# \multirow{3}*{1} & success & success & success \\
# & 0.07563307 & 0.067024701 & 0.718618355 \\
# & unsound: 1 & unsound: 0 & unsound: 0 \\

def make_table(entries):
    entries = entries # I don't know about arguments and I don't plan to
    fuzz_cols = len(set([x["fuzz"] for x in entries]))

    by_cvec = list(filter(lambda x: x["cvec"] != 0, entries))
    by_cvec.sort(key=lambda x: x["cvec"])
    entries_by_cvec = [list(v) for k,v in groupby(by_cvec, lambda x: x["cvec"])]

    by_sample = list(filter(lambda x: x["samples"] != 0, entries))
    by_sample.sort(key=lambda x: x["samples"])
    entries_by_sample = [list(v) for k,v in groupby(by_sample, lambda x: x["samples"])]

    for cvecs in entries_by_cvec:
        # SMT goes at the back
        cvecs.sort(key=lambda x: (float("inf") if x["smt"] else int(x["fuzz"])))

    for samples in entries_by_sample:
        # SMT goes at the back
        samples.sort(key=lambda x: (float("inf") if x["smt"] else int(x["fuzz"])))

    for c in entries_by_cvec:
        print(c, sep="\n")

    # how many columns do we need? fuzz (includes smt), domain
    cols = fuzz_cols + 1
    cs = ("c|" * (cols - 1)) + "c" #to avoid lines on the sides

    table_tex = ""
    table_tex += "\\begin{center} \n \\begin{tabular}{" + cs + "}\n"
    table_tex += "\\hline\n"

    # add headers
    fuzzes = list(set([x["fuzz"] for x in entries]))
    fuzzes.sort(key=lambda x: x)
    final_cols = []
    for f in fuzzes:
        if (f != "smt"):
            final_cols.append("fuzz " + str(f))
    final_cols.append("smt")

    headers = ["cvec"] + final_cols
    
    table_tex += " & ".join([str(x) for x in headers])
    table_tex += "\n \\\\ \\hline\n"

    # we need to make each cell data. it should all be in a row now
    for loe in entries_by_cvec:
        rows = ["status", "time", "rules", "unsound", "unknown"]
        table_tex += "\\multirow{" + str(len(rows)) + "}{*}{" + "cross: " + str(loe[0]["cvec"]) + "}"
        # guaranteed to exist I think
        for row in rows: 
            for entry in loe:
                table_tex += " & "
                table_tex += row + " : " + str(entry[row])
            table_tex += " \\\\"
        table_tex += " \\hline"

    table_tex += "\\hline"

    # make the sampled data separate
    for loe in entries_by_sample:
        rows = ["status", "time", "rules", "unsound", "unknown"]
        table_tex += "\\multirow{" + str(len(rows)) + "}{*}{" + "rand: " + str(loe[0]["samples"]) + "}"
        # guaranteed to exist I think
        for row in rows: 
            for entry in loe:
                table_tex += " & "
                table_tex += row + " : " + str(entry[row])
            table_tex += " \\\\"
        table_tex += " \\hline"

    table_tex += "\n\\hline \n \\end{tabular} \n \\end{center}"
    print(table_tex)   

if __name__ == "__main__":
    to_table(sys.argv[1])

# \begin{center}
# \begin{tabular}{ |c|c|c| } 
#  \hline
#  cell1 & cell2 & cell3 \\ 
#  cell4 & cell5 & cell6 \\ 
#  cell7 & cell8 & cell9 \\ 
#  \hline
# \end{tabular}
# \end{center}