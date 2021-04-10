import json
from itertools import groupby

def to_table(filename): 
    with open(filename) as f:
        data = json.load(f)

        # get the items we need
        # and put into a list of dicts
        entries = []

# TODO perhaps this processing is unnecessary
        for entry in data:
            info = {}
            info["status"] = entry['status']
            info["domain"] = entry['domain']
            info["time"] = entry["time"]
            info["num_rules"] = entry["num_rules"]
            info["unsound"] = "unsound: " + str(entry["post_unsound"])
            info["cvec"] = entry["params"]["important_cvec_offsets"]
            info["fuzz"] = entry["params"]["num_fuzz"]
            info["smt"] = entry["params"]["use_smt"]
            entries.append(info)

        # now we must begin to construct something of a table
        # the columns of the table are the fuzz/smt, so we should
        # collect/group
        # should I group by...?
        num_fuzz = len(set([x["fuzz"] for x in entries]))

        entries.sort(key=lambda x: x["cvec"])
        entries_by_cvec = [list(v) for k,v in groupby(entries, lambda x: x["cvec"])]

        # print(entries)
        for cvecs in entries_by_cvec:
            # SMT goes at the back
            cvecs.sort(key=lambda x: (float("inf") if x["smt"] else x["fuzz"]))
        
        print(entries_by_cvec)
        print(num_fuzz)


        # how many columns do we need? fuzz + smt + domain (fuzz includes smt)
        cols = num_fuzz + 1
        cs = "|" + ("c|" * cols)

        table_tex = ""
        table_tex += "\\begin{center}\\begin{tabular}{" + cs + "}\n"
        table_tex += "\\hline\n"

        # add headers
        headers = ["cvec"] + (list(set([x["fuzz"] for x in entries])))
        table_tex += " & ".join([str(x) for x in headers])
        table_tex += "\n \\\\ \\hline\n"

        # we need to make each cell data. it should all be in a row now
        for loe in entries_by_cvec:
        
            rows = ["status", "time", "unsound"]
            table_tex += "\\multirow{" + str(len(rows)) + "}*{" + str(loe[0]["cvec"]) + "}"
            # guaranteed to exist I think

            for row in rows: 
                for entry in loe:
                    table_tex += " & "
                    table_tex += str(entry[row])
                table_tex += " \\\\"
        table_tex += "\n\\hline \\end{tabular}\\end{center}"
        print(table_tex)

# \multirow{3}*{1} & success & success & success \\
# & 0.07563307 & 0.067024701 & 0.718618355 \\
# & unsound: 1 & unsound: 0 & unsound: 0 \\


to_table("all.json")

# \begin{center}
# \begin{tabular}{ |c|c|c| } 
#  \hline
#  cell1 & cell2 & cell3 \\ 
#  cell4 & cell5 & cell6 \\ 
#  cell7 & cell8 & cell9 \\ 
#  \hline
# \end{tabular}
# \end{center}