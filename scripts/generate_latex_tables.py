#!/usr/bin/python3
import sys
import json

def get_headers_for_baseline(domain):
  return [
    ("domain", "Domain"),
    ("# rules found", "SlideRule Rules"),
    ("rulefinding time (sec)", "Time (s)"),
    ("# {} rules".format(domain), "Baseline Rules"),
    ("enumo -> {} (lhs, lhs & rhs, all)".format(domain), "SlideRule derives Baseline"),
    ("enumo -> {} time".format(domain), "Derive Time (s)"),
    ("{} -> enumo (lhs, lhs & rhs, all)".format(domain), "Baseline derives SlideRule"),
    ("{} -> enumo time".format(domain), "Derive Time (s)"),
    ("minimization strategy", "Minimization")
  ]


def generate_tables(filename):
  with open(filename) as f:
    o = json.load(f)
  
  tables = []
  for (baseline, data) in o.items():
    tables.append(generate_table(data, baseline))
  return tables

def generate_table(data, baseline):
  lines = [
    r'\begin{table}[]',
    r'\resizebox{\textwidth}{!}{%',
    r'\begin{tabular}{lllllllll}'
  ]

  columns = get_headers_for_baseline(baseline)

  lines.append(parse_header(columns))

  for row in data:
    lines.append(parse_line(row, columns))

  lines.append(r'\end{tabular}%')
  lines.append(r'}')
  lines.append(r'\end{table}')

  return lines

def parse_header(columns):
  content = " & ".join([i[1] for i in columns])
  content += " \\\\ \cline{1-9}"
  return content

def try_round(val):
  try:
    return "{:0.2f}".format(float(val))
  except:
    return val

def parse_line(row, columns):
  cells = []
  for (raw, display) in columns:
    if raw in row:
      cells.append(row[raw])
    else:
      cells.append("-")
  for (i, cell) in enumerate(cells):
    if type(cell) == float:
      cells[i] = "{:0.2f}".format(cell)
    elif type(cell) == int:
      cells[i] = str(cell)
    elif "," in cell:
      parts = cell.split(",")
      for (j, part) in enumerate(parts):
        parts[j] = try_round(part)
      cells[i] = ",".join(parts)
  
  content = " & ".join(cells)
  content += "\\\\" 
  return content

def write_tables(tables, output_filename):
  with open(output_filename, "w") as f:
    for table in tables:
      for line in table:
        f.write(line + "\n")

      f.write("\n\n")

if __name__ == "__main__":
  input_filename = sys.argv[1]
  output_filename = sys.argv[2]
  tables = generate_tables(input_filename)
  write_tables(tables, output_filename)