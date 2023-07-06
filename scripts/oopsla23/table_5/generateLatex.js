const tableFormatter = require("../tableFormatter.js");
const { tryRound, getDerivability, formatRules, reformat } = tableFormatter;
const fs = require("fs");

// Maps each raw data object into an object corresponding to one row in the table
// Transformations include renaming columns, rounding numbers, and converting derivability
// to percentages
function getBvData() {
  const data = JSON.parse(fs.readFileSync("out/output.json"));
  let exps = data.filter((row) => row.TYPE === "bv");

  // Each key in this map corresponds to a column of the table
  // The value is a function from the row data object to the correct value for that cell
  let keys = {
    Domain: (row) => row.domain,
    Generated: (row) => row.direct_gen.rules.length,
    "Gen Time (s)": (row) => tryRound(row.direct_gen.time),
    "From BV4": (row) => row.from_bv4.rules.length,
    "From BV4 Time (s)": (row) => tryRound(row.from_bv4.time),
    LHS: (row) => getDerivability(row.derivability.lhs),
    "LHS Time": (row) => tryRound(row.derivability.lhs.time, 3),
    "LHS Missing": (row) => formatRules(row.derivability.lhs.cannot),
    "LHS-RHS": (row) => getDerivability(row.derivability.lhs_rhs),
    "LHS-RHS Time": (row) => tryRound(row.derivability.lhs_rhs.time, 3),
    "LHS-RHS Missing": (row) => formatRules(row.derivability.lhs_rhs.cannot),
  };
  return reformat(keys, exps);
}

function generateBvLatex() {
  let rows = getBvData();
  rows.sort((a, b) => a.Domain.substr(2) - b.Domain.substr(2));

  let keys = {
    domain: (row) => row.Domain,
    gen_rules_time: (row) => `${row.Generated} (${row["Gen Time (s)"]})`,
    valid_rules_time: (row) =>
      `${row["From BV4"]} (${row["From BV4 Time (s)"]})`,
    valid_generated: (row) => `${row.LHS}, ${row["LHS-RHS"]}`,
  };

  let lines = [
    String.raw`\documentclass{article}`,
    String.raw`\begin{document}`,
    String.raw`\begin{table}[h]`,
    String.raw`\footnotesize`,
    String.raw`\begin{tabular}{lccc}`,
    String.raw`Domain & Generated Rules (Time) & Valid BV4 Rules (Time) & Validated $\rightarrow$ Generated \\ \cline{1-4}`,
  ];
  rows.forEach((row) => {
    let latexRow = [];
    Object.entries(keys).forEach(([key, f]) => {
      latexRow.push(f(row));
    });
    lines.push(String.raw`${latexRow.join(" & ").replace(/%/g, "\\%")} \\`);
  });

  lines.push(String.raw`\end{tabular}%`);
  lines = lines.concat(getCaption());
  lines.push(String.raw`}%`);
  lines.push(String.raw`\label{table:bv}`);
  lines.push(String.raw`\end{table}`);
  lines.push(String.raw`\end{document}`);

  return lines;
}

function getCaption() {
  return String.raw`\caption{
  Comparison of rule synthesis for different widths of bitvectors.
  Shown for each bitvector width are
    (i) the number of rules generated from an
    Renumo program (time in seconds) for that domain,
    (ii) the number of Renumo-synthesized BV4 rules
    that are valid in that domain (time in seconds), and
    (iii) the percentage of the generated rules
    that are derivable from the validated BV4 rules
    (both lhs and lhs-and-rhs derivability).`;
}

const latexLines = generateBvLatex();
fs.writeFileSync("out/table.tex", latexLines.join("\n"));
