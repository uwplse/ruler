const tableFormatter = require("../tableFormatter.js");
const { tryRound, getDerivability, formatRules, reformat } = tableFormatter;
const fs = require("fs");

function generateBaselineLatex(tableName) {
  const data = JSON.parse(fs.readFileSync("out/output.json"));
  let macroNames = {
    oopsla: String.raw`Ruler`,
    herbie: String.raw`Herbie`,
  };

  let specNames = {
    bv4_base: "bv4",
    rational_replicate: "rational",
    exponential: "Exponential",
    rational_best: "Rational",
    trig: "Trig",
  };

  let rulerTimes = {
    bool: "(0.05)",
    bv4_base: "(0.96)",
    bv32: "(13.1)",
    rational_replicate: "(97.9)",
  };

  let allRows = data.filter((o) => o.baseline_name === tableName);
  if (tableName === "oopsla") {
    rows = [
      allRows.find((r) => r.spec_name === "bool"),
      allRows.find((r) => r.spec_name === "bv4_base"),
      allRows.find((r) => r.spec_name === "bv32"),
      allRows.find((r) => r.spec_name === "rational_replicate"),
    ].filter((row) => !!row); // Guard for missing rows
  } else if (tableName === "herbie") {
    rows = allRows.filter((r) => r.spec_name != "rational_replicate");
  } else {
    rows = allRows;
  }
  let keys = {
    domain: (row) => specNames[row.spec_name] || row.spec_name,
    loc: (row) => row.loc,
    renumo_time: (row) => `${row.rules.length} (${tryRound(row.time)})`,
    baseline_time: (row) =>
      `${
        row.derivability.enumo_derives_baseline.lhs.can.length +
        row.derivability.enumo_derives_baseline.lhs.cannot.length
      } ${rulerTimes[row.spec_name] || ""}`,
    enumo_derives_baseline: (row) => {
      lhs_percentage = getDerivability(
        row.derivability.enumo_derives_baseline.lhs
      );
      lhs_time = tryRound(row.derivability.enumo_derives_baseline.lhs?.time);
      lhs_rhs_percentage = getDerivability(
        row.derivability.enumo_derives_baseline.lhs_rhs
      );
      lhs_rhs_time = tryRound(
        row.derivability.enumo_derives_baseline.lhs_rhs?.time
      );
      return `${lhs_percentage} (${lhs_time}), ${lhs_rhs_percentage} (${lhs_rhs_time})`;
    },
    baseline_derives_enumo: (row) => {
      lhs_percentage = getDerivability(
        row.derivability.baseline_derives_enumo.lhs
      );
      lhs_time = tryRound(row.derivability.baseline_derives_enumo.lhs?.time);
      lhs_rhs_percentage = getDerivability(
        row.derivability.baseline_derives_enumo.lhs_rhs
      );
      lhs_rhs_time = tryRound(
        row.derivability.baseline_derives_enumo.lhs_rhs?.time
      );
      return `${lhs_percentage} ${
        lhs_time ? `(${lhs_time})` : ""
      }, ${lhs_rhs_percentage} ${lhs_rhs_time ? `(${lhs_rhs_time})` : ""}`;
    },
  };
  let lines = [
    String.raw`\documentclass[letterpaper]{article}`,
    String.raw`\usepackage[margin=1in,footskip=0.25in]{geometry}`,
    String.raw`\begin{document}`,
    String.raw`\begin{table}[h]`,
    // Note: Different than nightlies!
    String.raw`\footnotesize`,
    // String.raw`\resizebox{\textwidth}{!}{%`,
    String.raw`\begin{tabular}{lccccc}`,
    String.raw`Domain   & Renumo LOC  & \# Renumo (Time)   & \# ${
      macroNames[tableName]
    } ${tableName === "oopsla" ? "(Time)" : ""}  & Renumo $\rightarrow$ ${
      macroNames[tableName]
    } (Time) & ${
      macroNames[tableName]
    } $\rightarrow$ Renumo (Time)  \\ \cline{1-6}`,
  ];
  rows.forEach((row) => {
    let latexRow = [];
    Object.entries(keys).forEach(([key, f]) => {
      latexRow.push(f(row));
    });
    // Note: replaceAll is only available in ES12+, so use `replace` with global regexp
    lines.push(String.raw`${latexRow.join(" & ").replace(/%/g, "\\%")} \\`);
  });

  lines.push(String.raw`\end{tabular}%`);
  // lines.push(String.raw`}`);
  lines = lines.concat(getCaption(tableName));
  lines.push(String.raw`\label{table:${tableName}}`);
  lines.push(String.raw`\end{table}`);
  lines.push(String.raw`\end{document}`);

  return lines;
}

function getCaption() {
  return String.raw`\caption{Results comparing Renumo to Ruler.
  $ R_1 ~ \rightarrow ~ R_2$ indicates using $R_1$ to derive
  $R_2$ rules.
  We report on both lhs and lhs-and-rhs derivability, separated by commas.
  The numbers in parentheses are times in seconds.}`;
}

const latexLines = generateBaselineLatex("oopsla");
fs.writeFileSync("out/table.tex", latexLines.join("\n"));
