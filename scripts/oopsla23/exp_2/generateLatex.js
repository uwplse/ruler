const tableFormatter = require("../tableFormatter.js");
const { tryRound, getDerivability, optionalChaining } = tableFormatter;
const fs = require("fs");

function generateLatex() {
  const data = JSON.parse(fs.readFileSync("out/output.json"));
  let rows = data.filter((o) => o.spec_name === "halide");
  let keys = {
    domain: (row) => row.spec_name,
    loc: (row) => row.loc,
    renumo_time: (row) => `${row.rules.length} (${tryRound(row.time)})`,
    baseline_time: (row) =>
      `${
        row.derivability.enumo_derives_baseline.lhs.can.length +
        row.derivability.enumo_derives_baseline.lhs.cannot.length
      }`,
    enumo_derives_baseline: (row) => {
      lhs_percentage = getDerivability(
        row.derivability.enumo_derives_baseline.lhs
      );
      lhs_time = tryRound(
        optionalChaining(row.derivability.enumo_derives_baseline.lhs, "time")
      );
      lhs_rhs_percentage = getDerivability(
        row.derivability.enumo_derives_baseline.lhs_rhs
      );
      lhs_rhs_time = tryRound(
        optionalChaining(
          row.derivability.enumo_derives_baseline.lhs_rhs,
          "time"
        )
      );
      return `${lhs_percentage} (${lhs_time}), ${lhs_rhs_percentage} (${lhs_rhs_time})`;
    },
    baseline_derives_enumo: (row) => {
      lhs_percentage = getDerivability(
        row.derivability.baseline_derives_enumo.lhs
      );
      lhs_time = tryRound(
        optionalChaining(row.derivability.baseline_derives_enumo.lhs, "time")
      );
      lhs_rhs_percentage = getDerivability(
        row.derivability.baseline_derives_enumo.lhs_rhs
      );
      lhs_rhs_time = tryRound(
        optionalChaining(
          row.derivability.baseline_derives_enumo.lhs_rhs,
          "time"
        )
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
    String.raw`Domain & Renumo LOC & \# Renumo (Time) & \# Halide & Renumo $\rightarrow$ Halide (Time) & Halide $\rightarrow$ Renumo (Time)  \\ \cline{1-6}`,
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
  // lines = lines.concat(getCaption(tableName));
  lines.push(String.raw`\label{table:halide}`);
  lines.push(String.raw`\end{table}`);
  lines.push(String.raw`\end{document}`);

  return lines;
}

const latexLines = generateLatex();
fs.writeFileSync("out/table.tex", latexLines.join("\n"));
