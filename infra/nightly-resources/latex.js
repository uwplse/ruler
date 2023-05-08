function onGenerateClick(tableName) {
  let lines = [];
  if (tableName === "bv") {
    lines = generateBvLatex();
  } else if (tableName === "ff") {
    lines = generateFFLatex();
  } else {
    lines = generateBaselineLatex(tableName);
  }
  let elem = document.getElementById("latex");
  elem.innerHTML = lines.join("\n");
  elem.style.height = "200px";
}

function generateFFLatex() {
  let rows = getFFData();
  let lines = [
    String.raw`\begin{table}[h]`,
    String.raw`  \footnotesize`,
    String.raw`\begin{tabular}{lllcc}`,
    String.raw`  Phase 1 & Phase 2 & Phase 3 & Time (s) & \# Rules with Trig Operators \\ \cline{1-5}`,
  ];
  rows.forEach((row) => {
    let line = Object.values(row).join(" & ");
    line = line
      .replaceAll("R", String.raw`$\mathcal{R}$`)
      .replaceAll("E", String.raw`$\mathcal{E}$`)
      .replaceAll("A", String.raw`$\mathcal{A}$`);
    lines.push(String.raw`${line} \\`);
  });
  lines.push(String.raw`\end{tabular}%`);
  lines = lines.concat(getCaption("ff"));
  lines.push(String.raw`\label{table:ff-cmp}`);
  lines.push(String.raw`\end{table}`);
  return lines;
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
    lines.push(String.raw`${latexRow.join(" & ").replaceAll("%", "\\%")} \\`);
  });

  lines.push(String.raw`\end{tabular}%`);
  lines = lines.concat(getCaption("bv"));
  lines.push(String.raw`}%`);
  lines.push(String.raw`\label{table:bv}`);
  lines.push(String.raw`\end{table}`);

  return lines;
}

function generateBaselineLatex(tableName) {
  let macroNames = {
    oopsla: String.raw`\ruler`,
    herbie: String.raw`\herbie`,
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
    bv4: "(0.96)",
    bv32: "(13.1)",
    rational: "(97.9)",
  };

  let allRows = data.filter((o) => o.baseline_name === tableName);
  if (tableName === "oopsla") {
    rows = [
      allRows.find((r) => r.spec_name === "bool"),
      allRows.find((r) => r.spec_name === "bv4_base"),
      allRows.find((r) => r.spec_name === "bv32"),
      allRows.find((r) => r.spec_name === "rational_replicate"),
    ];
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
    String.raw`\begin{table}[h]`,
    String.raw`\resizebox{\textwidth}{!}{%`,
    String.raw`\begin{tabular}{lccccc}`,
    String.raw`Domain   & \enumo LOC  & \# \enumo (Time)   & \# ${
      macroNames[tableName]
    } ${tableName === "ooplsa" ? "(Time)" : ""}  & \enumo $\rightarrow$ ${
      macroNames[tableName]
    } (Time) & ${
      macroNames[tableName]
    } $\rightarrow$ \enumo (Time)  \\ \cline{1-6}`,
  ];
  rows.forEach((row) => {
    let latexRow = [];
    Object.entries(keys).forEach(([key, f]) => {
      latexRow.push(f(row));
    });
    lines.push(String.raw`${latexRow.join(" & ").replaceAll("%", "\\%")} \\`);
  });

  lines.push(String.raw`\end{tabular}%`);
  lines.push(String.raw`}`);
  lines = lines.concat(getCaption(tableName));
  lines.push(String.raw`\label{table:${tableName}}`);
  lines.push(String.raw`\end{table}`);

  return lines;
}

function getCaption(version) {
  if (version === "oopsla") {
    return String.raw`\caption{Results comparing \slide to \ruler.
  $ R_1 ~ \rightarrow ~ R_2$ indicates using $R_1$ to derive
  $R_2$ rules.
  We report on both \lhs and \lhsandrhs derivability, separated by commas.
  The numbers in parentheses are times in seconds.
%  The three numbers correspond to using the three
%  derivability metrics (\T{lhs-only}, \T{lhs-rhs}, \T{all})
%  defined in \autoref{subsec:derivability}.
%  \todo{update with final results. also add ruler times, if not, take
%  out enumo times. Add back both lhs and lhsrhs.}
}`;
  } else if (version === "herbie") {
    return String.raw`  \caption{
  Derivability comparison between rules from \enumo and \herbie.
  As in \autoref{table:oopsla},
    $R_1 ~ \rightarrow ~ R_2$ indicates using
    $R_1$ to derive $R_2$ rules.
  We report both \lhs and \lhsandrhs derivability,
    separated by commas. The numbers in parentheses
    are times in second.
  ${"``"}-'' indicates that the derivability test
    could not be completed due to \herbie's
    unsound rules (\autoref{para:herbie}).
  We integrate these rules for
    end-to-end runs of \herbie~\cite{herbie} and
    Megalibm~\cite{megalibm} (\autoref{subsubsec:numbers}).}`;
  } else if (version === "bv") {
    return String.raw`\caption{
  Comparison of rule synthesis for different widths of bitvectors.
  Shown for each bitvector width are
    (i) the number of rules generated from an
    \enumo program (time in seconds) for that domain,
    (ii) the number of \enumo-synthesized BV4 rules
    that are valid in that domain (time in seconds), and
    (iii) the percentage of the generated rules
    that are derivable from the validated BV4 rules
    (both \lhs and \lhsandrhs derivability).`;
  } else if (version === "ff") {
    return String.raw`\caption{Comparing \C{compress} and \C{eqsat} with different
    subsets of $\mathcal{R}$ for the three phases of \autoref{fig:ff-actual}.
    $\mathcal{A}$ is the allowed rules of $\mathcal{R}$ and
    $\mathcal{E}$ is the exploratory rules of $\mathcal{R}$.
    The last row corresponds to \autoref{fig:ff-actual}, showing that
    it produces a good ruleset fastest.}`;
  }
}
