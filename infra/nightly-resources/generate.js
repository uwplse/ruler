function getBaseline(data, baseline) {
  let keys = {
    baseline_name: "Baseline",
    enumo_spec_name: "Enumo Spec",
    loc: "Enumo Spec LOC",
    num_rules: "# Enumo",
    num_baseline: "# Baseline",
    time: "Time (s)",
    enumo_to_baseline_lhs_num: "E derives B (LHS)",
    enumo_to_baseline_lhsrhs_num: "E derives B (LHSRHS)",
    enumo_to_baseline_all_num: "E derives B (ALL)",

    enumo_to_baseline_lhs_time: "E derives B (LHS) Time (s)",
    enumo_to_baseline_lhsrhs_time: "E derives B (LHSRHS) Time (s)",
    enumo_to_baseline_all_time: "E derives B (ALL) Time (s)",

    baseline_to_enumo_lhs_num: "B derives E (LHS)",
    baseline_to_enumo_lhsrhs_num: "B derives E (LHSRHS)",
    baseline_to_enumo_all_num: "B derives E (ALL)",

    baseline_to_enumo_lhs_time: "B derives E (LHS) Time (s)",
    baseline_to_enumo_lhsrhs_time: "B derives E (LHSRHS) Time (s)",
    baseline_to_enumo_all_time: "B derives E (ALL) Time (s)",

    "minimization strategy": "Minimization",
  };
  let newData = [];
  data.forEach((row) => {
    if (!row["baseline_name"]?.includes(baseline)) {
      return;
    }
    let newRow = {};
    Object.keys(keys).forEach((key) => {
      newRow[keys[key]] = tryRound(row[key]);
    });

    convertDerivabilityToPercentages(newRow);

    consolidateColumns(newRow, "Enumo derives Baseline (LHS, LHS/RHS, All)", [
      "E derives B (LHS)",
      "E derives B (LHSRHS)",
      "E derives B (ALL)",
    ]);
    consolidateColumns(newRow, "Enumo derives Baseline Time (s)", [
      "E derives B (LHS) Time (s)",
      "E derives B (LHSRHS) Time (s)",
      "E derives B (ALL) Time (s)",
    ]);
    consolidateColumns(newRow, "Baseline derives Enumo (LHS, LHS/RHS, All)", [
      "B derives E (LHS)",
      "B derives E (LHSRHS)",
      "B derives E (ALL)",
    ]);
    consolidateColumns(newRow, "Baseline derives Enumo Time (s)", [
      "B derives E (LHS) Time (s)",
      "B derives E (LHSRHS) Time (s)",
      "B derives E (ALL) Time (s)",
    ]);
    newData.push(newRow);
  });
  return newData;
}

function convertDerivabilityToPercentages(row) {
  let derive_types = ["LHS", "LHSRHS", "ALL"];
  derive_types.forEach((dt) => {
    row[`E derives B (${dt})`] = toPercentage(
      row[`E derives B (${dt})`],
      row["# Baseline"],
      1
    );
    row[`B derives E (${dt})`] = toPercentage(
      row[`B derives E (${dt})`],
      row["# Enumo"],
      1
    );
  });
}

function load() {
  document.getElementById("baseline_table").innerHTML = ConvertJsonToTable(
    getBaseline(data, "oopsla")
  );

  document.getElementById("herbie_table").innerHTML = ConvertJsonToTable(
    getBaseline(data, "herbie")
  );

  document.getElementById("halide_table").innerHTML = ConvertJsonToTable(
    getBaseline(data, "halide")
  );

  document.getElementById("detail").innerHTML = populateDomainDetail();
}

function populateDomainDetail() {
  let domains = data.map((x) => x.enumo_spec_name).filter((x) => !!x);
  let str = "";
  domains.forEach((domain) => {
    str += "<p>";
    str += `${domain}: `;
    str += `<a href="rules.html?domain=${domain}">All Rules</a> `;
    str += `<a href="derive_detail.html?domain=${domain}">Derivability</a>`;
    str += "</p>";
  });
  return str;
}

function getDomainData() {
  let params = new URLSearchParams(window.location.search);
  let domain = Object.fromEntries(params).domain;

  if (!domain) {
    return;
  }

  let domainData = data.find((x) => x.enumo_spec_name == domain);
  if (!domainData) {
    return;
  }
  return domainData;
}

function loadRules() {
  let domainData = getDomainData();
  document.getElementById("domain_name").innerHTML = domainData.enumo_spec_name;
  let rules = domainData.rules.rules;
  let str = "";
  rules.forEach((rule) => {
    str += `${rule} <br />`;
  });
  document.getElementById("all_rules").innerHTML = str;
}

function loadDeriveDetail() {
  let domainData = getDomainData();
  document.getElementById("domain_name").innerHTML = domainData.enumo_spec_name;

  let deriveTypes = ["lhs", "lhs_rhs", "all"];
  deriveTypes.forEach((deriveType) => {
    document.getElementById(`${deriveType}_etob_can`).innerHTML =
      ConvertJsonToTable(
        domainData[`enumo_to_baseline_${deriveType}`]
          .enumo_derives_baseline_derivable,
        "Using Enumo to Derive Baseline (derivable)"
      );
    document.getElementById(`${deriveType}_etob_cannot`).innerHTML =
      ConvertJsonToTable(
        domainData[`enumo_to_baseline_${deriveType}`]
          .enumo_derives_baseline_underivable,
        "Using Enumo to Derive Baseline (not derivable)"
      );

    document.getElementById(`${deriveType}_btoe_can`).innerHTML =
      ConvertJsonToTable(
        domainData[`enumo_to_baseline_${deriveType}`]
          .baseline_derives_enumo_derivable,
        "Using Baseline to Derive Enumo (derivable)"
      );
    document.getElementById(`${deriveType}_btoe_cannot`).innerHTML =
      ConvertJsonToTable(
        domainData[`enumo_to_baseline_${deriveType}`]
          .baseline_derives_enumo_underivable,
        "Using Baseline to Derive Enumo (not derivable)"
      );
  });
}

function tryRound(v, precision) {
  if (typeof v == "number") {
    if (v % 1 == 0) {
      return v;
    } else {
      return v.toFixed(precision || 2);
    }
  } else {
    return v;
  }
}

function getFormattedHeader(baseline, columnNames) {
  let header = "";
  let baseline_names = {
    oopsla: String.raw`\ruler`,
    herbie: String.raw`\herbie`,
    halide: String.raw`\halide`,
  };
  if (baseline === "bv") {
    header = String.raw`Domain & \# Generated Rules & Generation Time (s) & \# Sound BV4 Rules & Validation Time (s) & Validate $\rightarrow$ Generated`;
  } else {
    let baseline_name = baseline_names[baseline];
    if (baseline_name) {
      header = String.raw`Domain & \enumo LOC & \# \enumo & \# ${baseline_names[baseline]} & \enumo $\rightarrow$ ${baseline_names[baseline]} & ${baseline_names[baseline]} $\rightarrow$ \enumo`;
    }
  }
  return String.raw`${header} \\ \cline{1-${header.split("&").length}}`;
}

function getCaption(baseline) {
  if (baseline === "oopsla") {
    return [
      String.raw`\caption{Results comparing \slide to \ruler.`,
      String.raw`  $ R_1 ~ \rightarrow ~ R_2$ indicates using $R_1$ to derive`,
      String.raw`  $R_2$ rules.`,
      String.raw`  The three numbers correspond to using the three`,
      String.raw`  derivability metrics (\T{lhs-only}, \T{lhs-rhs}, \T{all})`,
      String.raw`  defined in \autoref{subsec:derivability}.`,
      String.raw`  \todo{update with final results. also add ruler times, if not, take`,
      String.raw`  out enumo times}}`,
    ];
  }
}

function onGenerateClick(version) {
  var ignoreColumns = [];
  if (version === "bv") {
    let tableData = loadBvExp();
    console.log(tableData);
    generateLatex("bv", tableData, ["missing"]);
  } else {
    let tableData = getBaseline(data, version);

    ignoreColumns = [
      "Baseline",
      "Minimization",
      "Time (s)",
      "Baseline derives Enumo Time (s)",
      "Enumo derives Baseline Time (s)",
    ];

    generateLatex(version, tableData, ignoreColumns);
  }
}

function generateLatex(version, tableData, ignoreColumns) {
  document.getElementById("latex").innerHTML =
    "This feature is not currently supported.";
  // let columnNames = Object.keys(tableData[0]);
  // let lines = [
  //   String.raw`\begin{table}`,
  //   String.raw`\resizebox{\textwidth}{!}{%`,
  //   String.raw`\begin{tabular}{` +
  //     "l".repeat(columnNames.length - ignoreColumns.length) +
  //     "}",
  // ];

  // lines.push(getFormattedHeader(version, columnNames));

  // tableData.forEach((row) => {
  //   let line =
  //     Object.keys(row)
  //       .filter((key) => !ignoreColumns.includes(key))
  //       .map((key) => row[key])
  //       .join(" & ") + " \\\\";
  //   lines.push(line.replaceAll("%", "\\%"));
  // });

  // lines.push(String.raw`\end{tabular}%`);
  // lines.push(String.raw`}`);
  // lines = lines.concat(getCaption(version));
  // lines.push(String.raw`\label{table:${version}}`);
  // lines.push(String.raw`\end{table}`);

  // let s = lines.join("\n");
  // let elem = document.getElementById("latex");
  // elem.innerHTML = s;
  // elem.style.height = "200px";
}

function consolidateColumns(dataObj, consolidatedName, columnNames) {
  let values = [];
  columnNames.forEach((col) => {
    values.push(dataObj[col]);
    delete dataObj[col];
  });
  dataObj[consolidatedName] = values.join(", ");
}

function toPercentage(n, d, decimals) {
  return (
    (tryRound(n / d, decimals + 2 || 2) * 100)
      .toFixed(decimals || 0)
      .toString() + "%"
  );
}

function loadBvExp() {
  let exps = data.filter((x) => !!x.from_bv4);

  var columns = ["domain", "generated", "from bv4", "% derivable"];
  let rows = [];
  exps.forEach((exp) => {
    rows.push({
      domain: exp.domain,
      generated: exp.direct_gen.rules.length,
      "gen time (s)": tryRound(exp.direct_gen.time),
      from_bv4: exp.from_bv4.rules.length,
      "from_bv4 time (s)": tryRound(exp.from_bv4.time),
      LHS: toPercentage(
        exp.lhs_derive.can,
        exp.lhs_derive.can + exp.lhs_derive.cannot
      ),
      "LHS Time": tryRound(exp.lhs_derive.time),
      "LHS missing": formatRules(exp.lhs_derive.missing_rules),
      "LHS-RHS": toPercentage(
        exp.lhsrhs_derive.can,
        exp.lhsrhs_derive.can + exp.lhsrhs_derive.cannot
      ),
      "LHS-RHS Time": tryRound(exp.lhsrhs_derive.time),
      "LHS-RHS missing": formatRules(exp.lhsrhs_derive.missing_rules),
    });
  });

  // sort by increasing BV width
  rows.sort((a, b) => a.domain.substr(2) - b.domain.substr(2));
  return rows;
}

function onLoadBv() {
  let rows = loadBvExp();
  document.getElementById("container").innerHTML = ConvertJsonToTable(rows);
}

function formatRules(rules) {
  let bidir = [];
  rules.forEach((rule, i) => {
    let [left, right] = rule.split(" ==> ");
    if (rules.includes(`${right} ==> ${left}`)) {
      bidir.push(`${left} <=> ${right}`);
      rules.splice(i, 1);
    }
  });
  return bidir.join("<br/>");
}
