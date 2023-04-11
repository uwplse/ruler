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

function tryRound(v) {
  if (typeof v == "number") {
    if (v % 1 == 0) {
      return v;
    } else {
      return v.toFixed(2);
    }
  } else {
    return v;
  }
}

function generateLatex(baseline) {
  let escape = (s) => s.replaceAll("#", "\\#");

  let baselineData = getBaseline(data, baseline);

  let columnNames = Object.keys(baselineData[0]);

  var lines = [
    String.raw`\begin{table}[]`,
    String.raw`\resizebox{\textwidth}{!}{%`,
    String.raw`\begin{tabular}{` + "l".repeat(columnNames.length) + "}",
  ];

  lines.push(
    columnNames.join(" & ") + String.raw`\\ \cline{1-${columnNames.length}}`
  );

  baselineData.forEach((row) => {
    lines.push(Object.values(row).join(" & ") + " \\\\");
  });

  lines.push(String.raw`\end{tabular}%`);
  lines.push(String.raw`}`);
  lines.push(String.raw`\label{table:${baseline}}`);
  lines.push(String.raw`\end{table}`);

  let s = lines.map((l) => escape(l)).join("\n");
  let elem = document.getElementById("latex");
  elem.innerHTML = s;
  elem.style.height = "200px";
}

function consolidateColumns(dataObj, consolidatedName, columnNames) {
  let values = [];
  columnNames.forEach((col) => {
    values.push(dataObj[col]);
    delete dataObj[col];
  });
  dataObj[consolidatedName] = values.join(", ");
}

function toPercentage(v, b) {
  return (tryRound(v / b) * 100).toFixed(0).toString() + "%";
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
      derive: toPercentage(exp.derive.can, exp.derive.can + exp.derive.cannot),
      missing: exp.derive.missing_rules.join("<br/>"),
    });
  });
  document.getElementById("container").innerHTML = ConvertJsonToTable(rows);
}
