function getBaseline(data, baseline) {
  let keys = {
    baseline_name: "Baseline",
    enumo_spec_name: "Enumo Spec",
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
    if (!row["baseline_name"].includes(baseline)) {
      return;
    }
    let newRow = {};
    Object.keys(keys).forEach((key) => {
      newRow[keys[key]] = tryRound(row[key]);
    });
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
}

function loadDeriveDetail() {
  let params = new URLSearchParams(window.location.search);
  let domain = Object.fromEntries(params).domain;

  if (!domain) {
    return;
  }
  document.getElementById("domain_name").innerHTML = domain;

  let domainData = data.find((x) => x.enumo_spec_name == domain);
  if (!domainData) {
    return;
  }
  let deriveTypes = ["lhs", "lhs_rhs", "all"];
  deriveTypes.forEach((deriveType) => {
    console.log(deriveType);
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
