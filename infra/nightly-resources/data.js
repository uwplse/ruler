// Parses the intended domain out of the URL query param (?domain=<some domain>)
// Finds the corresponding JSON object in the global data array
function getDomainData() {
  let params = new URLSearchParams(window.location.search);
  let domain = Object.fromEntries(params).domain;

  return data.find((x) => x.spec_name == domain);
}

// Filters the global data array to just the rows corresponding to the bv4 experiment
// Maps each raw data object into an object corresponding to one row in the table
// Transformations include renaming columns, rounding numbers, and converting derivability
// to percentages
function getBvData() {
  let exps = data.filter((row) => !!row.from_bv4);

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
  let tableData = [];
  exps.forEach((row) => {
    let newRow = {};
    Object.entries(keys).forEach(([key, f]) => {
      newRow[key] = tryRound(f(row));
    });
    tableData.push(newRow);
  });
  return tableData;
}

// Filters the global data array to just the rows corresponding to the
// baseline experiments (oopsla, herbie, and halide)
// Maps each raw data object into an object corresponding to one row in the table
// Transformations include renaming columns, rounding numbers, and converting derivability
// to percentages
function getBaseline(name) {
  let keys = {
    Baseline: (row) => row.baseline_name,
    "Enumo Spec": (row) => row.spec_name,
    "Enumo LOC": (row) => row.loc,
    "# Enumo": (row) => row.rules.length,
    "Time (s)": (row) => row.time,
    "Enumo Derives Baseline (LHS / LHSRHS)": (row) =>
      `${getDerivability(
        row.derivability.enumo_derives_baseline.lhs
      )} / ${getDerivability(row.derivability.enumo_derives_baseline.lhs_rhs)}`,
    "Enumo derives Baseline Time (s)": (row) => {
      if (missingDerivability(row.derivability.enumo_derives_baseline)) {
        return "-";
      } else {
        return `${tryRound(
          row.derivability.enumo_derives_baseline.lhs.time,
          3
        )} / ${tryRound(
          row.derivability.enumo_derives_baseline.lhs_rhs.time,
          3
        )}`;
      }
    },
    "Baseline Derives Enumo (LHS / LHSRHS)": (row) =>
      `${getDerivability(
        row.derivability.baseline_derives_enumo.lhs
      )} / ${getDerivability(row.derivability.baseline_derives_enumo.lhs_rhs)}`,
    "Baseline derives Enumo Time (s)": (row) => {
      if (missingDerivability(row.derivability.baseline_derives_enumo)) {
        return "-";
      } else {
        return `${tryRound(
          row.derivability.baseline_derives_enumo.lhs.time,
          3
        )} / ${tryRound(
          row.derivability.baseline_derives_enumo.lhs_rhs.time,
          3
        )}`;
      }
    },
  };
  let tableData = [];
  data.forEach((row) => {
    if (!row["baseline_name"]?.includes(name)) {
      return;
    }
    let newRow = {};
    Object.entries(keys).forEach(([key, f]) => {
      newRow[key] = tryRound(f(row));
    });
    tableData.push(newRow);
  });
  return tableData;
}
