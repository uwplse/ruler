function getBaseline(name) {
  let keys = {
    Baseline: (row) => row.baseline_name,
    "Enumo Spec": (row) => row.spec_name,
    "Enumo LOC": (row) => row.loc,
    "# Enumo": (row) => row.rules.length,
    "Time (s)": (row) => row.time,
    "Enumo Derives Baseline (LHS / LHSRHS)": (row) =>
      getDerivability(row.derivability.enumo_derives_baseline),
    "Enumo derives Baseline Time (s)": (row) =>
      `${tryRound(
        row.derivability.enumo_derives_baseline.lhs.time,
        3
      )} / ${tryRound(
        row.derivability.enumo_derives_baseline.lhs_rhs.time,
        3
      )}`,
    "Baseline Derives Enumo (LHS / LHSRHS)": (row) =>
      getDerivability(row.derivability.baseline_derives_enumo),
    "Baseline derives Enumo Time (s)": (row) =>
      `${tryRound(
        row.derivability.baseline_derives_enumo.lhs.time,
        3
      )} / ${tryRound(
        row.derivability.baseline_derives_enumo.lhs_rhs.time,
        3
      )}`,
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

function getDerivability(o) {
  let total = o.lhs.can.length + o.lhs.cannot.length;
  return `${toPercentage(o.lhs.can.length, total, 1)} / ${toPercentage(
    o.lhs_rhs.can.length,
    total,
    1
  )}`;
}

function load() {
  document.getElementById("baseline_table").innerHTML = ConvertJsonToTable(
    getBaseline("oopsla")
  );
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

function toPercentage(n, d, decimals) {
  return (
    (tryRound(n / d, decimals + 2 || 2) * 100)
      .toFixed(decimals || 0)
      .toString() + "%"
  );
}
