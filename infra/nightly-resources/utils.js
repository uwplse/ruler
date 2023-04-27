function getDerivability(o) {
  if (!o) {
    return "-";
  }
  let total = o.can.length + o.cannot.length;
  return toPercentage(o.can.length, total, 1);
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
  if (n === d) {
    return "100%";
  }
  return (
    (tryRound(n / d, decimals + 2 || 2) * 100)
      .toFixed(decimals || 0)
      .toString() + "%"
  );
}

function formatRules(rules) {
  let bidir = [];
  if (!rules || rules.length == 0) {
    return "-";
  }
  rules.forEach((rule, i) => {
    let [left, right] = rule.split(" ==> ");
    if (rules.includes(`${right} ==> ${left}`)) {
      bidir.push(`${left} <=> ${right}`);
      rules.splice(i, 1);
    } else {
      bidir.push(`${left} ==> ${right}`);
    }
  });
  return bidir.join("<br />");
}

function missingDerivability(o) {
  return Object.keys(o).length === 0;
}
