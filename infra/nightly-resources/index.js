function load() {
  document.getElementById("baseline_table").innerHTML = ConvertJsonToTable(
    getBaseline("oopsla")
  );

  document.getElementById("herbie_table").innerHTML = ConvertJsonToTable(
    getBaseline("herbie")
  );

  document.getElementById("halide_table").innerHTML = ConvertJsonToTable(
    getBaseline("halide")
  );

  let domains = data.map((row) => row.spec_name).filter((x) => !!x);
  let domainList = "";
  domains.forEach((domain) => {
    domainList += `
      <p>
      ${domain}: 
      <a href="rules.html?domain=${domain}">All Rules</a>
      <a href="derive_detail.html?domain=${domain}">Derivability</a>
      </p>
    `;
  });
  document.getElementById("detail").innerHTML = domainList;
}

function onLoadBv() {
  document.getElementById("container").innerHTML = ConvertJsonToTable(
    getBvData()
  );
}

function loadRules() {
  let domainData = getDomainData();
  document.getElementById("domain_name").innerHTML = domainData.spec_name;
  let rules = domainData.rules;
  document.getElementById("all_rules").innerHTML = formatRules(rules);
}

function loadDeriveDetail() {
  let domainData = getDomainData();
  document.getElementById("domain_name").innerHTML = domainData.spec_name;
  let deriveTypes = ["lhs", "lhs_rhs"];
  deriveTypes.forEach((deriveType) => {
    let derivability = domainData.derivability;

    let tableData = [
      {
        "Enumo->Baseline (derivable)": formatRules(
          derivability.enumo_derives_baseline[deriveType]?.can
        ),
        "Enumo->Baseline (not derivable)": formatRules(
          derivability.enumo_derives_baseline[deriveType]?.cannot
        ),
        "Baseline->Enumo (derivable)": formatRules(
          derivability.baseline_derives_enumo[deriveType]?.can
        ),
        "Baseline->Enumo (not derivable)": formatRules(
          derivability.baseline_derives_enumo[deriveType]?.cannot
        ),
      },
    ];
    document.getElementById(`${deriveType}_table`).innerHTML =
      ConvertJsonToTable(tableData);
  });
}
