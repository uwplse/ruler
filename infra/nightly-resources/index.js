// Top-level load function for the main index page.
// Populates the oopsla, herbie, and halide tables with data from the
// global data object
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

// This function is called when you load bv.html.
// Populates the bv table with data from the global data object.
function onLoadBv() {
  document.getElementById("container").innerHTML = ConvertJsonToTable(
    getBvData()
  );
}

// This function is called when you load ff.html.
// Populates the ff table with data from the global data object.
function onLoadFF() {
  document.getElementById("container").innerHTML = ConvertJsonToTable(
    getFFData()
  );
}

// This function is called when you load rules.html.
// Shows all rules for the domain specified in the query param
// If there is no domain specified, it will just be a blank page.
function loadRules() {
  let domainData = getDomainData();
  document.getElementById("domain_name").innerHTML = domainData.spec_name;
  let rules = domainData.rules;
  document.getElementById("all_rules").innerHTML = formatRules(rules);
}

// This function is called when you load derive_detail.html.
// Shows all rules for the domain specified in the query param
// If there is no domain specified, it will just be a blank page.
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
