const fs = require('fs');
// load all the files
let data = [];
let dataByType = {};

let load_files = (base_folder, output_path) => {
    // check if slash, add if none

    let phase_time = "phase-times";
    let orat = "orat";
    let mrat = "mrat";
    let default_conf = "default";
    let no_run_rewrites = "no-run-rewrites";

    load_dir(base_folder + phase_time, "phase-times",
        () => load_dir(base_folder + default_conf, "default",
            () => load_dir(base_folder + orat, "orat",
                () => load_dir(base_folder + mrat, "mrat",
                    () => load_dir(base_folder + no_run_rewrites, "no-run-rewrites",
                        () => print_data(data))))));
}

// I should really just make promises... but whatever
let load_dir = (path, type, k) => {
    dataByType[type] = [];
    console.log(path);
    fs.readdir(path, 'utf8', (err, filenames) => {
        console.log(filenames);
        if (err != undefined || err != null) {
            console.log('lol');
        }

        // Use CPS so we know when all files are done 
        let process = (files, files_k) => {
            if (files.length == 0) {
                files_k();
                return;
            }
            let file = files[0];

            fs.readFile(path + '/' + file, 'utf8', (err, text) => {

                let entry = make_entry(text);
                entry.name = file;
                entry.type = type;
                parse_name(file, entry);
                data.push(entry);
                dataByType[type].push(entry);

                process(files.slice(1), files_k)
            })
        }

        process(filenames, k);
    })
}

let make_entry = (text) => {
    let total_time_pattern = /Learned (?<quantity>[\d]+)[\w\s]*(?<time>[.\d]+)$/gm;
    let egraph_size_pattern = /egraph n=([\d]+), e=([\d]+)/gm;
    let phase_time_pattern = /Time taken in... [\w.\s]+: ([\d.]+), [\w.\s]+: ([\d.]+), [\w.\s]+: ([\d.]+)$/gm
    let validation_pattern = /Time taken in validation+: ([\d.]+)$/gm // may not exist 

    // N.B. because of how this works, we need to subtract validation time from the minimization time.
    
    let item = total_time_pattern.exec(text);
    // let times = Array.from(text.matchAll(total_time_pattern), item => {
    let times =  { rules: item.groups.quantity, time: item.groups.time };
    // });
    let egraphs = Array.from(text.matchAll(egraph_size_pattern), item => {
        return { n: item[1], e: item[2], cv: item[3] };
    });
    let phases = Array.from(text.matchAll(phase_time_pattern), item => { return { run_rewrites: item[1], rule_discovery: item[2], rule_minimization: item[3] } });
    let validation = Array.from(text.matchAll(validation_pattern), item => { if (item != undefined) return parseFloat(item[1]); return 0 });
    // Assuming they would be the same length
    phases.forEach((d, i) => {
        v = validation[i] || 0.0;
        d["rule_minimization"] = (parseFloat(d["rule_minimization"]) - v).toString();
        d["rule_validation"] = v.toString();
    })

    let real = (/^real\s*([\w.]+)$/gm).exec(text)[1];
    let user = (/^user\s*([\w.]+)$/gm).exec(text)[1];
    let sys = (/^sys\s*([\w.]+)$/gm).exec(text)[1];

    real = +real.split("m")[0] * 60 + +real.split("m")[1].replace("s", "")
    user = +user.split("m")[0] * 60 + +user.split("m")[1].replace("s", "")
    sys = +sys.split("m")[0] * 60 + +sys.split("m")[1].replace("s", "")

    return {
        learned: times,
        egraphs: egraphs,
        phases: phases,
        real: real,
        user: user,
        sys: sys
    }

}

let parse_name = (name, data) => {
    let pieces = name.split(/[-_.]/g);
    data.domain = pieces[0];
    data.vars = pieces[1];
    data.iters = pieces[2];
    if (pieces.length == 5) { // accounting for log at end
        data.run = pieces[3]
    } else {
        data.mrat_m = pieces[3];
        data.run = pieces[4];
    }
}

let print_data = (data) => {
    let dataByTypeAndDomain = {};
    let dataByDomainOnly = {};
    Object.keys(dataByType).forEach(type => {
        let perType = dataByType[type];
        let domains = Array.from(new Set(perType.map(d => d.domain)));
        // for each domain, 
        dataByTypeAndDomain[type] = domains.reduce((acc, domain) => {
            let items = perType.filter(d => d.domain == domain);
            let existing = dataByDomainOnly[domain] || [];
            dataByDomainOnly[domain] = existing.concat(items);
            acc[domain] = perType.filter(d => d.domain == domain);
            return acc;
        }, {})

        console.log(dataByTypeAndDomain);
    })
    // organize by domain too...

    fs.writeFile(output_path, JSON.stringify(data),
        () => console.log("done!"));
}

console.log(process.cwd())
input_folder = process.argv[2];
output_path = "output/parsed.json";
load_files(input_folder, output_path)
