const fs = require('fs');
// load all the files
let data = [];
let dataByType = {};

let load_files = (base_folder,  is_rr) => {
    // check if slash, add if none

    // the names of the subfolders our script creates
    let phase_time = "phase-times";
    let orat = "orat";
    let mrat = "mrat";
    let default_conf = "default";
    let no_run_rewrites = "no-run-rewrites";

    // if this is a RR vs no-RR, we don't have all folders
    if (is_rr) {
        load_dir(base_folder + "orat-default", "default",
            () => load_dir(base_folder + "no-run-rewrites", "no-run-rewrites",
                () => print_data(data)));
        return;
    }

    // otherwise load all of them
    // this is weirdly CPS because we want to load all the files
    // but we don't know how many runs we've made
    load_dir(base_folder + phase_time, "phase-times",
        () => load_dir(base_folder + default_conf, "default",
            () => load_dir(base_folder + orat, "orat",
                () => load_dir(base_folder + mrat, "mrat",
                    // () => load_dir(base_folder + no_run_rewrites, "no-run-rewrites",
                        () => print_data(data)))));
}

// I should really just make promises... 
let load_dir = (path, type, k) => {
    // set up a map to data by the run type e.g. default run, orat run
    dataByType[type] = [];
    console.log(path);
    
    // let's find and read all the logfiles in that directory
    fs.readdir(path, 'utf8', (err, filenames) => {
        // console.log(filenames);
        if (err != undefined || err != null) {
            console.log('Something went wrong');
        }

        // get a list of logfiles to process
        // files_to_process = filenames.filter(str => str.substring(str.length - ".log".length, str.length) === ".log")
        files_to_process = filenames.filter(str => str.endsWith(".log"));
        
        // Use CPS so we know when all files are done 
        let process = (files, files_k) => {
            // todo list is empty
            if (files.length == 0) {
                // do "the rest of the work"
                files_k();
                return;
            }
            // todo list has a head
            let file = files[0];

            // get the contents of the file
            fs.readFile(path + '/' + file, 'utf8', (err, text) => {
                // process all relevant data
                let entry = make_entry(text);
                if (entry != undefined) {
                    entry.name = file;
                    entry.type = type;
                    parse_name(file, entry);
                    data.push(entry);
                // push this data to the array for this type
                    dataByType[type].push(entry);
                } else {
                    console.log("Failed to parse " + path + '/' + file)
                }

                // keep working!
                process(files.slice(1), files_k)
            })
        }

        console.log(files_to_process)
        process(files_to_process, k);
    })
}

let make_entry = (text) => {
    if (text === undefined) { 
        return;
    }

    // sorry... this is the most efficient way to handle any errors
    try {
    // look for each relevant log piece
    let total_time_pattern = /Learned (?<quantity>[\d]+)[a-z\s]*(?<time>[.\d]+)$/gm;
    let egraph_size_pattern = /egraph n=([\d]+), e=([\d]+)/gm;
    let phase_time_pattern = /Time taken in... [\w.\s]+: ([\d.]+), [\w.\s]+: ([\d.]+), [\w.\s]+: ([\d.]+)$/gm
    let validation_pattern = /Time taken in validation+: ([\d.]+)$/gm // may not exist!

    // N.B. because of how this was initially logged, we need to subtract validation time from the minimization time.
    let item = total_time_pattern.exec(text);

    // let times = Array.from(text.matchAll(total_time_pattern), item => {
    let times =  { rules: item.groups.quantity, time: item.groups.time };
    // });
    
    // get the matching groups and parse into separate pieces 
    let egraphs = Array.from(text.matchAll(egraph_size_pattern), item => {
        return { n: item[1], e: item[2], cv: item[3] };
    });

    let phases = Array.from(text.matchAll(phase_time_pattern), item => { 
        return { run_rewrites: item[1], rule_discovery: item[2], rule_minimization: item[3] } });

    // there might not be any validation, so we just give 0 in that case
    // Assuming they would be the same length
    let validation = Array.from(text.matchAll(validation_pattern), item => { if (item != undefined) return parseFloat(item[1]); return 0 });

    // TODO: hacky way of getting around 1-to-1
    // add up all the leftover validation pieces and attach to last phase-time logging
    if (validation.length > phases.length) {
        let rest = validation.slice(phases.length)
        let rest_validation = rest.reduce((x, acc) => x + acc, 0)
        validation[phases.length - 1] += rest_validation
    }
    phases.forEach((d, i) => {
        // TODO this assumes there is one validation statement emitted per phase, but this may not be true. add together all validation in between phase statements
        v = validation[i] || 0.0;  
        d["rule_minimization"] = (parseFloat(d["rule_minimization"]) - v).toString();
        d["rule_validation"] = v.toString();
    })

    // used for getting output from time command
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

    } catch (e) {
        console.log("Failed to parse: " + e);
        return undefined;
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
    // format the data from the map into json array
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
output_path = process.argv[2] + "/parsed.json";
// output_path = "output/parsed.json";
let is_rr = process.argv[3]
// console.log(is_rr)
load_files(input_folder, is_rr === "yes")
