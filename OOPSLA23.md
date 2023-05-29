# Equality Saturation Theory Exploration à la Carte

This is the artifact for our paper
"Equality Saturation Theory Exploration à la Carte".

<!-- TODO: artifact goals / reusable, etc. blurb -->

## Dependencies
<!-- Adapted from https://github.com/uwplse/szalinski -->
* Install Rust

* Install make by typing: `sudo apt-get install make`

* Install g++ by typing: `sudo apt-get install g++`

* Install pip by typing `sudo apt install python3-pip` and then
install the following:
`pip3 install pandas && pip3 install "jinja2>=3"`

## Step-by-step

Our paper has TODO quantitative evaluations:

1. Synthesizing Szalinski's CAD identities (`Section 6.2.2`): We show that Ruler can infer CAD identities for Szalinski that closely match the pre-existing hand-written rules.

2. TODO: add other evals.

## 1. Synthesizing Szalinski's CAD identities (Table 4).

First, clone our evaluation branch of Szalinski within the top-level directory of this repository.

```
git clone --branch eval https://github.com/rtjoa/szalinski
```

To show the results of a preexisting run, run the following from the `szalinski` repository. Table 4 should be printed to output.
```
./to_latex.py
```

To regenerate Table 4 from scratch, the Szalinski tool will have to be run end-to-end, which involves the following extra dependencies:

* Install jq by typing: `sudo apt-get install jq`

* Install [CGAL](https://www.cgal.org/download/linux.html) by typing
  `sudo apt-get install libcgal-dev`

* Install [OpenSCAD](https://www.openscad.org/) from [https://launchpad.net/~openscad/+archive/ubuntu/releases](https://launchpad.net/~openscad/+archive/ubuntu/releases)

Now, run the following from the `ruler` repository. It should finish within 30 minutes.
```
./scripts/oopsla23/sz-eval.sh
```
