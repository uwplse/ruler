#
# make sure you have the `moreutils` package
# and pandoc if you want to generate latex report

SHELL=/usr/bin/env bash

all:

rust-src=$(shell find ../../src/ -type f)

# check for the hardcoded programs that are in the VM
xsv=$(shell command -v ./xsv || command -v xsv)
cvc4=$(shell command -v ./cvc4-1.8-x86_64-linux-opt || command -v cvc4) \
  --sygus-rr-synth              \
  --sygus-rr-synth-check        \
  --no-sygus-sym-break          \
  --no-sygus-sym-break-dynamic  \
  --sygus-rr-synth-filter-cong  \
  --sygus-rr-synth-filter-match \
  --sygus-rr-synth-filter-order \
  --sygus-expr-miner-check-timeout=1000 \
  --strings-exp

ruler-reports=
cvc4-logs=
cvc4-reports=
diffs=

export RAYON_NUM_THREADS ?= 10

define synthesize # (domain, variables, iters, rules-to-take)
ruler-reports += results/ruler/$(1)-$(2)vars-$(3)iters.json
results/ruler/$(1)-$(2)vars-$(3)iters.json: # $(rust-src)
	mkdir -p results/ruler
	cargo $(1) --variables $(2) --iters $(3) --rules-to-take $(4) --outfile $$@

cvc4-logs += results/cvc4/$(1)-$(2)vars-$(3)iters.txt
results/cvc4/$(1)-$(2)vars-$(3)iters.txt: cvc4/$(1)-$(2)vars.sy
	mkdir -p results/cvc4
	\time -p $(cvc4) --sygus-abort-size $(3) $$< 2>&1 | sponge $$@

cvc4-reports += results/cvc4/$(1)-$(2)vars-$(3)iters.json
results/cvc4/$(1)-$(2)vars-$(3)iters.json: results/cvc4/$(1)-$(2)vars-$(3)iters.txt # $(rust-src)
	cargo convert-$(1) $$< $$@

diffs += results/diffs/$(1)-$(2)vars-$(3)iters.json
$(1) += results/diffs/$(1)-$(2)vars-$(3)iters.json
results/diffs/$(1)-$(2)vars-$(3)iters.json: results/ruler/$(1)-$(2)vars-$(3)iters.json results/cvc4/$(1)-$(2)vars-$(3)iters.json
	mkdir -p results/diffs
	cargo derive-$(1) $$^ $$@
endef

define synthesize-no-consts # (domain, variables, iters, rules-to-take)
ruler-no-consts-reports += results/ruler/$(1)noconsts-$(2)vars-$(3)iters.json
results/ruler/$(1)noconsts-$(2)vars-$(3)iters.json: # $(rust-src)
	mkdir -p results/ruler
	cargo $(1) --variables $(2) --iters $(3) --rules-to-take $(4) --no-constants-above-iter=1 --outfile $$@

diffs-no-consts += results/diffs/$(1)noconsts-$(2)vars-$(3)iters.json
results/diffs/$(1)noconsts-$(2)vars-$(3)iters.json: results/ruler/$(1)noconsts-$(2)vars-$(3)iters.json results/ruler/$(1)-$(2)vars-$(3)iters.json
	mkdir -p results/diffs
	cargo derive-$(1) $$^ $$@ --iter-limit=4
endef

PRECIOUS: $(cvc4-logs) $(ruler-reports) $(diffs)

# params: (domain, variables, iters, rules-to-take)
# You can't have spaces between them
# $(eval $(call synthesize,bool,2,2,0))
# $(eval $(call synthesize,bool,2,3,0))
$(eval $(call synthesize,bool,3,2,0))
$(eval $(call synthesize,bool,3,3,0))
# $(eval $(call synthesize,bool,4,2,0))
# $(eval $(call synthesize,bool,4,3,0))
# $(eval $(call synthesize,bool,4,4,0))

# $(eval $(call synthesize,bv4,2,2,0))
# $(eval $(call synthesize,bv4,2,3,0))
$(eval $(call synthesize,bv4,3,2,0))
$(eval $(call synthesize,bv4,3,3,0))

# $(eval $(call synthesize-no-consts,bv4,3,2,0))
# $(eval $(call synthesize-no-consts,bv4,3,3,0))
# $(eval $(call synthesize-no-consts,bv32,3,2,0))
# $(eval $(call synthesize-no-consts,bv32,3,3,0))

$(eval $(call synthesize,bv32,3,2,0))
$(eval $(call synthesize,bv32,3,3,0))

# $(eval $(call synthesize,bv4ns,2,2,0))
# $(eval $(call synthesize,bv4ns,2,3,0))
# $(eval $(call synthesize,bv4ns,3,2,0))
# $(eval $(call synthesize,bv4ns,3,3,0))

# $(eval $(call synthesize,str,3,2,0))


.PHONY: latex-report
latex-report: $(diffs)
	python3 compare.py $^ | pandoc -f csv -t latex --columns 200 | sed 's/tabularnewline/\\/'

.PHONY: report
report: $(diffs)
	python3 compare.py $^ | $(xsv) table

.PHONY: report-no-consts
report-no-consts: $(diffs-no-consts)
	python3 compare.py $^ | $(xsv) table

.PHONY: latex-report-no-consts
latex-report-no-consts: $(diffs-no-consts)
	python3 compare.py $^ | pandoc -f csv -t latex --columns 200 | sed 's/tabularnewline/\\/'

.PHONY: ruler-reports
ruler-reports: $(ruler-reports)
.PHONY: cvc4-reports
cvc4-reports: $(cvc4-reports)
.PHONY: diffs
diffs: $(diffs)
.PHONY: bool
bool: $(bool)
.PHONY: bv4
bv4: $(bv4)

all: ruler-reports cvc4-reports diffs report

clean:
	rm -rf results

# Local Variables:
# indent-tabs-mode: t
# End:
