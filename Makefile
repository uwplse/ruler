#
# make sure you have the `moreutils` package
# and pandoc if you want to generate latex report

SHELL=/usr/bin/env bash

all:

rust-src=$(shell find src/ -type f)
<<<<<<< HEAD
ruler=cargo run --release --bin ruler --
=======
>>>>>>> origin/trait
cvc4=cvc4 \
  --sygus-rr-synth              \
  --sygus-rr-synth-check        \
  --no-sygus-sym-break          \
  --no-sygus-sym-break-dynamic  \
  --sygus-rr-synth-filter-cong  \
  --sygus-rr-synth-filter-match \
  --sygus-rr-synth-filter-order

ruler-reports=
cvc4-logs=
cvc4-reports=
diffs=

define synthesize # (domain, variables, iters, rules-to-take)
ruler-reports += results/ruler/$(1)-$(2)vars-$(3)iters.json
results/ruler/$(1)-$(2)vars-$(3)iters.json: # $(rust-src)
	mkdir -p results/ruler
<<<<<<< HEAD
	$(ruler) --domain $(1) --variables $(2) --iters $(3) --rules-to-take $(4) --outfile $$@
=======
	# cargo $(1) --variables $(2) --iters $(3) --rules-to-take $(4) --outfile $$@
	cargo $(1) --variables $(2) --iters $(3) --minimize --outfile $$@
>>>>>>> origin/trait

cvc4-logs += results/cvc4/$(1)-$(2)vars-$(3)iters.txt
results/cvc4/$(1)-$(2)vars-$(3)iters.txt: cvc4/$(1)-$(2)vars.sy
	mkdir -p results/cvc4
	\time -p $(cvc4) --sygus-abort-size $(3) $$< 2>&1 | sponge $$@

cvc4-reports += results/cvc4/$(1)-$(2)vars-$(3)iters.json
results/cvc4/$(1)-$(2)vars-$(3)iters.json: results/cvc4/$(1)-$(2)vars-$(3)iters.txt # $(rust-src)
<<<<<<< HEAD
	cargo run --release --bin convert_sexp -- $$< | sponge $$@

diffs += results/diffs/$(1)-$(2)vars-$(3)iters.json
results/diffs/$(1)-$(2)vars-$(3)iters.json: results/ruler/$(1)-$(2)vars-$(3)iters.json results/cvc4/$(1)-$(2)vars-$(3)iters.json
	mkdir -p results/diffs
	cargo run --release --bin derive -- $$^ $$@
=======
	cargo convert-$(1) $$< $$@

diffs += results/diffs/$(1)-$(2)vars-$(3)iters.json
$(1) += results/diffs/$(1)-$(2)vars-$(3)iters.json
results/diffs/$(1)-$(2)vars-$(3)iters.json: results/ruler/$(1)-$(2)vars-$(3)iters.json results/cvc4/$(1)-$(2)vars-$(3)iters.json
	mkdir -p results/diffs
	cargo derive-$(1) $$^ $$@
>>>>>>> origin/trait
endef

PRECIOUS: $(cvc4-logs) $(ruler-reports) $(diffs)

# params: (domain, variables, iters, rules-to-take)
# You can't have spaces between them
$(eval $(call synthesize,bool,2,2,99999))
<<<<<<< HEAD
$(eval $(call synthesize,bool,2,3,99999))
$(eval $(call synthesize,bool,3,2,99999))
$(eval $(call synthesize,bool,3,3,99999))
$(eval $(call synthesize,bool,4,2,99999))
$(eval $(call synthesize,bool,4,3,99999))
# $(eval $(call synthesize,bool,4,4,99999))
#
$(eval $(call synthesize,bv4,2,2,99999))
$(eval $(call synthesize,bv4,2,3,99999))
$(eval $(call synthesize,bv4,3,2,99999))
$(eval $(call synthesize,bv4,3,3,99999))

$(eval $(call synthesize,bv4ns,2,2,99999))
$(eval $(call synthesize,bv4ns,2,3,99999))
$(eval $(call synthesize,bv4ns,3,2,99999))
$(eval $(call synthesize,bv4ns,3,3,99999))


.PHONY: report
report: $(diffs)
	./scripts/compare.py $^ | pandoc -f csv -t latex --columns 200 | sed 's/tabularnewline/\\/'
=======
# $(eval $(call synthesize,bool,2,3,99999))
$(eval $(call synthesize,bool,3,2,99999))
# $(eval $(call synthesize,bool,3,3,99999))
# $(eval $(call synthesize,bool,4,2,99999))
# $(eval $(call synthesize,bool,4,3,99999))
# $(eval $(call synthesize,bool,4,4,99999))

$(eval $(call synthesize,bv4,2,2,99999))
# $(eval $(call synthesize,bv4,2,3,99999))
$(eval $(call synthesize,bv4,3,2,99999))
# $(eval $(call synthesize,bv4,3,3,99999))

# $(eval $(call synthesize,bv4ns,2,2,99999))
# $(eval $(call synthesize,bv4ns,2,3,99999))
# $(eval $(call synthesize,bv4ns,3,2,99999))
# $(eval $(call synthesize,bv4ns,3,3,99999))


.PHONY: latex-report
latex-report: $(diffs)
	./scripts/compare.py $^ | pandoc -f csv -t latex --columns 200 | sed 's/tabularnewline/\\/'

.PHONY: report
report: $(diffs)
	./scripts/compare.py $^ | xsv table
>>>>>>> origin/trait

.PHONY: ruler-reports
ruler-reports: $(ruler-reports)
.PHONY: cvc4-reports
cvc4-reports: $(cvc4-reports)
.PHONY: diffs
diffs: $(diffs)
<<<<<<< HEAD

all: ruler-reports cvc4-reports diffs
=======
.PHONY: bool
bool: $(bool)
.PHONY: bv4
bv4: $(bv4)

all: ruler-reports cvc4-reports diffs report
>>>>>>> origin/trait

# Local Variables:
# indent-tabs-mode: t
# End:
