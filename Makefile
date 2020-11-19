#
# make sure you have the `moreutils` package


SHELL=/usr/bin/env bash

all:

rust-src=$(shell find src/ -type f)
ruler=cargo run --release --bin ruler --
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
	$(ruler) --domain $(1) --variables $(2) --iters $(3) --rules-to-take $(4) --outfile $$@

cvc4-logs += results/cvc4/$(1)-$(2)vars-$(3)iters.txt
results/cvc4/$(1)-$(2)vars-$(3)iters.txt: cvc4/$(1)-$(2)vars.sy
	mkdir -p results/cvc4
	\time -p $(cvc4) --sygus-abort-size $(3) $$< 2>&1 | sponge $$@

cvc4-reports += results/cvc4/$(1)-$(2)vars-$(3)iters.json
results/cvc4/$(1)-$(2)vars-$(3)iters.json: results/cvc4/$(1)-$(2)vars-$(3)iters.txt $(rust-src)
	cargo run --release --bin convert_sexp -- $$< | sponge $$@

diffs += results/diffs/$(1)-$(2)vars-$(3)iters.json
results/diffs/$(1)-$(2)vars-$(3)iters.json: results/ruler/$(1)-$(2)vars-$(3)iters.json results/cvc4/$(1)-$(2)vars-$(3)iters.json
	mkdir -p results/diffs
	cargo run --release --bin derive -- $$^ $$@
endef

PRECIOUS: $(cvc4-logs) $(ruler-reports) $(diffs)

# params: (domain, variables, iters, rules-to-take)
# You can't have spaces between them
$(eval $(call synthesize,bool,2,2,99999))
$(eval $(call synthesize,bool,2,3,99999))
$(eval $(call synthesize,bool,3,2,99999))
$(eval $(call synthesize,bool,3,3,99999))
$(eval $(call synthesize,bool,4,2,99999))
$(eval $(call synthesize,bool,4,3,99999))
# $(eval $(call synthesize,bool,4,4,99999))

$(eval $(call synthesize,bv4,2,2,99999))
$(eval $(call synthesize,bv4,2,3,99999))
$(eval $(call synthesize,bv4,3,2,99999))
$(eval $(call synthesize,bv4,3,3,99999))

.PHONY: ruler-reports
ruler-reports: $(ruler-reports)
.PHONY: cvc4-reports
cvc4-reports: $(cvc4-reports)

all: ruler-reports cvc4-reports

# Local Variables:
# indent-tabs-mode: t
# End:
