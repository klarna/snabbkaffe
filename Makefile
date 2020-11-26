BUILD_DIR := $(CURDIR)/_build
CONCUERROR := $(BUILD_DIR)/Concuerror/bin/concuerror
CONCUERROR_RUN := $(CONCUERROR) -x code -x code_server -x error_handler --assertions_only \
	-pa $(BUILD_DIR)/concuerror+test/lib/snabbkaffe/ebin

.PHONY: compile
compile:
	rebar3 do dialyzer,eunit,ct

concuerror = $(CONCUERROR_RUN) -f $(BUILD_DIR)/concuerror+test/lib/snabbkaffe/test/concuerror_tests.beam -t $(1) || \
	{ cat concuerror_report.txt; exit 1; }

.PHONY: concuerror_test
concuerror_test: $(CONCUERROR)
	rebar3 as concuerror eunit
	$(call concuerror,race_test)
	$(call concuerror,causality_test)
	$(call concuerror,fail_test)

$(CONCUERROR):
	mkdir -p _build/
	cd _build && git clone https://github.com/parapluu/Concuerror.git
	$(MAKE) -C _build/Concuerror/
