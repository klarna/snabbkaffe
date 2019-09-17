BUILD_DIR := $(CURDIR)/_build
CONCUERROR := $(BUILD_DIR)/Concuerror/bin/concuerror
CONCUERROR_RUN := $(CONCUERROR) -x snabbkaffe_collector -x snabbkaffe \
	-x code -x code_server -x error_handler \
	-pa $(BUILD_DIR)/concuerror+test/lib/snabbkaffe/ebin

.PHONY: compile
compile:
	rebar3 do dialyzer,eunit,ct

.PHONY: concuerror_test
concuerror_test: $(CONCUERROR)
	rebar3 as concuerror eunit
	$(CONCUERROR_RUN) -f $(BUILD_DIR)/concuerror+test/lib/snabbkaffe/test/concuerror_tests.beam

$(CONCUERROR):
	mkdir -p _build/
	cd _build && git clone https://github.com/parapluu/Concuerror.git
	$(MAKE) -C _build/Concuerror/
