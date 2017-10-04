TEST_IN_DIR=../test
TEST_OUT_DIR=../test_output

TESTFILE_IN=$(sort $(wildcard $(TEST_IN_DIR)/*.ac))
TESTFILE_OUT=$(patsubst $(TEST_IN_DIR)/%,$(TEST_OUT_DIR)/%,$(TESTFILE_IN:.ac=))

All: AcDc

test: test_pre test_all
test_pre: All $(TEST_OUT_DIR)
test_all: $(TESTFILE_OUT)

clean: clean_exec clean_test
clean_exec:
	rm -f AcDc
clean_test:
	rm -rf $(TEST_OUT_DIR)

.FORCE:


% : %.c
	gcc $< -std=c99 -o $@
$(TEST_OUT_DIR):
	mkdir -p $@
../test_output/%: ../test/%.ac .FORCE
	@echo "=== Testing $< ==="
	@./AcDc $< $@ || (echo "\033[31m[ Test exited with error $$? ]\033[0m")
	@echo ""
