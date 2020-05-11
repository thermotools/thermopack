PYTHON := python

test: test-python2 test-python3

test-python2 test-python3:
	@$(MAKE) PYTHON="$(subst test-,,$@)" MODE=debug test-general
	@$(MAKE) PYTHON="$(subst test-,,$@)" MODE=optim test-general

test-general:
	@$(MAKE) $(MODE)
	@$(MAKE) pytest

pytest:
	$(PYTHON) -m pytest

debug:
	$(PYTHON) ./makescript.py debug

optim:
	$(PYTHON) ./makescript.py optim

clean:
	@rm -f pytp/tp*.so
