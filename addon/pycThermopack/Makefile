PYTHON := python
PYCTP_DIR = ./pyctp

pytest:
	$(PYTHON) -m pytest

debug:
	$(PYTHON) ./makescript.py debug
	cd $(PYCTP_DIR); $(PYTHON) ./generate_platform_specifics.py; cd ..

optim:
	$(PYTHON) ./makescript.py optim
	cd $(PYCTP_DIR); $(PYTHON) ./generate_platform_specifics.py; cd ..

clean:
	@rm -f pyctp/libthermopack.so
