LISP ?= sbcl
ASD := bkmks.asd
ASD_PATH := $(shell pwd)/$(ASD)
BINARY := bkmks
INSTALL_PATH := ~/.local/bin

build:
	$(LISP) --eval "(asdf:load-asd #P\"$(ASD_PATH)\")" \
		--eval '(ql:quickload :bkmks)' \
		--eval "(sb-ext:save-lisp-and-die #P\"$(BINARY)\" :toplevel #'main :executable t)" \
		--eval '(quit)'

install:
	install -v $(BINARY) $(INSTALL_PATH)/$(BINARY)

clean:
	rm -v $(BINARY)

uninstall:
	rm -v $(INSTALL_PATH)/$(BINARY)
