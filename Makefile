PROJECT ?= TensorFlock 
SRC_DIR ?= src/
PROJECT_TOP ?= toplevel 
PROJECT_EXTENSION ?= src/toplevel.native
OCAML_SENTINAL ?= .ocaml-sentinal
PROJECT_PARSER ?= parser
OPAM_FILE ?= opam
COMPILER_FLAGS = -cflag -warn-error=+A-4-42-27
COMPILER_PACKAGES = -use-ocamlfind -package llvm,llvm.analysis,llvm.bitwriter 

SHELL=/bin/bash
ifeq ($(shell uname), Darwin)
export PATH:=$(shell brew --prefix llvm)/bin:$(PATH)
endif


$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) . -y
	opam install --deps-only $(PROJECT)
	touch $@

$(PROJECT_EXTENSION): $(OCAML_SENTINAL) clean $(SRC_DIR)* 
	ocamlbuild $(COMPILER_FLAGS) $(COMPILER_PACKAGES) $@

state: $(OCAML_SENTINAL) clean
	ocamlyacc -v $(SRC_DIR)$(PROJECT_PARSER).mly
ifeq ($(wildcard _state),)
	mkdir _state
endif
	mv $(SRC_DIR)$(PROJECT_PARSER).ml _state/.
	mv $(SRC_DIR)$(PROJECT_PARSER).mli _state/.
	mv $(SRC_DIR)$(PROJECT_PARSER).output _state/.

test: $(PROJECT_EXTENSION) $(OCAML_SENTINAL)
	bash ./test_runner.sh

demo: $(PROJECT_EXTENTION)
	./toplevel.native -c tests/codegen/pass/interesting_math.tf
	lli output.ll

zip: clean
	zip -r tensorflock.zip ../TensorFlock -x "*.git*" "*.gitignore*" "*.circleci*" "*.merlin*" "*_state*" "*proposal*"

clean: 
	ocamlbuild -clean
ifneq ($(wildcard *.ll),)
	rm *.ll
endif
ifneq ($(wildcard _state),)
	rm -rf _state
endif
ifneq ($(wildcard *.zip),)
	rm -f *.zip 
endif

docker-build-image:
	docker build -t nbuonin/ocaml4.06-llvm3.8 docker

docker-shell:
	docker run --rm -it -v `pwd`:/root/TensorFlock -w=/root/TensorFlock --entrypoint=/bin/bash nbuonin/ocaml4.06-llvm3.6

docker-make:
	docker run --rm -it -v `pwd`:/root/TensorFlock -w=/root/TensorFlock --entrypoint="" nbuonin/ocaml4.06-llvm3.6 make

docker-make-test:
	docker run --rm -it -v `pwd`:/root/TensorFlock -w=/root/TensorFlock --entrypoint="" nbuonin/ocaml4.06-llvm3.6 make test

.PHONY: state test clean zip docker demo docker-shell docker-make docker-make-test
