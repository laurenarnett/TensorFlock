-include *.mk
PROJECT ?= TensorFlock
SRC_DIR ?= src/
PROJECT_TOP ?= toplevel.native
PROJECT_EXTENSION ?= src/$(PROJECT_TOP)
OCAML_SENTINAL ?= .ocaml-sentinal
PROJECT_PARSER ?= parser
OPAM_FILE ?= opam
COMPILER_FLAGS = -cflag -warn-error=+A-4-42-27
COMPILER_PACKAGES = -use-ocamlfind -package llvm,llvm.analysis,llvm.bitwriter 

# Docker: after updating the Dockerfile, build a new image and tag with an
# incremented version number. Also update the version in .drone.yml
DOCKER_IMAGE = nbuonin/ocaml4.06-llvm3.8
DOCKER_TAG = v3

SHELL=/bin/sh

$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) . -y
	opam install --deps-only $(PROJECT)
	touch $@

$(PROJECT_TOP): $(OCAML_SENTINAL) $(SRC_DIR)*
	ocamlbuild $(COMPILER_FLAGS) $(COMPILER_PACKAGES) $(PROJECT_EXTENSION)
	clang -c $(SRC_DIR)runtime.c -o _build/src/runtime.o

state: $(OCAML_SENTINAL) clean
	ocamlyacc -v $(SRC_DIR)$(PROJECT_PARSER).mly
ifeq ($(wildcard _state),)
	mkdir _state
endif
	mv $(SRC_DIR)$(PROJECT_PARSER).ml _state/.
	mv $(SRC_DIR)$(PROJECT_PARSER).mli _state/.
	mv $(SRC_DIR)$(PROJECT_PARSER).output _state/.

test: | llvm $(OCAML_SENTINAL) $(PROJECT_TOP)
	bash ./test_runner.sh

demo: | llvm $(PROJECT_TOP)
	./toplevel.native -c tests/codegen/pass/interesting_math.tf
	lli output.ll

zip: clean
	zip -r tensorflock.zip ../`pwd | sed -E 's/(.*\/)(.*)/\2/'` -x "*.git*" "*.gitignore*" "*.drone.yml*" "*.merlin*" "*_state*" "*proposal*" "*.DS_Store*" "*local.mk*"

clean: 
ifneq ($(wildcard *.native),)
	rm *.native
endif
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
ifneq ($(wildcard *.o),)
	rm -f *.o 
endif
ifneq ($(wildcard *.out),)
	rm -f *.out 
endif
ifneq ($(wildcard *.s),)
	rm -f *.s 
endif

watch:
	./watch.sh $(SRC_DIR)

docker-build-image:
	docker build -t $(DOCKER_IMAGE):$(DOCKER_TAG) docker

docker-shell:
	docker run --rm -it -v "`pwd`":/root/TensorFlock -w=/root/TensorFlock --entrypoint=/bin/bash $(DOCKER_IMAGE):$(DOCKER_TAG)

docker-make:
	docker run --rm -it -v "`pwd`":/root/TensorFlock -w=/root/TensorFlock --entrypoint="" $(DOCKER_IMAGE):$(DOCKER_TAG) make

docker-test:
	docker run --rm -it -v "`pwd`":/root/TensorFlock -w=/root/TensorFlock --entrypoint="" $(DOCKER_IMAGE):$(DOCKER_TAG) make test

docker-attach:
	@echo "Use ctrl-c to detach from running container"
	docker exec -it --detach-keys="ctrl-c" -w=/root/TensorFlock `docker ps --latest --filter ancestor=$(DOCKER_IMAGE):$(DOCKER_TAG) --filter status=running --format "{{.ID}}"` bash

# if lli isn't on the path, try to set it from a var, else warn user end exit
llvm:
ifeq ($(shell which lli),)
ifneq ($(LLVM_PATH),)
export PATH:=$(LLVM_PATH):$(PATH)
else
	$(error "Make can not find where LLVM is located. Please create a file named 'local.mk' in the project directory and assign LLVM_PATH=/path/to/llvm/binaries")
endif
endif

.PHONY: state test clean zip demo docker docker-shell docker-make docker-test llvm
