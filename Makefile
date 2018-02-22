PROJECT ?= TensorFlock 
SRC_DIR ?= src/
PROJECT_TOP ?= toplevel 
PROJECT_EXTENSION ?= src/toplevel.native
OCAML_SENTINAL ?= .ocaml-sentinal
PROJECT_PARSER ?= parser
OPAM_FILE ?= opam

$(OCAML_SENTINAL): $(OPAM_FILE)
	opam pin add --no-action $(PROJECT) . -y
	opam install --deps-only $(PROJECT)
	touch $@

$(PROJECT_EXTENSION): $(OCAML_SENTINAL) clean $(SRC_DIR)* 
	ocamlbuild $@

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

zip: clean
	zip -r tensorflock.zip ../TensorFlock -x "*.git*" "*.gitignore*" "*.circleci*" "*.merlin*" "*_state*" "*proposal*"

clean: 
	ocamlbuild -clean
ifneq ($(wildcard _state),)
	rm -rf _state
endif
ifneq ($(wildcard *.zip),)
	rm -f *.zip 
endif

.PHONY: state test clean zip
