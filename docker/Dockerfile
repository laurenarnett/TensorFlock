FROM ubuntu:16.04
ENV OCAML_VERSION=4.06.0 \
    OPAMYES=1 \
    OPAMJOBS=2 \
    OPAMROOTISOK=1

# Install some things we need to get started
RUN apt-get update && apt-get install -y wget build-essential make m4 patch unzip git pkg-config

# Install LLVM
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -; \
    echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial main" >> /etc/apt/sources.list; \
    echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial main" >> /etc/apt/sources.list; \
    apt-get install -y clang-3.8 lldb-3.8

# Install Opam
RUN wget -O $HOME/opam https://github.com/ocaml/opam/releases/download/2.0.0-rc/opam-2.0.0-rc-x86_64-linux; \
    chmod +x $HOME/opam; \
    $HOME/opam --version 

# Compile OCaml and manually set $HOME/opam config env 
RUN $HOME/opam init --compiler=$OCAML_VERSION

RUN ln $HOME/opam /bin 

RUN opam install llvm.3.8

ENV OPAM_SWITCH_PREFIX=/root/.opam/4.06.0 \
    CAML_LD_LIBRARY_PATH=/root/.opam/4.06.0/lib/stublibs:/root/.opam/4.06.0/lib/ocaml/stublibs:/root/.opam/4.06.0/lib/ocaml \
    OCAML_TOPLEVEL_PATH=/root/.opam/4.06.0/lib/toplevel \
    MANPATH=:/root/.opam/4.06.0/man \
    PATH=/root/.opam/4.06.0/bin:$PATH

# Link the LLVM dependencies to bare names
RUN ln -s /usr/bin/lli-3.8 /usr/bin/lli
RUN ln -s /usr/bin/llc-3.8 /usr/bin/llc
RUN ln -s /usr/bin/clang-3.8 /usr/bin/clang

ENTRYPOINT ["/bin/bash"]
