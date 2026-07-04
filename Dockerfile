# K has binaries available or Ubuntu.
# Easier than Nix for quick tests
FROM docker.io/ocaml/opam:ubuntu-22.04-ocaml-4.13
ARG OCAML_VERSION="4.13.1"
ARG K_VERSION="5.5.53"
WORKDIR /kamelo
COPY --chown=opam:opam . .

# TODO: determine if versions of system libraries should be pinned. Not mentioned in the original CI script. 
USER root
# hadolint ignore=DL3009
RUN <<EOF 
  echo "Upgrading base system..."
  # Need extra repos for K framework
  echo "deb http://archive.ubuntu.com/ubuntu/ jammy main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu/ jammy-updates main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu/ jammy-security main restricted universe multiverse" > /etc/apt/sources.list
  apt-get update
  apt-get upgrade --yes
  apt-get clean
EOF

RUN <<EOF
  echo "Installing framework dependencies"
  apt-get install --yes \
    wget \
    make \
    z3 \
    python3-z3 \
    libgmp-dev \
    m4 \
    pkg-config \
    autoconf \
    zlib1g-dev
  rm -rf /var/lib/apt/lists/*
EOF

USER opam
RUN <<EOF
  echo "Setting up opam..." 
  # TODO: decide whether to use
  # custom switch or not.
  # if [ ! -d _opam ]; then
  #  echo "No local switch detected. Creating new switch."
  #  opam switch create --yes kamelo ocaml-base-compiler.${OCAML_VERSION}
  # fi
  opam update
EOF

RUN <<EOF
  echo "Installing Lambdapi..."
  opam install --yes dune bindlib timed sedlex menhir pratter ezjsonm yaml yojson cmdliner why3 alcotest alt-ergo odoc
  why3 config detect
  opam install --yes lambdapi.2.2.1
EOF

USER root
# hadolint ignore=DL3015
RUN <<EOF
  echo "Installing K Framework..." 
  wget --quiet https://github.com/runtimeverification/k/releases/download/v"${K_VERSION}"/K.Framework.Ubuntu.Jammy.22.04.Deb -O K.deb
  apt-get install -y ./K.deb
EOF

USER opam
RUN <<EOF
  echo "Building KaMeLo..."
  eval "$(opam env)"
  make
EOF

RUN <<EOF 
  echo "Running tests..."
  eval "$(opam env)"
  make test-lp
EOF



