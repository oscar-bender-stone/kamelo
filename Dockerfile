# K has binaries available or Ubuntu.
# Easier than Nix for quick tests
FROM docker.io/ocaml/opam:ubuntu-22.04-ocaml-4.13
ARG OCAML_VERSION="4.13.1"
ARG K_VERSION="5.5.53"
WORKDIR /kamelo
COPY . .

# TODO: determine if versions of system libraries should be pinned. Not mentioned in the original CI script. 
USER root
RUN <<EOF 
  echo "Upgrading base system..."
  apt-get update
  apt-get upgrade --yes
  echo "Installing system dependencies"
  # wget, make, and alt-ergo
  apt-get install --yes wget make libgmp-dev m4 pkg-config autoconf zlib1g-dev --no-install-recommends
  rm -rf /var/lib/apt/lists/*
EOF

USER opam
RUN <<EOF
  echo "Setting up opam..." 
  if [ ! -d _opam ]; then
    echo "No local switch detected. Creating new switch."
    opam switch create --yes kamelo ${OCAML_VERSION}
  fi
  opam update
EOF

RUN <<EOF
  echo "Installing Lambdapi..."
  opam install --yes --switch kamelo dune bindlib timed sedlex menhir pratter ezjsonm yaml yojson cmdliner why3 alcotest alt-ergo odoc
  opam exec --switch=kamelo -- why3 config detect
  opam install --yes --switch=kamelo lambdapi.2.2.1
EOF

RUN <<EOF
  echo "Installing K Framework..."
  wget --quiet https://github.com/runtimeverification/k/releases/download/v"${K_VERSION}"/K.Framework.Ubuntu.Jammy.22.04.Deb
EOF

RUN <<EOF
  echo "Building KaMeLo..."
  opam exec --switch=kamleo -- make
EOF

RUN <<EOF 
  echo "Running tests..."
  make test-lsp
EOF



