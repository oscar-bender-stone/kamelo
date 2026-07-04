# K has binaries available or Ubuntu.
# Easier than Nix for quick tests
FROM docker.io/ocaml/opam:ubuntu-24.04-opam-4.13
ARG OCAML_VERSION="4.13.1"
# TODO: determine if versions of system libraries should be pinned. Not mentioned in the original CI script. 
RUN <<EOF 
  echo "Upgrading base system..."
  apt-get update
  apt-get upgrade --yes
  rm -rf /var/lib/apt/lists/*

  echo "Setting up opam..." 
  if [ ! -d _opam ]; then
    echo "No local switch detected. Creating new switch."
    opam switch create --yes --no-install . ${OCAML_VERSION}
  fi
  opam update
  eval "$(opam env)"

  echo "Installing system dependencies"
  # Alt-ergo and wget
  apt-get install --yes wget libgmp-dev m4 pkg-config autoconf zlib1g-dev --no-install-recommends

  echo "Installing Lambdapi"
  opam install --yes dune bindlib timed sedlex menhir pratter yojson cmdliner why3 alcotest alt-ergo odoc
  why3 config detect
  opam install --yes lambdapi.2.2.1

EOF



