# K has binaries available or Ubuntu.
# Easier than Nix for quick tests
FROM docker.io/ocaml/opam:ubuntu-24.04-opam-4.13
ARG OCAML_VERSION="4.13.1"
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

EOF



