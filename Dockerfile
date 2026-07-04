# K has binaries available or Ubuntu.
# Easier than Nix for quick tests
FROM ubuntu:24.04
# NOTE
RUN <<EOF 
  apt-get update
  apt-get upgrade --yes
  rm -rf /var/lib/apt/lists/*
EOF



