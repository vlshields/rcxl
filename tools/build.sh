#!/bin/sh
# Build and install rcxl into tools/lib for benchmarking/testing.
set -e
cd "$(dirname "$0")/.."
mkdir -p tools/lib
find src -name '*.o' -delete; rm -f src/*.so
R CMD INSTALL --library=tools/lib --no-byte-compile . >/dev/null 2>&1 || \
  R CMD INSTALL --library=tools/lib --no-byte-compile .
echo "installed rcxl into tools/lib"
