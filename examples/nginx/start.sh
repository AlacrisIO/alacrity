#!/bin/sh -eu
HERE=$(dirname "$0")
cd "$HERE/../.."
TOP=$(pwd)
mkdir -p _run/logs _run/nginx-tmp
cd _run

# NB: Use sudo to run as root, otherwise run as regular user.
# Check file permissions if you mix the two
nginx -p "$TOP"/_run -c "$TOP"/examples/nginx/nginx.conf
