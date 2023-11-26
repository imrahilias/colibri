#!/bin/sh

# Note, ranger uses return codes for different semantics so we append ||
# true to enable caching in lf. Also, arguments ${4} and ${5} are used
# for image previews in ranger which does not work directly in lf so
# they are left empty. See also Previews wiki page for image previews in
# lf.

"/opt/sw/vsc4/VSC/x86_64/generic/lf/scope.sh" "${1}" "${2}" "${3}" "" "" || true
