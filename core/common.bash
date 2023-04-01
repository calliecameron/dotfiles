# This file should be included with 'source', not executed directly

# Dir of the including script, not of this file (which would be BASH_SOURCE[0]).
# Doesn't work if this file is sourced while in a function (rather than a
# top-level script) -- but that should never be done anyway!
# shellcheck disable=SC2034
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[1]}")" && pwd)"
