# shellcheck shell=bats
# bats file_tags=slow

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="$(readlink -f "${THIS_DIR}/../../packages/misc/bin")"

    TMP_DIR="$(mktemp -d)"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'

    cd "${TMP_DIR}" || exit 1
}

teardown() {
    rm -rf "${TMP_DIR}"
}

run_script() {
    run env -i -C "${TMP_DIR}" HOME="${TMP_DIR}" "PATH=${BIN_DIR}:${PATH}" "${@}"
}

@test 'mvz usage' {
    run_script "${BIN_DIR}/mvz"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvz success' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'foo' >"${TMP_DIR}zb"
    run_script "${BIN_DIR}/mvz" "${TMP_DIR}/a" "${TMP_DIR}/b" "${TMP_DIR}/c"
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/c" ]
    assert [ -f "${TMP_DIR}/za" ]
    assert [ "$(cat "${TMP_DIR}/za")" = 'foo' ]
    assert [ -f "${TMP_DIR}/zb" ]
    assert [ "$(cat "${TMP_DIR}/zb")" = 'bar' ]
    assert [ -f "${TMP_DIR}/zc" ]
    assert [ "$(cat "${TMP_DIR}/zc")" = 'baz' ]
}

@test 'cpz usage' {
    run_script "${BIN_DIR}/cpz"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cpz success' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'foo' >"${TMP_DIR}zb"
    run_script "${BIN_DIR}/cpz" "${TMP_DIR}/a" "${TMP_DIR}/b" "${TMP_DIR}/c"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'bar' ]
    assert [ -f "${TMP_DIR}/c" ]
    assert [ "$(cat "${TMP_DIR}/c")" = 'baz' ]
    assert [ -f "${TMP_DIR}/za" ]
    assert [ "$(cat "${TMP_DIR}/za")" = 'foo' ]
    assert [ -f "${TMP_DIR}/zb" ]
    assert [ "$(cat "${TMP_DIR}/zb")" = 'bar' ]
    assert [ -f "${TMP_DIR}/zc" ]
    assert [ "$(cat "${TMP_DIR}/zc")" = 'baz' ]
}

@test 'swap no args' {
    run_script "${BIN_DIR}/swap"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'swap one arg' {
    run_script "${BIN_DIR}/swap" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'swap success' {
    echo 'foo' >"${TMP_DIR}/a"
    echo 'bar' >"${TMP_DIR}/b"
    run_script "${BIN_DIR}/swap" "${TMP_DIR}/a" "${TMP_DIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'bar' ]
    assert [ -f "${TMP_DIR}/b" ]
    assert [ "$(cat "${TMP_DIR}/b")" = 'foo' ]
}

@test 'change-file-ext no args' {
    run_script "${BIN_DIR}/change-file-ext"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'change-file-ext one arg' {
    run_script "${BIN_DIR}/change-file-ext" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'change-file-ext success' {
    echo 'foo' >"${TMP_DIR}/a.txt"
    echo 'bar' >"${TMP_DIR}/b.txt"
    echo 'baz' >"${TMP_DIR}/c.foo"
    echo 'quux' >"${TMP_DIR}/b.txt2"
    run_script "${BIN_DIR}/change-file-ext" 'txt' 'txt2'
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${TMP_DIR}/a.txt" ]
    assert [ ! -e "${TMP_DIR}/b.txt}" ]
    assert [ -f "${TMP_DIR}/c.foo" ]
    assert [ "$(cat "${TMP_DIR}/c.foo")" = 'baz' ]
    assert [ -f "${TMP_DIR}/a.txt2" ]
    assert [ "$(cat "${TMP_DIR}/a.txt2")" = 'foo' ]
    assert [ -f "${TMP_DIR}/b.txt2" ]
    assert [ "$(cat "${TMP_DIR}/b.txt2")" = 'bar' ]
}

@test 'mvlist no args' {
    run_script "${BIN_DIR}/mvlist"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist one arg' {
    run_script "${BIN_DIR}/mvlist" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist bad file' {
    run_script "${BIN_DIR}/mvlist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist bad dir' {
    touch "${TMP_DIR}/list"
    run_script "${BIN_DIR}/mvlist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist success' {
    cat >"${TMP_DIR}/list" <<EOF
a
b
c
EOF
    mkdir -p "${TMP_DIR}/dir"
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'bar' >"${TMP_DIR}/dir/b"
    echo 'quux' >"${TMP_DIR}/dir/c"
    run_script "${BIN_DIR}/mvlist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/list" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/c" ]
    assert [ -d "${TMP_DIR}/dir" ]
    assert [ -f "${TMP_DIR}/dir/a" ]
    assert [ "$(cat "${TMP_DIR}/dir/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/dir/b" ]
    assert [ "$(cat "${TMP_DIR}/dir/b")" = 'bar' ]
    assert [ -f "${TMP_DIR}/dir/c" ]
    assert [ "$(cat "${TMP_DIR}/dir/c")" = 'baz' ]
}

@test 'cplist no args' {
    run_script "${BIN_DIR}/cplist"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist one arg' {
    run_script "${BIN_DIR}/cplist" 'a'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist bad file' {
    run_script "${BIN_DIR}/cplist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist bad dir' {
    touch "${TMP_DIR}/list"
    run_script "${BIN_DIR}/cplist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist success' {
    cat >"${TMP_DIR}/list" <<EOF
a
b
c
EOF
    mkdir -p "${TMP_DIR}/dir"
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'bar' >"${TMP_DIR}/dir/b"
    echo 'quux' >"${TMP_DIR}/dir/c"
    run_script "${BIN_DIR}/cplist" "${TMP_DIR}/list" "${TMP_DIR}/dir"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/list" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/c" ]
    assert [ "$(cat "${TMP_DIR}/c")" = 'baz' ]
    assert [ -d "${TMP_DIR}/dir" ]
    assert [ -f "${TMP_DIR}/dir/a" ]
    assert [ "$(cat "${TMP_DIR}/dir/a")" = 'foo' ]
    assert [ -f "${TMP_DIR}/dir/b" ]
    assert [ "$(cat "${TMP_DIR}/dir/b")" = 'bar' ]
    assert [ -f "${TMP_DIR}/dir/c" ]
    assert [ "$(cat "${TMP_DIR}/dir/c")" = 'baz' ]
}

@test 'rmlist no args' {
    run_script "${BIN_DIR}/rmlist"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'rmlist bad file' {
    run_script "${BIN_DIR}/rmlist" "${TMP_DIR}/list"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'rmlist success' {
    cat >"${TMP_DIR}/list" <<EOF
a
b
c
EOF
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    run_script "${BIN_DIR}/rmlist" "${TMP_DIR}/list"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/list" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/c" ]
}

@test 'mvlists no args' {
    run_script "${BIN_DIR}/mvlists"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists one arg' {
    run_script "${BIN_DIR}/mvlists" 'src'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists bad src' {
    run_script "${BIN_DIR}/mvlists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists bad dest' {
    touch "${TMP_DIR}/src"
    run_script "${BIN_DIR}/mvlists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists lengths differ' {
    cat >"${TMP_DIR}/src" <<EOF
a
b
c
EOF
    cat >"${TMP_DIR}/dest" <<EOF
d
e
EOF
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'quux' >"${TMP_DIR}/d"
    echo 'bar' >"${TMP_DIR}/e"
    run_script "${BIN_DIR}/mvlists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/src" ]
    assert [ -f "${TMP_DIR}/dest" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/c" ]
    assert [ "$(cat "${TMP_DIR}/c")" = 'baz' ]
    assert [ -f "${TMP_DIR}/d" ]
    assert [ "$(cat "${TMP_DIR}/d")" = 'quux' ]
    assert [ -f "${TMP_DIR}/e" ]
    assert [ "$(cat "${TMP_DIR}/e")" = 'bar' ]
}

@test 'mvlists success' {
    cat >"${TMP_DIR}/src" <<EOF
a
b
c
EOF
    cat >"${TMP_DIR}/dest" <<EOF
d
e
f
EOF
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'quux' >"${TMP_DIR}/d"
    echo 'bar' >"${TMP_DIR}/e"
    run_script "${BIN_DIR}/mvlists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TMP_DIR}/src" ]
    assert [ -f "${TMP_DIR}/dest" ]
    assert [ ! -e "${TMP_DIR}/a" ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ ! -e "${TMP_DIR}/c" ]
    assert [ -f "${TMP_DIR}/d" ]
    assert [ "$(cat "${TMP_DIR}/d")" = 'foo' ]
    assert [ -f "${TMP_DIR}/e" ]
    assert [ "$(cat "${TMP_DIR}/e")" = 'bar' ]
    assert [ -f "${TMP_DIR}/f" ]
    assert [ "$(cat "${TMP_DIR}/f")" = 'baz' ]
}

@test 'cplists no args' {
    run_script "${BIN_DIR}/cplists"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists one arg' {
    run_script "${BIN_DIR}/cplists" 'src'
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists bad src' {
    run_script "${BIN_DIR}/cplists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists bad dest' {
    touch "${TMP_DIR}/src"
    run_script "${BIN_DIR}/cplists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists lengths differ' {
    cat >"${TMP_DIR}/src" <<EOF
a
b
c
EOF
    cat >"${TMP_DIR}/dest" <<EOF
d
e
EOF
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'quux' >"${TMP_DIR}/d"
    echo 'bar' >"${TMP_DIR}/e"
    run_script "${BIN_DIR}/cplists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${TMP_DIR}/src" ]
    assert [ -f "${TMP_DIR}/dest" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/c" ]
    assert [ "$(cat "${TMP_DIR}/c")" = 'baz' ]
    assert [ -f "${TMP_DIR}/d" ]
    assert [ "$(cat "${TMP_DIR}/d")" = 'quux' ]
    assert [ -f "${TMP_DIR}/e" ]
    assert [ "$(cat "${TMP_DIR}/e")" = 'bar' ]
}

@test 'cplists success' {
    cat >"${TMP_DIR}/src" <<EOF
a
b
c
EOF
    cat >"${TMP_DIR}/dest" <<EOF
d
e
f
EOF
    echo 'foo' >"${TMP_DIR}/a"
    echo 'baz' >"${TMP_DIR}/c"
    echo 'quux' >"${TMP_DIR}/d"
    echo 'bar' >"${TMP_DIR}/e"
    run_script "${BIN_DIR}/cplists" "${TMP_DIR}/src" "${TMP_DIR}/dest"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${TMP_DIR}/src" ]
    assert [ -f "${TMP_DIR}/dest" ]
    assert [ -f "${TMP_DIR}/a" ]
    assert [ "$(cat "${TMP_DIR}/a")" = 'foo' ]
    assert [ ! -e "${TMP_DIR}/b" ]
    assert [ -f "${TMP_DIR}/c" ]
    assert [ "$(cat "${TMP_DIR}/c")" = 'baz' ]
    assert [ -f "${TMP_DIR}/d" ]
    assert [ "$(cat "${TMP_DIR}/d")" = 'foo' ]
    assert [ -f "${TMP_DIR}/e" ]
    assert [ "$(cat "${TMP_DIR}/e")" = 'bar' ]
    assert [ -f "${TMP_DIR}/f" ]
    assert [ "$(cat "${TMP_DIR}/f")" = 'baz' ]
}

@test 'dirsummary bad dir' {
    run_script "${BIN_DIR}/dirsummary" "${TMP_DIR}/dir"
    assert_failure
    assert_line --partial 'is not a directory'
}

@test 'dirsummary default' {
    mkdir -p "${TMP_DIR}/dir"
    echo 'foo' >"${TMP_DIR}/a.txt"
    echo 'bar' >"${TMP_DIR}/dir/a.txt"
    echo 'baz' >"${TMP_DIR}/dir/b.txt"
    echo 'quux' >"${TMP_DIR}/dir/a.txt2"
    echo 'blah' >"${TMP_DIR}/dir/b"
    echo 'yay' >"${TMP_DIR}/dir/Makefile"
    echo 'stuff' >"${TMP_DIR}/dir/makefile"
    run_script "${BIN_DIR}/dirsummary"
    assert_success
    assert_output "No extension: 1 file(s), 5.0 B
Makefile: 2 file(s), 10.0 B
txt: 3 file(s), 12.0 B
txt2: 1 file(s), 5.0 B"
}

@test 'dirsummary dir' {
    mkdir -p "${TMP_DIR}/dir"
    echo 'foo' >"${TMP_DIR}/a.txt"
    echo 'bar' >"${TMP_DIR}/dir/a.txt"
    echo 'baz' >"${TMP_DIR}/dir/b.txt"
    echo 'quux' >"${TMP_DIR}/dir/a.txt2"
    echo 'blah' >"${TMP_DIR}/dir/b"
    echo 'yay' >"${TMP_DIR}/dir/Makefile"
    echo 'stuff' >"${TMP_DIR}/dir/makefile"
    run_script "${BIN_DIR}/dirsummary" "${TMP_DIR}/dir"
    assert_success
    assert_output "No extension: 1 file(s), 5.0 B
Makefile: 2 file(s), 10.0 B
txt: 2 file(s), 8.0 B
txt2: 1 file(s), 5.0 B"
}
