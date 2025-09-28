# shellcheck shell=bats
# bats file_tags=slow

setup() {
    THIS_DIR="$(cd "$(dirname "${BATS_TEST_FILENAME}")" && pwd)"
    BIN_DIR="$(readlink -f "${THIS_DIR}/../../packages/misc/bin")"

    bats_load_library 'bats-support'
    bats_load_library 'bats-assert'

    cd "${BATS_TEST_TMPDIR}" || exit 1
}

run_script() {
    run env -i -C "${BATS_TEST_TMPDIR}" HOME="${BATS_TEST_TMPDIR}" "PATH=${BIN_DIR}:${PATH}" "${@}"
}

@test 'mvz usage' {
    run_script "${BIN_DIR}/mvz"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvz success' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'foo' >"${BATS_TEST_TMPDIR}zb"
    run_script "${BIN_DIR}/mvz" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b" "${BATS_TEST_TMPDIR}/c"
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/c" ]
    assert [ -f "${BATS_TEST_TMPDIR}/za" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/za")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/zb" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/zb")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/zc" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/zc")" = 'baz' ]
}

@test 'cpz usage' {
    run_script "${BIN_DIR}/cpz"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cpz success' {
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'foo' >"${BATS_TEST_TMPDIR}zb"
    run_script "${BIN_DIR}/cpz" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b" "${BATS_TEST_TMPDIR}/c"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c")" = 'baz' ]
    assert [ -f "${BATS_TEST_TMPDIR}/za" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/za")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/zb" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/zb")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/zc" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/zc")" = 'baz' ]
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
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b"
    run_script "${BIN_DIR}/swap" "${BATS_TEST_TMPDIR}/a" "${BATS_TEST_TMPDIR}/b"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b")" = 'foo' ]
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
    echo 'foo' >"${BATS_TEST_TMPDIR}/a.txt"
    echo 'bar' >"${BATS_TEST_TMPDIR}/b.txt"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c.foo"
    echo 'quux' >"${BATS_TEST_TMPDIR}/b.txt2"
    run_script "${BIN_DIR}/change-file-ext" 'txt' 'txt2'
    assert_success
    refute_output --partial 'Usage:'
    assert [ ! -e "${BATS_TEST_TMPDIR}/a.txt" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b.txt}" ]
    assert [ -f "${BATS_TEST_TMPDIR}/c.foo" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c.foo")" = 'baz' ]
    assert [ -f "${BATS_TEST_TMPDIR}/a.txt2" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a.txt2")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/b.txt2" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/b.txt2")" = 'bar' ]
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
    run_script "${BIN_DIR}/mvlist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist bad dir' {
    touch "${BATS_TEST_TMPDIR}/list"
    run_script "${BIN_DIR}/mvlist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlist success' {
    cat >"${BATS_TEST_TMPDIR}/list" <<EOF
a
b
c
EOF
    mkdir -p "${BATS_TEST_TMPDIR}/dir"
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'bar' >"${BATS_TEST_TMPDIR}/dir/b"
    echo 'quux' >"${BATS_TEST_TMPDIR}/dir/c"
    run_script "${BIN_DIR}/mvlist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/list" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/c" ]
    assert [ -d "${BATS_TEST_TMPDIR}/dir" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/b")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/c")" = 'baz' ]
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
    run_script "${BIN_DIR}/cplist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist bad dir' {
    touch "${BATS_TEST_TMPDIR}/list"
    run_script "${BIN_DIR}/cplist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplist success' {
    cat >"${BATS_TEST_TMPDIR}/list" <<EOF
a
b
c
EOF
    mkdir -p "${BATS_TEST_TMPDIR}/dir"
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'bar' >"${BATS_TEST_TMPDIR}/dir/b"
    echo 'quux' >"${BATS_TEST_TMPDIR}/dir/c"
    run_script "${BIN_DIR}/cplist" "${BATS_TEST_TMPDIR}/list" "${BATS_TEST_TMPDIR}/dir"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/list" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c")" = 'baz' ]
    assert [ -d "${BATS_TEST_TMPDIR}/dir" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/a")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/b" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/b")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/dir/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/dir/c")" = 'baz' ]
}

@test 'rmlist no args' {
    run_script "${BIN_DIR}/rmlist"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'rmlist bad file' {
    run_script "${BIN_DIR}/rmlist" "${BATS_TEST_TMPDIR}/list"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'rmlist success' {
    cat >"${BATS_TEST_TMPDIR}/list" <<EOF
a
b
c
EOF
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    run_script "${BIN_DIR}/rmlist" "${BATS_TEST_TMPDIR}/list"
    assert_success
    refute_line --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/list" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/c" ]
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
    run_script "${BIN_DIR}/mvlists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists bad dest' {
    touch "${BATS_TEST_TMPDIR}/src"
    run_script "${BIN_DIR}/mvlists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'mvlists lengths differ' {
    cat >"${BATS_TEST_TMPDIR}/src" <<EOF
a
b
c
EOF
    cat >"${BATS_TEST_TMPDIR}/dest" <<EOF
d
e
EOF
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'quux' >"${BATS_TEST_TMPDIR}/d"
    echo 'bar' >"${BATS_TEST_TMPDIR}/e"
    run_script "${BIN_DIR}/mvlists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/src" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dest" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c")" = 'baz' ]
    assert [ -f "${BATS_TEST_TMPDIR}/d" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/d")" = 'quux' ]
    assert [ -f "${BATS_TEST_TMPDIR}/e" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/e")" = 'bar' ]
}

@test 'mvlists success' {
    cat >"${BATS_TEST_TMPDIR}/src" <<EOF
a
b
c
EOF
    cat >"${BATS_TEST_TMPDIR}/dest" <<EOF
d
e
f
EOF
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'quux' >"${BATS_TEST_TMPDIR}/d"
    echo 'bar' >"${BATS_TEST_TMPDIR}/e"
    run_script "${BIN_DIR}/mvlists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/src" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dest" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/a" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/c" ]
    assert [ -f "${BATS_TEST_TMPDIR}/d" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/d")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/e" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/e")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/f" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/f")" = 'baz' ]
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
    run_script "${BIN_DIR}/cplists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists bad dest' {
    touch "${BATS_TEST_TMPDIR}/src"
    run_script "${BIN_DIR}/cplists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    assert_line --partial 'Usage:'
}

@test 'cplists lengths differ' {
    cat >"${BATS_TEST_TMPDIR}/src" <<EOF
a
b
c
EOF
    cat >"${BATS_TEST_TMPDIR}/dest" <<EOF
d
e
EOF
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'quux' >"${BATS_TEST_TMPDIR}/d"
    echo 'bar' >"${BATS_TEST_TMPDIR}/e"
    run_script "${BIN_DIR}/cplists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_failure
    refute_line --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/src" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dest" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c")" = 'baz' ]
    assert [ -f "${BATS_TEST_TMPDIR}/d" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/d")" = 'quux' ]
    assert [ -f "${BATS_TEST_TMPDIR}/e" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/e")" = 'bar' ]
}

@test 'cplists success' {
    cat >"${BATS_TEST_TMPDIR}/src" <<EOF
a
b
c
EOF
    cat >"${BATS_TEST_TMPDIR}/dest" <<EOF
d
e
f
EOF
    echo 'foo' >"${BATS_TEST_TMPDIR}/a"
    echo 'baz' >"${BATS_TEST_TMPDIR}/c"
    echo 'quux' >"${BATS_TEST_TMPDIR}/d"
    echo 'bar' >"${BATS_TEST_TMPDIR}/e"
    run_script "${BIN_DIR}/cplists" "${BATS_TEST_TMPDIR}/src" "${BATS_TEST_TMPDIR}/dest"
    assert_success
    refute_output --partial 'Usage:'
    assert [ -f "${BATS_TEST_TMPDIR}/src" ]
    assert [ -f "${BATS_TEST_TMPDIR}/dest" ]
    assert [ -f "${BATS_TEST_TMPDIR}/a" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/a")" = 'foo' ]
    assert [ ! -e "${BATS_TEST_TMPDIR}/b" ]
    assert [ -f "${BATS_TEST_TMPDIR}/c" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/c")" = 'baz' ]
    assert [ -f "${BATS_TEST_TMPDIR}/d" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/d")" = 'foo' ]
    assert [ -f "${BATS_TEST_TMPDIR}/e" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/e")" = 'bar' ]
    assert [ -f "${BATS_TEST_TMPDIR}/f" ]
    assert [ "$(cat "${BATS_TEST_TMPDIR}/f")" = 'baz' ]
}

@test 'dirsummary bad dir' {
    run_script "${BIN_DIR}/dirsummary" "${BATS_TEST_TMPDIR}/dir"
    assert_failure
    assert_line --partial 'is not a directory'
}

@test 'dirsummary default' {
    mkdir -p "${BATS_TEST_TMPDIR}/dir"
    echo 'foo' >"${BATS_TEST_TMPDIR}/a.txt"
    echo 'bar' >"${BATS_TEST_TMPDIR}/dir/a.txt"
    echo 'baz' >"${BATS_TEST_TMPDIR}/dir/b.txt"
    echo 'quux' >"${BATS_TEST_TMPDIR}/dir/a.txt2"
    echo 'blah' >"${BATS_TEST_TMPDIR}/dir/b"
    echo 'yay' >"${BATS_TEST_TMPDIR}/dir/Makefile"
    echo 'stuff' >"${BATS_TEST_TMPDIR}/dir/makefile"
    run_script "${BIN_DIR}/dirsummary"
    assert_success
    assert_output "No extension: 1 file(s), 5.0 B
Makefile: 2 file(s), 10.0 B
txt: 3 file(s), 12.0 B
txt2: 1 file(s), 5.0 B"
}

@test 'dirsummary dir' {
    mkdir -p "${BATS_TEST_TMPDIR}/dir"
    echo 'foo' >"${BATS_TEST_TMPDIR}/a.txt"
    echo 'bar' >"${BATS_TEST_TMPDIR}/dir/a.txt"
    echo 'baz' >"${BATS_TEST_TMPDIR}/dir/b.txt"
    echo 'quux' >"${BATS_TEST_TMPDIR}/dir/a.txt2"
    echo 'blah' >"${BATS_TEST_TMPDIR}/dir/b"
    echo 'yay' >"${BATS_TEST_TMPDIR}/dir/Makefile"
    echo 'stuff' >"${BATS_TEST_TMPDIR}/dir/makefile"
    run_script "${BIN_DIR}/dirsummary" "${BATS_TEST_TMPDIR}/dir"
    assert_success
    assert_output "No extension: 1 file(s), 5.0 B
Makefile: 2 file(s), 10.0 B
txt: 2 file(s), 8.0 B
txt2: 1 file(s), 5.0 B"
}
