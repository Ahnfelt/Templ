function check {
    c="../templ check $1.templ \"$2\""
    eval $c
    if [ $? == "1" ]
    then
        echo "ERROR: $c"; false
    else
        true
    fi
}

function checkError {
    c="../templ check $1.templ \"$2\""
    eval $c
    if [ $? == "1" ]
    then
        true
    else
        echo "NO ERROR: $c"; false
    fi
}

(echo "TESTING TYPE CHECKER..." &&
# Ensure basic type compatibility
check List "[*]" &&
check String "*" &&
check Record "(b: (c: [*]), a: *)" &&
check Record "(c: *, b: (c: [*]), a: *)" && # width
check Record "(b: (c: [*], d: *), a: *)" && # depth
# Check optionality subtyping
check Optional "(a: *, b: (x: *))" &&
check Optional "(a: *?, b: (x: *))" &&
check Optional "(b: (x: *))" &&
# Check various scripts that should work
check For "()" &&

echo "PROVOKING TYPE ERRORS..." &&
# Ensure basic type incompatibility
checkError String "()" 2> /dev/null &&
checkError String "[*]" 2> /dev/null &&
checkError List "*" 2> /dev/null &&
checkError List "()" 2> /dev/null &&
checkError List "[[*]]" 2> /dev/null &&
checkError Record "(a: *)" 2> /dev/null && # break width
checkError Record "(b: (c: [*]), a: [*])" 2> /dev/null && # break depth
# Check optionality subtyping
checkError Record "(b: (c: [*]), a: *?)" 2> /dev/null &&
checkError Optional "(b: (x: *), a: [*])" 2> /dev/null &&
# Check various scripts that contian type errors internally
checkError FieldMissing "()" 2> /dev/null &&
checkError ForProblem "()" 2> /dev/null &&
checkError BrokenOptional "()" 2> /dev/null &&

echo "RUNNING EXAMPLES..." &&
echo "(no examples provided yet)" &&

echo "NO PROBLEMS FOUND") ||
echo "SOME TEST FAILED"

