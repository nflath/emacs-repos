# Test syntactic recognition of awk top-level constructs.
/regexp/ {
}
#
/regexp/
/regexp/ ;
/regexp/                                        \

/regexp/                                        \
;
#
/regexp1/ &&
/regexp2/ {}
#
/regexp/ &&
(NF > 3) &&
# [In middle of pattern]
(NF < 5)
#
/regexp/
(NF == 3)
