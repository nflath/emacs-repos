# Test indentation of lines following autoincrement.
/regexp/ {
    h++
    print "foo"
    print "bar"
}
