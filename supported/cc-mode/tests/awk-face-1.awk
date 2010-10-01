# Test (most of) awk-font-lock-keywords.
# Function names
func foo(bar) {}
 func foo(bar) {}
func foo (bar) {}
function foo (bar) {}
function
func
# Built-in variables
{print ARGC ARGIND ARGV BINMODE CONVFMT ENVIRON ERRNO \
        FIELDWIDTHS FILENAME FNR FS IGNORECASE LINT NF  \
        NR OFMT OFS ORS PROCINFO RLENGTH RS RSTART RT   \
        SUBSEP TEXTDOMAIN argc ARGCC ARGC_ ARGC+1}
# Special file names (1)
{print "foo" > "/dev/stderr"}
{print "foo" > "/dev/stderr "}
{print "foo" > "/dev/stderr one"}
{print "foo" > "dev/stderr"}
{print "foo" > "/dev/stderr1"}
{print "foo" > "/dev/stderr/"}
{print "foo" > "/dev/stderr%"}
{print "foo" > "/dev/stderr
}
{print "foo" > "/dev/stderr one
}
{print "foo" > "dev/stderr
}
{print "foo" > "/dev/stderr1
}
{print "foo" > "/dev/stderr/
}
{print "foo" > "/dev/stderr%
}
{print "foo" > "/dev/stdout"}
{getline < "/dev/stdin"}
{print "foo" > "/dev/fd/0"}
{print "foo" > "/dev/fd/3"}
{print "foo" > "/dev/fd/0123456789"}
{print "foo" > "/dev/fd"}
{print "foo" > "/dev/fd/"}
{print "foo" > "/dev/fd/n"}
{getline < "/dev/pid"}
{getline < "/dev/ppid"}
{getline < "/dev/pgrpid"}
{getline < "/dev/user"}
# Special File Names (2)
{print "foo" > "/inet/tcp/lport/rhost/rport"}
{print "foo" > "/inet/tcp/lport/rhost/rport "}
{print "foo" > "/inet/tcp/lport/rhost/rport one"}
{print "foo" > "inet/tcp/lport/rhost/rport"}
{print "foo" > "/inet/tcp/lport/rhost/rport1"}
{print "foo" > "/inet/tcp/lport/rhost/rport/"}
{print "foo" > "/inet/tcp/lport/rhost/rport%"}
{print "foo" > "/inet/tcp/lport/rhost/rport
}
{print "foo" > "/inet/tcp/lport/rhost/rport one
}
{print "foo" > "inet/tcp/lport/rhost/rport
}
{print "foo" > "/inet/tcp/lport/rhost/rport1
}
{print "foo" > "/inet/tcp/lport/rhost/rport/
}
{print "foo" > "/inet/tcp/lport/rhost/rport%
}
{print "foo" > "/inet/udp/lport/rhost/rport"}
{print "foo" > "/inet/raw/lport/rhost/rport"}
# Keywords.
BEGIN {
    while (a in b)
             if (a)
                 delete b[a]
             else
                 getline
    for (a in b) {
        if (a = -1) break
        if (a = 1) continue
    }
}
function foo (   bar) {
    return bar
}
/^$/ {
    if (bar > 0)
        next
    else if (bar < 0)
        nextfile
    else
        exit
}
END {
    do foo()
    while (bar)
}
# Builtins
{
    adump (a)
    b = and ($1, $2)
    b = and($1, $2)
    n = asort (a)
    f = atan2 ($1)
    bindtextdomain ("~/mybind")
    close ("bar")
    n = compl ($1)
    f = cos ($1)
    s = dcgettext ("Hello")
    f = exp ($1)
    extension ("./filefuncs.so", "dlload")
    fflush ("bar")
    s = gensub ("awk", "AWK", "g")
    n = gsub ("awk", "AWK", $1)
    n = index ($0, "AWK")
    n = int ($1)
    n = length ($1)
    f = log ($1)
    n = lshift ($1, 2)
    n = match (s, "[Aa]wk", a)
    s = mktime ("2003 02 20 19 00 00")
    b = or ($1, $2)
    print $1
    print ($1)
    printf ("%2.0d", $1)
    f = rand ()
    n = rshift ($1, 2)
    f = sin ($1)
    n = split ($1, a, "\t")
    s = sprintf ("%2.0d", $1)
    f = sqrt ($1)
    seed = srand ()
    stopme ()
    s = strftime("%Y %m %d %H %M %S 00")
    n = strtonum ("0xDE")
    n = sub ("awk", "AWK", $1)
    s = substr ($0, 5, 10)
    n = system ("echo BOO")
    n = systime ()
    s = tolower ($1)
    s = toupper ($1)
    n = xor ($1, 255)
}
# Calling user defined functions with spurious space between "foo" and "("
{
    foo(bar)
    foo (bar)
    foo	(bar)
    foo\
(bar)
    foo\
\
\
(bar)
    foo \
(bar)
    foo \

    (bar)
    foo\
 \
\
(bar)
}

# Space after \ in what looks line an escaped newline.

/pattern/ \
{
    action
}
/pattern/ \ 
{
    action
}
/pattern/ \	
{
    action
}
# Unbalanced "string" /regex/ delimimiters is too complicated for this file.
# gawk 3.1 localizable strings
{
    print _"Hello"
    print _"Hello
} 

# A "character class" regular expression.
/[^[:alpha:]/]/ {print}

# Local Variables:
# cc-test-skip: (no-syntax-properties)
# End:
