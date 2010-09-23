#! /usr/bin/env python

"""Manage releases of CC Mode.

Usage: %(program)s [-b] [-t|-T] [-p] [-d] [-h] revnum

Where:

    --bump
    -b
            set version number of versioned files to minor revision number

    --tag 
    -t
            tag all release files with minor revision number

    --TAG
    -T
            like --tag, but relocates any existing tag.  See
	    `cvs tag -F'.  Only one of --tag or --TAG can be given on the
	    command line.

    --help
    -h      - this help message

    revnum is required and is the minor revision number of this release
    (e.g. for release 5.20, revnum would be `20').

"""

import sys
import os
import string
import re
import getopt

program = sys.argv[0]

def usage(status):
    print __doc__ % globals()
    sys.exit(status)


RELEASE = None
RELEASE_NAME = ''

# for bumping
VERSIONED_FILES = [
    # file, prefix -- not grouped! trailing space is significant
    ('ANNOUNCEMENT', 'CC Mode Version '),
    ('MANIFEST', 'Manifest for CC Mode '),
    ('README', 'README for CC Mode '),
    ('cc-defs.el', r'\(defconst c-version "'),
    ('cc-mode.texi', r'(@center @titlefont\{CC Mode|version) '),
    ]



def tag_release(revnum, retag):
    # first verify that the ChangeLog is up-to-date
    fp = open("ChangeLog")
    cre = re.compile('^[ \t]*[*] Release 5.(?P<rev>[0-9]{2})')
    while 1:
	line = fp.readline()
	if line == '':
	    print '*****WARNING*****'
	    print 'Could not find a Release tag in the ChangeLog!'
	    sys.exit(1)
	mo = cre.match(line)
	if mo:
	    docorev = mo.group('rev')
	    if docorev <> revnum:
		print '*****WARNING*****'
		print 'ChangeLog has not been updated... exiting!'
		print 'Found tag ' + docorev
		sys.exit(1)
	    break
    fp.close()
    relname = '"Release_5_' + revnum + '"'
    cvscmd = 'cvs tag'
    option = ''
    if retag:
	option = '-F'
    os.system('%s %s %s' % (cvscmd, option, relname))
    relname = '"Branch_5_' + revnum + '"'
    cvscmd = 'cvs tag -b '
    os.system('%s %s %s' % (cvscmd, option, relname))



def bump_release(revnum):
    compiled = {}
    for f, prefix in VERSIONED_FILES:
        print '%s:' % f,
	cre = re.compile('^(?P<prefix>' +
			 prefix +
			 ')5.(?P<rev>[0-9]{2})(?P<suffix>.*)$')
	print 'checking...',
	fp = open(f, 'r')
	while 1:
	    line = fp.readline()
	    if not line:
                if not compiled.has_key(f):
                    print 'no matching version line.'
                break
	    mo = cre.match(line)
	    if mo:
		if int(mo.group('rev')) <> int(revnum) - 1:
		    continue
		else:
                    compiled[f] = cre
	fp.close()
    # now bump them
    for f, cre in compiled.items():
	cre = compiled[f]
	print 'bumping.',
	fp_in = open(f, 'r')
	fp_out = open(f + '.new', 'w')
        matched = 0
	while 1:
	    line = fp_in.readline()
	    if not line:
		break
            mo = cre.match(line)
            if mo:
                prefix, suffix = mo.group('prefix', 'suffix')
                line = '%s5.%s%s\n' % (prefix, revnum, suffix)
                matched = matched + 1
	    fp_out.write(line)
	fp_in.close()
	fp_out.close()
	os.rename(f + '.new', f)
        print matched, 'lines changed.'


def main():
    try:
	opts, args = getopt.getopt(
	    sys.argv[1:],
	    'btTh',
	    ['bump', 'tag', 'TAG', 'help'])
    except getopt.error, msg:
	print msg
	usage(1)

    # required minor rev number
    if len(args) <> 1:
	print 'Minor revision number is required'
	usage(1)
    revnum = args[0]

    # default options
    tag = 0
    retag = 0
    help = 0
    bump = 0

    for opt, arg in opts:
	if opt in ('-h', '--help'):
	    help = 1
	elif opt in ('-b', '--bump'):
	    bump = 1
	elif opt in ('-t', '--tag'):
	    tag = 1
	elif opt in ('-T', '--TAG'):
	    tag = 1
	    retag = 1

    if help:
	usage(0)

    # very important!!!
    os.umask(002)
    if tag:
	tag_release(revnum, retag)

    if bump:
	bump_release(revnum)


if __name__ == '__main__':
    main()
