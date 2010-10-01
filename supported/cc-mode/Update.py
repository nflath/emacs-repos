#! /usr/bin/env python

"""ChangeLog updater.  X/Emacs's rcs2log is just f*cking broken.

Rather than try to fix the brokenness in rcs2log and vc-update-change-log,
this Python script will get you 90%% of the way toward converting the CVS logs
to ChangeLog format.  I took a few liberties with the ChangeLog format, namely
eliminating the time field.

The output of this script will still have to be munged somewhat by hand.
Paragraphs will have to be wrapped manually, and similar file changes will
have to be collapsed.  That's not a huge penalty because the CVS logs always
have to be, er, sanitized (proofread) before sending to RMS.

This script does properly ignore `#' leading log messages.

Usage: %(PROGRAM)s [-d] [-h]

Where:

    -d
    --date
        print the log-from date only.  do not generate change log output

    -h
    --help
        print this help message

"""

import sys
import os
import string
import re
import time
import getopt
import pwd

PROGRAM = sys.argv[0]



def usage(status):
    print __doc__ % globals()
    sys.exit(status)



date_re = re.compile(
    '^'
    '(?P<year>\d{2,4})-'
    '(?P<mon>\d{1,2})-'
    '(?P<day>\d{1,2})'
    )


def get_last_date(filename):
    fp = open(filename)
    while 1:
	line = fp.readline()
	if not line:
	    break
	mo = date_re.match(line)
	if mo:
	    fp.close()
	    return mo.group('year', 'mon', 'day')
    return None



filere = re.compile('^Working file: (?P<file>.*)')
revre = re.compile('^revision \d.\d')
logdate_re = re.compile(
    '^date: (?P<year>\d{4})/(?P<mon>\d{2})/(?P<day>\d{2})\s+'
    '(?P<time>\d{2}:\d{2}:\d{2});\s+'
    'author: (?P<name>[^;]+);\s+'
    #state: Exp;
    )


def read_revision(fp):
    revision = fp.readline()
    dateline = fp.readline()
    mo = logdate_re.match(dateline)
    date = tuple(map(int, mo.group('year', 'mon', 'day')))
    # now collect change log entry
    logmsg = ''
    endof_file = 0
    nonempty_line = 0
    while 1:
	line = fp.readline()
	if not line or \
	   line[:28] == '----------------------------' or \
	   line[:28] == '============================':
	    break
	# look for `comment'
	if line[0] == '#':
	    continue
	if not nonempty_line:
	    if line[:-1] == '':
		continue
	    else:
		nonempty_line = 1
	logmsg = logmsg + line[:-1] + ' '
    if line[:28] == '============================':
	endof_file = 1
    return (endof_file, logmsg, date, mo.group('name'))


def get_log_entries(year, mon, day):
    cvscmd = 'cvs log -l -b -d">%s-%s-%s"' % (year, mon, day)
    fp = os.popen(cvscmd, 'r')

    changes = {}
    while 1:
	line = fp.readline()
	if not line:
	    break
	# the name of the file??
	mo = filere.match(line)
	if mo:
	    filename = mo.group('file')
	    continue
	if line[:28] == '----------------------------':
	    while 1:
		endof_file, logmsg, date, name = read_revision(fp)
		if logmsg:
		    todays_changes = changes.get(date, [])
		    todays_changes.append((logmsg, name, filename))
		    changes[date] = todays_changes
		if endof_file:
		    break
    # all done now
    fp.close()
    return changes



def group_by_user(changes):
    changes_bydate = {}
    for date in changes.keys():
	changes_byuser = {}
	for logmsg, name, filename in changes[date]:
	    byuser = changes_byuser.get(name, [])
	    byuser.append((filename, logmsg))
	    changes_byuser[name] = byuser
	changes_bydate[date] = changes_byuser
    return changes_bydate



# older dates come last
def datecmp(date1, date2):
    # compare years
    if date1[0] == date2[0]:
	# compare months
	if date1[1] == date2[1]:
	    # compare days
	    return cmp(date2[2], date1[2])
	else:
	    return cmp(date2[1], date1[1])
    else:
	return cmp(date2[0], date1[0])
    


def main():
    zeros = (0, 0, 0, 0, 0, 0)
    usernames = {}

    lastchange = get_last_date('ChangeLog')
    changes = apply(get_log_entries, lastchange)
    changes = group_by_user(changes)

    dates = changes.keys()
    dates.sort(datecmp)
    for date in dates:
	timetuple = time.localtime(time.mktime(tuple(date) + zeros))
	timestr = time.strftime('%Y-%m-%d', timetuple)

	changes_byuser = changes[date]
	for name in changes_byuser.keys():
	    longname = usernames.get(name, None)
	    if not longname:
		try:
		    longname = pwd.getpwnam(name)[4]
		except KeyError:
		    longname = name
		usernames[name] = longname
	    print '\n%s  %s  <bug-cc-mode@gnu.org>' % (timestr, longname)
	    for filename, logmsg in changes_byuser[name]:
		print '\n\t*', filename, logmsg


if __name__ == '__main__':
    try:
	opts, args = getopt.getopt(sys.argv[1:], 'dh', ['date', 'help'])
    except getopt.error, msg:
	print msg
	usage(1)

    if args:
	usage(1)

    # option defaults
    getdate_only = 0

    for opt, arg in opts:
	if opt in ('-h', '--help'):
	    usage(0)
	elif opt in ('-d', '--date'):
	    getdate_only = 1

    if getdate_only:
	datetuple = get_last_date('ChangeLog')
	if datetuple is None:
	    print "Couldn't find a date stamp in ChangeLog"
	else:
	    print 'Generating entries since:', \
		  string.join(datetuple, '-')
    else:
	main()
