"""
requires Python Image Library - http://www.pythonware.com/products/pil/
requires grep and diff - http://www.mingw.org/msys.shtml
requires command line cvs - http://tortoisecvs.sourceforge.net/

example release.cfg:
=========================
[auth]
USER=kichik
WIKI_PASSWORD=xxx

[version]
VERSION=2.10
VER_MAJOR=2
VER_MINOR=1
VER_REVISION=0
VER_BUILD=0

[cvs]
CVS="C:\Program Files\TortoiseCVS\cvs.exe"
CVS_EXT="C:\Program Files\PuTTY\plink.exe" -2 -l "%u" "%h"

[compression]
TAR_BZ2=7zatarbz2.bat %s %s
ZIP="C:\Program Files\7-zip\7za.exe" a -tzip %s -mx9 -mfb=255 -mpass=4 %s

[rsh]
RSH="C:\Program Files\PuTTY\plink.exe" -2 -l kichik nsis.sourceforge.net

[wiki]
PURGE_URL=http://nsis.sourceforge.net/%s?action=purge
UPDATE_URL=http://nsis.sourceforge.net/Special:Simpleupdate?action=raw
=========================

7zatarbz2.bat:
=========================
@echo off
"C:\Program Files\7-zip\7za.exe" a -ttar %2.tar -r %2
"C:\Program Files\7-zip\7za.exe" a -tbzip2 %1 -mx9 -mpass=7 %2.tar
=========================

TODO
~~~~

 * Create release on SourceForge automatically
 * Edit update.php
 * http://en.wikipedia.org/w/index.php?title=Nullsoft_Scriptable_Install_System&action=edit
 * Update Freshmeat
 * Update BetaNews

"""

import os
import sys
import time
import Image, ImageFont, ImageDraw
from ConfigParser import ConfigParser
from ftplib import FTP
import time

### read config

cfg = ConfigParser()
cfg.read('release.cfg')

USER = cfg.get('auth', 'USER')
WIKI_PASSWORD = cfg.get('auth', 'WIKI_PASSWORD')

VERSION = cfg.get('version', 'VERSION')
VER_MAJOR = cfg.get('version', 'VER_MAJOR')
VER_MINOR = cfg.get('version', 'VER_MINOR')
VER_REVISION = cfg.get('version', 'VER_REVISION')
VER_BUILD = cfg.get('version', 'VER_BUILD')

CVS = cfg.get('cvs', 'CVS')
CVS_EXT = cfg.get('cvs', 'CVS_EXT')

TAR_BZ2 = cfg.get('compression', 'TAR_BZ2')
ZIP = cfg.get('compression', 'ZIP')

RSH = cfg.get('rsh', 'RSH')

PURGE_URL = cfg.get('wiki', 'PURGE_URL')
UPDATE_URL = cfg.get('wiki', 'UPDATE_URL')

### config env

os.environ['CVS_EXT'] = CVS_EXT
os.environ['CVSROOT'] = ':ext:%s@nsis.cvs.sourceforge.net:/cvsroot/nsis' % USER

CVS_TAG = 'v' + ''.join(VERSION.split('.'))

newverdir = 'nsis-%s-src' % VERSION

### some useful functions

def log(msg, log_dir = '.'):
	open('%s\\release-%s.log' % (log_dir, VERSION), 'a').write(msg + '\n')

def exit(log_dir = '.'):
	log('\nerror occurred, exiting', log_dir)
	sys.exit(3)

def run(command, log_name, err, wanted_ret = 0, log_dir = '.'):
	log('\nrunning %s\n' % command, log_dir)

	if log_name:
		cmd = '%s >> %s\\release-%s.log 2>&1' % (command, log_dir, VERSION)
	else:
		cmd = command

	ret = os.system(cmd)

	# sleep because for some weird reason, running cvs.exe hugs
	# the release log for some time after os.system returns
	import time
	time.sleep(5)

	if ret != wanted_ret:
		print '*** ' + err
		log('*** ' + err, log_dir)
		exit(log_dir)

def confirm(question):
	print question
	if raw_input() != 'y':
		sys.exit(2)

### confirm

confirm('are you sure you want to release version %s?' % VERSION)
confirm('did you update history.but?')

### start log

open('release-%s.log' % VERSION, 'w').write('releasing version %s at %s\n\n' % (VERSION, time.ctime()))

### test

print 'running tests...'

run(
	'scons -C .. test',
	'test',
	'tests failed - see test.log for details'
)

### create images

print 'creating images...'

## create new header.gif for menu

im = Image.new('RGB', (598, 45), '#000000')

# copy background from header-notext.gif

bim = Image.open(r'..\Menu\images\header-notext.gif')
im.paste(bim)

# draw new version number

draw = ImageDraw.Draw(im)
font = ImageFont.truetype('trebuc.ttf', 24)
text = 'nullsoft scriptable install system %s' % VERSION
draw.text((85, 7), text, font = font, fill = 'white')

# save

im = im.convert('P', palette = Image.ADAPTIVE)
im.save(r'..\Menu\images\header.gif')

# commit header.gif

print 'committing header.gif...'

run(
	'%s commit -m %s ..\\Menu\\images\\header.gif' % (CVS, VERSION),
	'header.gif.commit',
	'failed committing header.gif'
)

### test installer

print 'testing installer...'

os.mkdir('insttestscons')

run(
	'scons -C .. VERSION=test PREFIX=%s\\insttestscons install dist-installer' % os.getcwd(),
	'inst',
	'installer creation failed'
)

run(
	'..\\nsis-test-setup.exe /S /D=%s\\insttest' % os.getcwd(),
	None,
	'installer failed'
)

run(
	'diff -r insttest insttestscons | grep -v uninst-nsis.exe',
	'diff',
	'scons and installer installations differ',
	1
)

### tag

print 'tagging...'

run(
	'%s -z3 tag -R %s ..' % (CVS, CVS_TAG),
	'tag',
	'failed creating tag %s' % CVS_TAG
)

### export

print 'exporting a fresh copy...'

run(
	'%s -z3 export -r %s -d %s NSIS' % (CVS, CVS_TAG, newverdir),
	'export',
	'export failed'
)

### create source tarball

print 'creating source tarball...'

run(
	TAR_BZ2 % (newverdir + '.tar.bz2', newverdir),
	'tarball',
	'source tarball creation failed'
)

### build release files

scons_line = 'scons -C %s VERSION=%s VER_MAJOR=%s VER_MINOR=%s VER_REVISION=%s VER_BUILD=%s ' \
             % (newverdir, VERSION, VER_MAJOR, VER_MINOR, VER_REVISION, VER_BUILD)

print 'creating distribution files...'

run(
	scons_line + 'dist',
	'dist',
	'creation of distribution files failed'
)

def create_special_build(name, option):
	print 'creating %s special build...' % name

	os.mkdir(name)

	run(
		scons_line + 'PREFIX=%s\\%s %s install-compiler install-stubs' % (os.getcwd(), name, option),
		name,
		'creation of %s special build failed' % name
	)

	os.chdir(name)
	run(
		ZIP % ('..\\nsis-%s-%s.zip' % (VERSION, name), '*'),
		'%s.zip' % name,
		'copmression of %s special build failed' % name,
		log_dir = '..'
	)
	os.chdir('..')

create_special_build('strlen_8192', 'NSIS_MAX_STRLEN=8192')
create_special_build('log', 'NSIS_CONFIG_LOG=yes')

### upload files to SourceForge

print 'uploading files to SourceForge...'

def upload(ftp, file):
	print '  uploading %s...' % file
	ftp.storbinary('STOR /incoming/%s' % file.split('\\')[-1], open(file, 'rb'))

ftp = FTP('upload.sourceforge.net')
ftp.login()

upload(ftp, newverdir + '.tar.bz2')
upload(ftp, newverdir + '\\nsis-%s-setup.exe' % VERSION)
upload(ftp, newverdir + '\\nsis-%s.zip' % VERSION)
upload(ftp, 'nsis-%s-log.zip' % VERSION)
upload(ftp, 'nsis-%s-strlen_8192.zip' % VERSION)

ftp.quit()

### update some websites...

# manual release

print 'release url:'
print '  http://sourceforge.net/project/admin/qrs.php?package_id=0&group_id=22049'
print

sys.stdout.write('What\'s the SF release id of the new version? ')
release_id = raw_input()

# update wiki

print 'updating wiki...'

def update_wiki_page(page, data, summary):
	print '  updating `%s` to `%s`' % (page, data)

	import urllib

	post =  'su_user=' + urllib.quote(USER)
	post += '&su_password=' + urllib.quote(WIKI_PASSWORD)
	post += '&su_title=' + urllib.quote(page)
	post += '&su_data=' + urllib.quote(data)
	post += '&su_summary=' + urllib.quote(summary)
	
	if urllib.urlopen(UPDATE_URL, post).read() != 'success':
		log('*** failed updating `%s` wiki page' % page)
		print '  *** failed updating `%s` wiki page' % page
		exit()

def purge_wiki_page(page):
	import urllib
	urllib.urlopen(PURGE_URL % page).read()

update_wiki_page('Template:NSISVersion', VERSION, 'new version')
update_wiki_page('Template:NSISReleaseDate', time.strftime('%B %d, %Y'), 'new version')
update_wiki_page('Template:NSISReleaseID', release_id, 'new version')

purge_wiki_page('Main_Page')
purge_wiki_page('Download')
purge_wiki_page('Special_Builds')
purge_wiki_page('What_is_the_latest_version_of_NSIS')
purge_wiki_page('Change_Log')

# update changelog start time

run(
	'%s touch /home/groups/n/ns/nsis/bin/cl.timestamp' % RSH,
	'cl-timestamp',
	'change log start time modification failed'
)

### still left to do

print 'automatic phase done\n'
print """
 * Edit update.php
 * http://en.wikipedia.org/w/index.php?title=Nullsoft_Scriptable_Install_System&action=edit
 * Update Freshmeat
 * Update BetaNews
"""

### all done

log('done')
