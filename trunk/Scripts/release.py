"""
requires Python Image Library - http://www.pythonware.com/products/pil/
requires grep and diff - http://www.mingw.org/msys.shtml
requires command line cvs - http://tortoisecvs.sourceforge.net/

example release.cfg:
=========================
[auth]
USER=kichik

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

[scp]
SCP="pscp -2"
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
 * Edit cl.sh
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISVersion&action=edit
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISReleaseDate&action=edit
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISReleaseID&action=edit
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

### read config

cfg = ConfigParser()
cfg.read('release.cfg')

USER = cfg.get('auth', 'USER')

VERSION = cfg.get('version', 'VERSION')
VER_MAJOR = cfg.get('version', 'VER_MAJOR')
VER_MINOR = cfg.get('version', 'VER_MINOR')
VER_REVISION = cfg.get('version', 'VER_REVISION')
VER_BUILD = cfg.get('version', 'VER_BUILD')

CVS = cfg.get('cvs', 'CVS')
CVS_EXT = cfg.get('cvs', 'CVS_EXT')

TAR_BZ2 = cfg.get('compression', 'TAR_BZ2')
ZIP = cfg.get('compression', 'ZIP')

SCP = cfg.get('scp', 'SCP')

### config env

os.environ['CVS_EXT'] = CVS_EXT
os.environ['CVSROOT'] = ':ext:%s@cvs.sourceforge.net:/cvsroot/nsis' % USER

CVS_TAG = 'v' + ''.join(VERSION.split('.'))

newverdir = 'nsis-%s-src' % VERSION

### some useful functions

def log(msg, log_dir = '.'):
	open('%s\\release-%s.log' % (log_dir, VERSION), 'a').write(msg + '\n')

def exit(log_dir = '.'):
	log('\nerror occurred, exiting', log_dir)
	sys.exit(3)

def run(command, log_name, err, wanted_ret = 0, log_dir = '.'):
	log('running %s' % command, log_dir)

	if log_name:
		cmd = '%s >> %s\\release-%s.log 2>&1' % (command, log_dir, VERSION)
	else:
		cmd = command

	if os.system(cmd) != wanted_ret:
		print '*** ' + err
		log('*** ' + err, log_dir)
		exit(log_dir)

	log('', log_dir)

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
	'%s commit -m "%s" ..\\Menu\\images\\header.gif' % (CVS, VERSION),
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
	'..\\nsis-test.exe /S /D=%s\\insttest' % os.getcwd(),
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
upload(ftp, newverdir + '\\nsis-%s.exe' % VERSION)
upload(ftp, newverdir + '\\nsis-%s.zip' % VERSION)
upload(ftp, 'nsis-%s-log.zip' % VERSION)
upload(ftp, 'nsis-%s-strlen_8192.zip' % VERSION)

ftp.quit()

### update some websites...

print 'automatic phase done\n'
print """
 * Add SourceForge release
 * Edit update.php
 * Edit cl.sh
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISVersion&action=edit
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISReleaseDate&action=edit
 * http://nsis.sourceforge.net/mediawiki/index.php?title=Template:NSISReleaseID&action=edit
 * http://en.wikipedia.org/w/index.php?title=Nullsoft_Scriptable_Install_System&action=edit
 * Update Freshmeat
 * Update BetaNews
"""

### all done

log('done')
