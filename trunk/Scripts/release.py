"""
requires Python Image Library - http://www.pythonware.com/products/pil/
requires grep and diff - http://www.mingw.org/msys.shtml
requires command line svn - http://subversion.tigris.org/
requires pysvn - http://pysvn.tigris.org/
requires win32com - http://starship.python.net/~skippy/win32/Downloads.html
requires requests - https://requests.readthedocs.io/en/latest/

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

[svn]
SVN="C:\svn-win32\bin\svn.exe"
SVNROOT=https://svn.code.sf.net/p/nsis/code/NSIS

[compression]
TAR_BZ2=7zatarbz2.bat %s %s
ZIP="C:\Program Files\7-zip\7za.exe" a -tzip %s -mx9 -mfb=255 -mpass=4 %s

[rsh]
RSH="C:\Program Files\PuTTY\plink.exe" -2 -l kichik nsis.sourceforge.net

[sftp]
SFTP="C:\Program Files\PuTTY\psftp.exe" -2 -l kichik,nsis -batch -b %s frs.sourceforge.net

[wiki]
PURGE_URL=https://nsis.sourceforge.io/%s?action=purge
UPDATE_URL=https://nsis.sourceforge.io/Special:Simpleupdate?action=raw

[svn2cl]
SVN2CL_XSL=svn2cl.xsl

[options]
SKIP_CPPUNIT=no
SCONS_ARGS="TOOLSET=msvc,mssdk,mslink,mslib,masm"
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
import os.path
import shutil
import sys
import time
from PIL import Image, ImageFont, ImageDraw
from configparser import RawConfigParser
import time
import pysvn
import requests
import urllib.parse

### read config

if not os.path.isfile('release.cfg'):
    print('Unable to find release.cfg. Is this the right working directory?')
    sys.exit(1)

cfg = RawConfigParser()
cfg.read('release.cfg')

USER = cfg.get('auth', 'USER')
WIKI_PASSWORD = cfg.get('auth', 'WIKI_PASSWORD')

VERSION = cfg.get('version', 'VERSION')
VER_MAJOR = cfg.get('version', 'VER_MAJOR')
VER_MINOR = cfg.get('version', 'VER_MINOR')
VER_REVISION = cfg.get('version', 'VER_REVISION')
VER_BUILD = cfg.get('version', 'VER_BUILD')

PRE_RELEASE_VERSION = 'a' in VERSION or 'b' in VERSION

SVN = cfg.get('svn', 'SVN')
SVNROOT = cfg.get('svn', 'SVNROOT')

TAR_BZ2 = cfg.get('compression', 'TAR_BZ2')
ZIP = cfg.get('compression', 'ZIP')

RSH = cfg.get('rsh', 'RSH')

SFTP = cfg.get('sftp', 'SFTP')

PURGE_URL = cfg.get('wiki', 'PURGE_URL')
UPDATE_URL = cfg.get('wiki', 'UPDATE_URL')

SVN2CL_XSL = cfg.get('svn2cl', 'SVN2CL_XSL')

SKIP_CPPUINT = cfg.get('options', 'SKIP_CPPUNIT')

SCONS_ARGS = cfg.get('options', 'SCONS_ARGS')

### config env

SVN_TAG = 'v' + ''.join(VERSION.split('.'))

SOURCE_DATE_EPOCH = str(int(time.time()))
SCONS_REPRODUCIBLE_ARGS = 'SOURCE_DATE_EPOCH=%s' % (SOURCE_DATE_EPOCH)

newverdir = 'nsis-%s-src' % VERSION
scons_line = 'scons %s %s -C %s VERSION=%s VER_MAJOR=%s VER_MINOR=%s VER_REVISION=%s VER_BUILD=%s ' \
						 % (SCONS_ARGS, SCONS_REPRODUCIBLE_ARGS, newverdir, \
						    VERSION, VER_MAJOR, VER_MINOR, VER_REVISION, VER_BUILD)

### utility functions

def log(msg, log_dir = '.'):
	open('%s\\release-%s.log' % (log_dir, VERSION), 'a').write(msg + '\n')

def exit(log_dir = '.'):
	log('\nerror occurred, exiting', log_dir)
	sys.exit(3)

LOG_ERRORS  = 2
LOG_ALL     = 1
LOG_NOTHING = 0

def run(command, log_level, err, wanted_ret = 0, log_dir = '.'):
	log('\nrunning %s\n' % command, log_dir)

	if log_level == LOG_ERRORS:
		cmd = '%s 2>> %s\\release-%s.log' % (command, log_dir, VERSION)
	elif log_level == LOG_ALL:
		cmd = '%s >> %s\\release-%s.log 2>&1' % (command, log_dir, VERSION)
	elif log_level == LOG_NOTHING:
		cmd = command
	else:
		raise ValueError

	ret = os.system('if 1==1 ' + cmd)

	# sleep because for some weird reason, running cvs.exe hugs
	# the release log for some time after os.system returns
	#    still needed for svn?
	import time
	time.sleep(5)

	if ret != wanted_ret:
		print('*** ' + err)
		log('*** ' + err, log_dir)
		exit(log_dir)

def confirm(question):
	print(question)
	if input() != 'y':
		sys.exit(2)

### process functions

def Confirm():
	confirm('are you sure you want to release version %s?' % VERSION)
	confirm('did you update history.but?')

def StartLog():
	open('release-%s.log' % VERSION, 'w').write('releasing version %s at %s\n\n' % (VERSION, time.ctime()))
	
def DeleteOldFolders():
    for d in ['strlen_8192', 'log', 'insttest', 'insttestscons']:
        if os.path.isdir(d):
            log('Deleting %s' % d)
            shutil.rmtree(d)

def RunTests():
	print('running tests...')

	run(
		'scons %s -C .. %s' % (SCONS_ARGS, SKIP_CPPUINT == 'yes' and 'test-scripts' or 'test'),
		LOG_ALL,
		'tests failed - see test.log for details'
	)

def TestSubversionEOL():
	print('ensuring EOL...')

	from os import walk
	from os.path import join
	from os.path import splitext
	
	eoldict = {
		'.nsh' : 'native',
		'.nsi' : 'native',
		'.txt' : 'native',
		'.ini' : 'CRLF',
		'.dsp' : 'CRLF',
		'.dsw' : 'CRLF'
	}

	exceptions = ['newfile.txt', 'oldfile.txt']

	svn = pysvn.Client()

	bad_eol = False
	for root, dirs, files in walk('..'):
		def versioned(f):
			try:
				s = svn.status(join(root, f))[0].text_status
			except pysvn.ClientError as e:
				if 'not a working copy' in e.args[0]:
					return False
				if 'was not found' in e.args[0]:
					return False
				raise
			return s != pysvn.wc_status_kind.unversioned

		svn_files = filter(versioned, files)
		svn_files = filter(lambda x: x not in exceptions, svn_files)

		for f in svn_files:
			ext = splitext(f)[1]
			if ext in eoldict.keys():
				eol = eoldict[ext]
				path = join(root, f)
				s = list(svn.propget('svn:eol-style', path).values())
				if not s or s[0] != eol:
					error = f'*** {path} has bad eol-style (expected "{eol}", got "{s}")'
					print(error)
					log(error)
					bad_eol = True
					
	if bad_eol:
		exit()

def CreateMenuImage():
	print('creating images...')

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

def CommitMenuImage():
	print('committing header.gif...')

	run(
		'%s commit -m %s ..\\Menu\\images\\header.gif' % (SVN, VERSION),
		LOG_ALL,
		'failed committing header.gif'
	)

def TestInstaller():
	print('testing installer...')

	os.mkdir('insttestscons')

	run(
		'scons %s -C .. VERSION=test PREFIX=%s\\insttestscons install dist-installer' % (SCONS_ARGS, os.getcwd()),
		LOG_ALL,
		'installer creation failed'
	)

	run(
		'..\\nsis-test-setup.exe /S /D=%s\\insttest' % os.getcwd(),
		LOG_NOTHING,
		'installer failed'
	)

	run(
		'diff -r insttest insttestscons | grep -v uninst-nsis.exe | grep -v NSIS.exe',
		LOG_ALL,
		'scons and installer installations differ',
		1
	)

def Tag():
	print('tagging...')

	run(
		'%s copy %s/trunk %s/tags/%s -m "Tagging for release %s"' % (SVN, SVNROOT, SVNROOT, SVN_TAG, VERSION),
		LOG_ALL,
		'failed creating tag %s' % SVN_TAG
	)

def Export():
	print('exporting a fresh copy...')

	run(
		'%s export %s/tags/%s %s' % (SVN, SVNROOT, SVN_TAG, newverdir),
		LOG_ALL,
		'export failed'
	)

def CreateChangeLog():
	import win32com.client
	import codecs

	if not os.path.isfile(SVN2CL_XSL):
		print('downloading svn2cl.xsl stylesheet...')
		r = requests.get('http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/svn2cl/svn2cl.xsl')
		r.raise_for_status()
		with open(SVN2CL_XSL, 'w') as f:
			f.write(r.text)

	print('generating ChangeLog...')

	changelog = os.path.join(newverdir, 'ChangeLog')

	# generate changelog xml
	run(
		'%s log --xml --verbose %s > %s' % (SVN, SVNROOT, changelog),
		LOG_ERRORS,
		'changelog failed'
	)

	# load changelog xml
	xmlo = win32com.client.Dispatch('Microsoft.XMLDOM')
	xmlo.loadXML(open(changelog).read())
	xmlo.preserveWhiteSpace = True

	# load xsl
	xslo = win32com.client.Dispatch('Microsoft.XMLDOM')
	xslo.validateOnParse = False
	xslo.preserveWhiteSpace = True
	xslo.loadXML(open(SVN2CL_XSL).read())

	# set strip-prefix to ''
	for a in xslo.selectNodes("/xsl:stylesheet/xsl:param[@name = 'strip-prefix']")[0].attributes:
		if a.name == 'select':
			a.value = "''"

	# transform
	transformed = xmlo.transformNode(xslo)
	codecs.open(changelog, 'w', 'utf-8').write(transformed)

def CreateSourceTarball():
	print('creating source tarball...')

	run(
		TAR_BZ2 % (newverdir + '.tar.bz2', newverdir),
		LOG_ALL,
		'source tarball creation failed'
	)

def BuildRelease():
	print('creating distribution files...')

	run(
		scons_line + 'dist',
		LOG_ALL,
		'creation of distribution files failed'
	)

def CreateSpecialBuilds():
	def create_special_build(name, option):
		print('creating %s special build...' % name)

		os.mkdir(name)

		run(
			scons_line + 'PREFIX=%s\\%s %s install-compiler install-stubs' % (os.getcwd(), name, option),
			LOG_ALL,
			'creation of %s special build failed' % name
		)

		os.chdir(name)
		run(
			ZIP % ('..\\nsis-%s-%s.zip' % (VERSION, name), '*'),
			LOG_ALL,
			'compression of %s special build failed' % name,
			log_dir = '..'
		)
		os.chdir('..')

	create_special_build('strlen_8192', 'NSIS_MAX_STRLEN=8192')
	create_special_build('log', 'NSIS_CONFIG_LOG=yes')

def UploadFiles():
	print('uploading files to SourceForge...')

	folder = 'NSIS 3/' + VERSION
	if PRE_RELEASE_VERSION:
		folder = 'NSIS 3 Pre-release/' + VERSION

	sftpcmds = open('sftp-commands', 'w')
	sftpcmds.write('mkdir "/home/frs/project/nsis/%s"\n' % folder)
	sftpcmds.write('cd "/home/frs/project/nsis/%s"\n' % folder)
	sftpcmds.write('put %s.tar.bz2\n' % newverdir)
	sftpcmds.write('put %s\\nsis-%s-setup.exe\n' % (newverdir, VERSION))
	sftpcmds.write('put %s\\nsis-%s.zip\n' % (newverdir, VERSION))
	sftpcmds.write('put nsis-%s-log.zip\n' % VERSION)
	sftpcmds.write('put nsis-%s-strlen_8192.zip\n' % VERSION)
	sftpcmds.write('put %s\\build\\urelease\\Docs\\chm\\SectionF.1.html RELEASE.html\n' % newverdir)
	sftpcmds.close()

	run(
		SFTP % 'sftp-commands',
		LOG_ERRORS,
		'upload failed'
	)

	os.unlink('sftp-commands')

def ManualRelease():
	print('go fix release notes...')
	print('  http://nsis.sf.net/rn/new')
	print()

	input()

def UpdateWiki():
	print('updating wiki...')

	def update_wiki_page(page, data, summary):
		print('  updating `%s` to `%s`' % (page, data))

		import urllib

		post =  'su_user=' + urllib.parse.quote(USER)
		post += '&su_password=' + urllib.parse.quote(WIKI_PASSWORD)
		post += '&su_title=' + urllib.parse.quote(page)
		post += '&su_data=' + urllib.parse.quote(data)
		post += '&su_summary=' + urllib.parse.quote(summary)
		
		if requests.get(UPDATE_URL, post).text.strip() != 'success':
			log('*** failed updating `%s` wiki page' % page)
			print('	*** failed updating `%s` wiki page' % page)

	if not PRE_RELEASE_VERSION:
		update_wiki_page('Template:NSISVersion', VERSION, 'new version')
		update_wiki_page('Template:NSISReleaseDate', time.strftime('%B %d, %Y'), 'new version')
		update_wiki_page('Template:NSISIsPreRelease', 'no', 'new version')
	else:
		update_wiki_page('Template:NSISPreVersion', VERSION, 'new version')
		update_wiki_page('Template:NSISPreReleaseDate', time.strftime('%B %d, %Y'), 'new version')
		update_wiki_page('Template:NSISIsPreRelease', 'yes', 'new version')

	os.system('start ' + PURGE_URL % 'Download')

def ToDo():
	print('automatic phase done\n')
	print("""
 * Make new release files the default download
 * Edit update.php
 * Post news item
 * http://en.wikipedia.org/w/index.php?title=Nullsoft_Scriptable_Install_System&action=edit
 * Update Freshmeat
 * Update BetaNews
	""")

def CloseLog():
	log('done')

### ok, let's go!

Confirm()
StartLog()
DeleteOldFolders()
RunTests()
TestSubversionEOL()
CreateMenuImage()
CommitMenuImage()
TestInstaller()
Tag()
Export()
CreateChangeLog()
CreateSourceTarball()
BuildRelease()
CreateSpecialBuilds()
UploadFiles()
ManualRelease()
UpdateWiki()
ToDo()
CloseLog()
