## TODO
#
#   * Write SConscript for NSIS Menu
#    - Use inheritance instead of current wxWidgets patches
#    - Compile for POSIX too? wxWidgets is cross platform after all...
#
##

EnsurePythonVersion(1,6)

try:
	EnsureSConsVersion(0,96,91)
except TypeError: # EnsureSConsVersion in older versions took only two parameters
	print 'SCons 0.96.91 or greater is required, but you have an older version'
	Exit(2)
	
stubs = [
	'bzip2',
	'lzma',
	'zlib'
]

plugins = [
	'AdvSplash',
	'Banner',
	'BgImage',
	'Dialer',
	'InstallOptions',
	'LangDLL',
	'Library/TypeLib',
	'Math',
	'nsExec',
	'NSISdl',
	'Splash',
	'StartMenu',
	'System',
	'UserInfo',
	'VPatch/Source/Plugin'
]

utils = [
	'Library/LibraryLocal',
	'Library/RegTool',
	'MakeLangId',
	'Makensisw',
	'NSIS Menu',
	'UIs',
	'VPatch/Source/GenPat',
	'zip2exe'
]

misc = [
	'Graphics',
	'Language files',
	'Modern UI',
	'VPatch'
]

doc = [
	'license.txt'
]

defenv = Environment()
Export('defenv')

######################################################################
#######  Includes                                                  ###
######################################################################

SConscript('SCons/utils.py')

######################################################################
#######  Options                                                   ###
######################################################################

import os
hhc = 'no'
if defenv.WhereIs('hhc', os.environ['PATH']):
	hhc = 'yes'

from time import strftime, gmtime
cvs_version = strftime('%d-%b-%Y.cvs', gmtime())

opts = Options()

# load configuration options
#  it's important this will be done here so NSIS_CONFIG_CONST_DATA_PATH
#  will be available for the next few lines and so `dirs` can be set
SConscript('SCons/config.py')

opts.Update(defenv)
Help(opts.GenerateHelpText(defenv))

install_dirs = {
	'relocatable': {
		'dest': '',
		'prefix': '',
		'conf': '$PREFIX',
		'bin': '$PREFIX',
		'data': '$PREFIX',
		'doc': '$PREFIX',
	},
	'static': {
		'dest': '',
		'prefix': '/usr/local',
		'conf': '$PREFIX/etc',
		'bin': '$PREFIX/bin',
		'data': '$PREFIX/share/nsis',
		'doc': '$PREFIX/share/doc/nsis',
	}
}

if 'NSIS_CONFIG_CONST_DATA_PATH' in defenv['NSIS_CPPDEFINES']:
	dirs = install_dirs['static']
else:
	dirs = install_dirs['relocatable']

# version
opts.Add(('VERSION', 'Version of NSIS', cvs_version))
opts.Add(('VER_MAJOR', 'Major version of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_MINOR', 'Minor version of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_REVISION', 'Revision of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_BUILD', 'Build version of NSIS (recommended for dist-installer)', None))
# installation
opts.Add(('PREFIX', 'Installation prefix', dirs['prefix']))
opts.Add(ListOption('SKIPSTUBS', 'A list of stubs that will not be built', 'none', stubs))
opts.Add(ListOption('SKIPPLUGINS', 'A list of plug-ins that will not be built', 'none', plugins))
opts.Add(ListOption('SKIPUTILS', 'A list of utilities that will not be built', 'none', utils))
opts.Add(ListOption('SKIPMISC', 'A list of plug-ins that will not be built', 'none', misc))
opts.Add(ListOption('SKIPDOC', 'A list of doc files that will not be built/installed', 'none', doc))
# build tools
opts.Add(BoolOption('MSTOOLKIT', 'Use Microsoft Visual C++ Toolkit', 'no'))
opts.Add(BoolOption('CHMDOCS', 'Build CHM documentation, requires hhc.exe', hhc))
opts.Add(PathOption('CPPPATH', 'Path to search for include files', None))
opts.Add(PathOption('LIBPATH', 'Path to search for libraries', None))
# build options
opts.Add(BoolOption('DEBUG', 'Build executables with debugging information', 'no'))
opts.Add(PathOption('CODESIGNER', 'A program used to sign executables', None))
# path related build options
opts.Add(('PREFIX_DEST', 'Intermediate installation prefix (extra install time prefix)', dirs['dest']))
opts.Add(('PREFIX_CONF', 'Path to install nsisconf.nsh to', dirs['conf']))
opts.Add(('PREFIX_BIN', 'Path to install native binaries to', dirs['bin']))
opts.Add(('PREFIX_DATA', 'Path to install nsis data to (plugins, includes, stubs, contrib, win32 binaries)', dirs['data']))
opts.Add(('PREFIX_DOC','Path to install nsis README / INSTALL / TODO files to.', dirs['doc']))

opts.Update(defenv)
Help(opts.GenerateHelpText(defenv))

# add prefixes defines
if 'NSIS_CONFIG_CONST_DATA_PATH' in defenv['NSIS_CPPDEFINES']:
	defenv.Append(NSIS_CPPDEFINES = [('PREFIX_CONF', '"%s"' % defenv.subst('$PREFIX_CONF'))])
	defenv.Append(NSIS_CPPDEFINES = [('PREFIX_DATA', '"%s"' % defenv.subst('$PREFIX_DATA'))])

# write configuration into sconf.h and defines.h
sconf_h = open(File('#Source/exehead/sconf.h').abspath, 'w')
defines_h = open(File('#Source/defines.h').abspath, 'w')
for i in defenv['NSIS_CPPDEFINES']:
	if type(i) is not str:
		sconf_h.write('#define %s %s\n' % (i[0], i[1]))
		if str(i[1])[0] != '"':
			defines_h.write('definedlist.add("%s", "%s");\n' % (i[0], i[1]))
		else:
			defines_h.write('definedlist.add("%s", %s);\n' % (i[0], i[1]))
	else:
		sconf_h.write('#define %s\n' % (i))
		defines_h.write('definedlist.add("%s");\n' % (i))
sconf_h.close()
defines_h.close()

# write version into version.h
f = open(File('#Source/version.h').abspath, 'w')
f.write('#define NSIS_VERSION "v%s"\n' % defenv['VERSION'])
f.close()

######################################################################
#######  Functions                                                 ###
######################################################################

defenv['ZIPDISTDIR'] = defenv.Dir('#nsis-$VERSION')
defenv['INSTDISTDIR'] = defenv.Dir('#.instdist')
defenv['TESTDISTDIR'] = defenv.Dir('#.test')
defenv['DISTSUFFIX'] = ''

if defenv.has_key('CODESIGNER'):
	defenv['DISTSUFFIX'] = '-signed'

defenv.Execute(Delete('$ZIPDISTDIR'))
defenv.Execute(Delete('$INSTDISTDIR'))
defenv.Execute(Delete('$TESTDISTDIR'))

def Distribute(files, names, component, path, subpath, alias, install_alias=None):
	if isinstance(files, (str, type(File('SConstruct')))):
		files = [files]
	files = map(File, files)

	if isinstance(names, str):
		names = [names]
	if not names:
		names = map(str, files)
		names = map(os.path.basename, names)

	for d in ('$ZIPDISTDIR', '$INSTDISTDIR', '$TESTDISTDIR'):
		paths = map(lambda file: os.path.join(d, path, subpath, file), names)
		defenv.InstallAs(paths, files)

	if (defenv.has_key('PREFIX') and defenv['PREFIX']) or (defenv.has_key('PREFIX_DEST') and defenv['PREFIX_DEST']) :
		prefix = '${PREFIX_DEST}${PREFIX_%s}' % component.upper()
		paths = map(lambda file: os.path.join(prefix, path, subpath, file), names)
		ins = defenv.InstallAs(paths, files)
	else:
		ins = []

	if ins:
		defenv.Alias('install', ins)
		defenv.Alias('install-%s' % component, ins)
		if alias:
			defenv.Alias(alias, ins)
		if install_alias:
			defenv.Alias('install-%s' % install_alias, ins)

		return ins

def DistributeBin(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'bin', '', path, alias)

def DistributeConf(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'conf', '', path, alias)

def DistributeW32Bin(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Bin', path, alias, 'w32bin')

def DistributeStubs(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Stubs', path, alias, 'stubs')

def DistributePlugin(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Plugins', path, alias, 'plugins')

def DistributeContrib(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Contrib', path, alias, 'contrib')

def DistributeMenu(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Menu', path, alias, 'menu')

def DistributeInclude(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'data', 'Include', path, alias, 'includes')

def DistributeDoc(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'doc', '', path, alias)

def DistributeDocs(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'doc', 'Docs', path, alias, 'docs')

def DistributeExamples(files, names=[], path='', alias=None):
	return defenv.Distribute(files, names, 'doc', 'Examples', path, alias, 'examples')

def Sign(targets):
	if defenv.has_key('CODESIGNER'):
		for t in targets:
			a = defenv.Action('$CODESIGNER "%s"' % t.path)
			defenv.AddPostAction(t, a)

def TestScript(scripts):
	defenv.Install('$TESTDISTDIR/Tests', scripts)

defenv.Distribute = Distribute
defenv.DistributeBin = DistributeBin
defenv.DistributeConf = DistributeConf
defenv.DistributeW32Bin = DistributeW32Bin
defenv.DistributeStubs = DistributeStubs
defenv.DistributePlugin = DistributePlugin
defenv.DistributeContrib = DistributeContrib
defenv.DistributeMenu = DistributeMenu
defenv.DistributeInclude = DistributeInclude
defenv.DistributeDoc = DistributeDoc
defenv.DistributeDocs = DistributeDocs
defenv.DistributeExamples = DistributeExamples
defenv.Sign = Sign
defenv.TestScript = TestScript

######################################################################
#######  Environments                                              ###
######################################################################

if defenv['DEBUG']:
	defenv.Replace(BUILD_PREFIX = 'build/debug')
else:
	defenv.Replace(BUILD_PREFIX = 'build/release')

if defenv['MSTOOLKIT']:
	defenv.Tool('mstoolkit', toolpath = ['SCons/Tools'])

defenv.Default('$BUILD_PREFIX')

tools = defenv['TOOLS']

envs = []

if 'msvc' in tools or 'mstoolkit' in tools:
	envs = SConscript('SCons/Config/ms')
elif 'gcc' in tools:
	envs = SConscript('SCons/Config/gnu')
else:
	envs = SConscript('SCons/Config/default')

stub_env = envs[0]
makensis_env = envs[1]
plugin_env = envs[2]
util_env = envs[3]
cp_util_env = envs[4]
test_env = envs[5]

######################################################################
#######  Distribution                                              ###
######################################################################

dist_zip = 'nsis-${VERSION}${DISTSUFFIX}.zip'
zip_target = defenv.Zip(dist_zip, '$ZIPDISTDIR')
defenv.Alias('dist-zip', zip_target)

AlwaysBuild(defenv.AddPostAction(zip_target, Delete('$ZIPDISTDIR')))

if defenv['PLATFORM'] == 'win32':
	optchar = '/'
else:
	optchar = '-'

defenv['INSTVER'] = '%sDVERSION=$VERSION' % optchar
if defenv.has_key('VER_MAJOR') and defenv.has_key('VER_MINOR') \
    and defenv.has_key('VER_REVISION') and defenv.has_key('VER_BUILD'):
	defenv['INSTVER'] += ' %sDVER_MAJOR=$VER_MAJOR' % optchar
	defenv['INSTVER'] += ' %sDVER_MINOR=$VER_MINOR' % optchar
	defenv['INSTVER'] += ' %sDVER_REVISION=$VER_REVISION' % optchar
	defenv['INSTVER'] += ' %sDVER_BUILD=$VER_BUILD' % optchar

inst_env = {}
inst_env['NSISDIR'] = os.path.abspath(str(defenv['INSTDISTDIR']))
inst_env['NSISCONFDIR'] = os.path.abspath(str(defenv['INSTDISTDIR']))

installer_target = defenv.Command('nsis-${VERSION}-setup${DISTSUFFIX}.exe',
                                  '$INSTDISTDIR' + os.sep + 'Examples' + os.sep + 'makensis.nsi',
                                  '$INSTDISTDIR' + os.sep + 'makensis$PROGSUFFIX ' +
                                  '%sDOUTFILE=$TARGET.abspath $INSTVER $SOURCE' % optchar,
                                  ENV = inst_env)
defenv.Depends(installer_target, '$INSTDISTDIR')
defenv.Sign(installer_target)
defenv.Alias('dist-installer', installer_target)

AlwaysBuild(defenv.AddPostAction(installer_target, Delete('$INSTDISTDIR')))

defenv.Alias('dist', ['dist-zip', 'dist-installer'])

######################################################################
#######  Distribute Basics                                         ###
######################################################################

for d in doc:
	if d in defenv['SKIPDOC']:
		continue
	defenv.DistributeDoc(d)

defenv.DistributeConf('nsisconf.nsh')

######################################################################
#######  Stubs                                                     ###
######################################################################

def BuildStub(compression, solid):
	env = stub_env.Copy()

	suffix = ''
	if solid:
		suffix = '_solid'

	build_dir = '$BUILD_PREFIX/stub_%s%s' % (compression, suffix)

	exports = { 'env' : env, 'compression' : compression, 'solid_compression' : solid }

	target = defenv.SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = False, exports = exports)
	env.SideEffect('%s/stub_%s.map' % (build_dir, stub), target)

	env.DistributeStubs(target, names=compression+suffix)

	defenv.Alias(compression, target)
	defenv.Alias('stubs', target)

for stub in stubs:
	if stub in defenv['SKIPSTUBS']:
		continue

	BuildStub(stub, False)
	BuildStub(stub, True)

defenv.DistributeStubs('Source/exehead/uninst.ico',names='uninst')

######################################################################
#######  makensis                                                  ###
######################################################################

build_dir = '$BUILD_PREFIX/makensis'
exports = { 'env' : makensis_env }

makensis = defenv.SConscript(dirs = 'Source', build_dir = build_dir, duplicate = False, exports = exports)

makensis_env.SideEffect('%s/makensis.map' % build_dir, makensis)

defenv.Alias('makensis', makensis)

ins = defenv.DistributeBin(makensis,alias='install-compiler')

######################################################################
#######  Common Functions                                          ###
######################################################################

def AddEnvStandardFlags(env, defines, flags, entry, nodeflib):
	if defines:
		env.Append(CPPDEFINES = defines)
	if flags:
		env.Append(CCFLAGS = flags)

	if entry:
		env.Append(LINKFLAGS = '${ENTRY_FLAG("%s")}' % entry)

	if nodeflib:
		env.Append(LINKFLAGS = '$NODEFLIBS_FLAG') # no default libraries

def AppendRES(env, source, res, resources, target_name = None):
	if res:
		target_res = env.RES(target_name, res)
		if resources:
			env.Depends(target_res, resources)
		source.append(target_res)

def CleanMap(env, target, target_name):
	env.Clean(target, File(target_name + '.map'))

def DistributeExtras(env, target, examples, docs):
	if examples:
		env.DistributeExamples(examples, path=target)
	if docs:
		env.DistributeDocs(docs, path=target)

######################################################################
#######  Plug-ins                                                  ###
######################################################################

def BuildPlugin(target, source, libs, examples = None, docs = None,
                entry = 'DllMain', res = None, res_target = None,
                resources = None, defines = None, flags = None, 
                nodeflib = True, cppused = False):
	env = plugin_env.Copy()

	if cppused and env['CPP_REQUIRES_STDLIB']:
		nodeflib = False

	AddEnvStandardFlags(env, defines, flags, entry, nodeflib)

	AppendRES(env, source, res, resources, res_target)

	plugin = env.SharedLibrary(target, source, LIBS = libs)
	defenv.Alias(target, plugin)
	defenv.Alias('plugins', plugin)

	defenv.Sign(plugin)

	CleanMap(env, plugin, target)

	for i in plugin:
		if str(i)[-4:].lower() == '.dll':
			plugin = i
			break
	env.DistributePlugin(plugin)

	DistributeExtras(env, target, examples, docs)

for plugin in plugins:
	if plugin in defenv['SKIPPLUGINS']:
		continue

	path = 'Contrib/' + plugin
	build_dir = '$BUILD_PREFIX/' + plugin
	exports = {'BuildPlugin' : BuildPlugin, 'env' : plugin_env.Copy()}

	defenv.SConscript(dirs = path, build_dir = build_dir, duplicate = False, exports = exports)

######################################################################
#######  Utilities                                                 ###
######################################################################

def BuildUtil(target, source, libs, entry = None, res = None, 
              resources = None, defines = None, flags = None,
              nodeflib = False, file_name = '', path='', contrib = False,
              examples = None, docs = None, cross_platform = False,
							root_util = False):
	if not cross_platform:
		env = util_env.Copy()
	else:
		env = cp_util_env.Copy()

	AddEnvStandardFlags(env, defines, flags, entry, nodeflib)

	AppendRES(env, source, res, resources)

	if file_name != '':
		target = "%s/%s" % (target, file_name)

	util = env.Program(target, source, LIBS = libs)
	defenv.Alias(target, util)
	defenv.Alias('utils', util)

	defenv.Sign(util)

	CleanMap(env, util, target)

	if contrib:
		ins = env.DistributeContrib(util, path=path, alias='install-utils')
	elif cross_platform and not env['PLATFORM'] == 'win32' or root_util and env['PLATFORM'] == 'win32':
		ins = env.DistributeBin(util, path=path, alias='install-utils')
	else:
		ins = env.DistributeW32Bin(util, path=path, alias='install-utils')

	DistributeExtras(env, target, examples, docs)

	return util

for util in utils:
	if util in defenv['SKIPUTILS']:
		continue

	path = 'Contrib/' + util
	build_dir = '$BUILD_PREFIX/' + util
	exports = {'BuildUtil' : BuildUtil, 'env' : util_env.Copy()}

	defenv.SConscript(dirs = path, build_dir = build_dir, duplicate = False, exports = exports)

######################################################################
#######  Documentation                                             ###
######################################################################

halibut = defenv.SConscript(
	dirs = 'Docs/src/bin/halibut',
	build_dir = '$BUILD_PREFIX/halibut',
	duplicate = False,
	exports = {'env' : defenv.Copy()}
)

if defenv['CHMDOCS']:
	defenv.SConscript(
		dirs = 'Docs/src',
		build_dir = '$BUILD_PREFIX/Docs/chm',
		duplicate = False,
		exports = {'halibut' : halibut, 'env' : defenv.Copy(), 'build_chm' : True}
	)
else:
	defenv.SConscript(
		dirs = 'Docs/src',
		build_dir = '$BUILD_PREFIX/Docs/html',
		duplicate = False,
		exports = {'halibut' : halibut, 'env' : defenv.Copy(), 'build_chm' : False}
	)

######################################################################
#######  Examples                                                  ###
######################################################################

defenv.SConscript(
	dirs = 'Examples',
	exports = {'env': defenv.Copy()}
)

######################################################################
#######  Includes                                                  ###
######################################################################

defenv.SConscript(
	dirs = 'Include',
	exports = {'env': defenv.Copy()}
)

######################################################################
#######  Miscellaneous                                             ###
######################################################################

for i in misc:
	if i in defenv['SKIPMISC']:
		continue

	defenv.SConscript(dirs = 'Contrib/%s' % i)

######################################################################
#######  Tests                                                     ###
######################################################################

# test code

build_dir = '$BUILD_PREFIX/tests'
exports = {'env' : test_env.Copy()}

defenv.SConscript(
	dirs = 'Source/Tests',
	duplicate = False,
	exports = exports,
	build_dir = build_dir
)

defenv.Ignore('$BUILD_PREFIX', '$BUILD_PREFIX/tests')

# test scripts

test_scripts_env = defenv.Copy(ENV = os.environ) # env needed for some scripts
test_scripts_env['ENV']['NSISDIR'] = os.path.abspath(str(defenv['TESTDISTDIR']))
test_scripts_env['ENV']['NSISCONFDIR'] = os.path.abspath(str(defenv['TESTDISTDIR']))

def test_scripts(target, source, env):
	from os import walk, sep

	instdir = source[0].path

	makensis = instdir + sep + 'makensis'

	for root, dirs, files in walk(instdir):
		for file in files:
			if file[-4:] == '.nsi':
				nsi = root + sep + file
				cmd = env.Command(None, nsi, '%s $SOURCE' % makensis)
				AlwaysBuild(cmd)
				env.Alias('test-scripts', cmd)

	return None

test = test_scripts_env.Command('test-scripts.log', '$TESTDISTDIR', test_scripts)
test_scripts_env.Alias('test-scripts', test)

# test all

defenv.Alias('test', ['test-code', 'test-scripts'])
