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

# version
opts.Add(('VERSION', 'Version of NSIS', cvs_version))
opts.Add(('VER_MAJOR', 'Major version of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_MINOR', 'Minor version of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_REVISION', 'Revision of NSIS (recommended for dist-installer)', None))
opts.Add(('VER_BUILD', 'Build version of NSIS (recommended for dist-installer)', None))
# installation
opts.Add(PathOption('PREFIX', 'Installation prefix', None))
opts.Add(ListOption('SKIPSTUBS', 'A list of stubs that will not be built', 'none', stubs))
opts.Add(ListOption('SKIPPLUGINS', 'A list of plug-ins that will not be built', 'none', plugins))
opts.Add(ListOption('SKIPUTILS', 'A list of utilities that will not be built', 'none', utils))
opts.Add(ListOption('SKIPMISC', 'A list of plug-ins that will not be built', 'none', misc))
# build tools
opts.Add(BoolOption('MSTOOLKIT', 'Use Microsoft Visual C++ Toolkit', 'no'))
opts.Add(BoolOption('CHMDOCS', 'Build CHM documentation, requires hhc.exe', hhc))
opts.Add(PathOption('CPPPATH', 'Path to search for include files', None))
opts.Add(PathOption('LIBPATH', 'Path to search for libraries', None))
# build options
opts.Add(BoolOption('DEBUG', 'Build executables with debugging information', 'no'))
opts.Add(PathOption('CODESIGNER', 'A program used to sign executables', None))

opts.Update(defenv)

Help(opts.GenerateHelpText(defenv))

# build configuration
SConscript('SCons/config.py')

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

def Distribute(dir, files):
	defenv.Install('$ZIPDISTDIR/%s' % dir, files)
	defenv.Install('$INSTDISTDIR/%s' % dir, files)
	defenv.Install('$TESTDISTDIR/%s' % dir, files)

	if defenv.has_key('PREFIX') and defenv['PREFIX']:
		ins = defenv.Install('$PREFIX/%s' % dir, files)
		return ins

	return []

def DistributeAs(path, file):
	defenv.InstallAs('$ZIPDISTDIR/%s' % path, file)
	defenv.InstallAs('$INSTDISTDIR/%s' % path, file)
	defenv.InstallAs('$TESTDISTDIR/%s' % path, file)

	if defenv.has_key('PREFIX') and defenv['PREFIX']:
		ins = defenv.InstallAs('$PREFIX/%s' % path, file)
		return ins

	return []

def DistributeExamples(dir, examples):
	return Distribute('Examples/%s' % dir, examples)

def DistributeDocs(dir, docs):
	return Distribute('Docs/%s' % dir, docs)

def DistributeContribs(dir, contribs):
	return Distribute('Contrib/%s' % dir, contribs)

def Sign(targets):
	if defenv.has_key('CODESIGNER'):
		for t in targets:
			a = defenv.Action('$CODESIGNER "%s"' % t.path)
			defenv.AddPostAction(t, a)

def TestScript(scripts):
	defenv.Install('$TESTDISTDIR/Tests', scripts)

defenv.Distribute = Distribute
defenv.DistributeAs = DistributeAs
defenv.DistributeExamples = DistributeExamples
defenv.DistributeDocs = DistributeDocs
defenv.DistributeContribs = DistributeContribs
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

######################################################################
#######  Aliases                                                   ###
######################################################################

defenv.Alias('install', '$PREFIX')
defenv.Alias('install-docs', '$PREFIX/NSIS.chm')
defenv.Alias('install-docs', '$PREFIX/Docs')
defenv.Alias('install-examples', '$PREFIX/Examples')
defenv.Alias('install-plugins', '$PREFIX/Plugins')
defenv.Alias('install-stubs', '$PREFIX/Stubs')
defenv.Alias('install-includes', '$PREFIX/Include')

# defined elsewhere:
#  install-compiler
#  install-utils

######################################################################
#######  Distribution                                              ###
######################################################################

dist_zip = 'nsis-${VERSION}${DISTSUFFIX}.zip'
zip_target = defenv.Zip(dist_zip, '$ZIPDISTDIR')
defenv.Alias('dist-zip', zip_target)

AlwaysBuild(defenv.AddPostAction(zip_target, Delete('$ZIPDISTDIR')))

defenv['INSTVER'] = '/DVERSION=$VERSION'
if defenv.has_key('VER_MAJOR') and defenv.has_key('VER_MINOR') \
    and defenv.has_key('VER_REVISION') and defenv.has_key('VER_BUILD'):
	defenv['INSTVER'] += ' /DVER_MAJOR=$VER_MAJOR'
	defenv['INSTVER'] += ' /DVER_MINOR=$VER_MINOR'
	defenv['INSTVER'] += ' /DVER_REVISION=$VER_REVISION'
	defenv['INSTVER'] += ' /DVER_BUILD=$VER_BUILD'

installer_target = defenv.Command('nsis-${VERSION}${DISTSUFFIX}.exe',
                                  '$INSTDISTDIR' + os.sep + 'Examples' + os.sep + 'makensis.nsi',
                                  '$INSTDISTDIR' + os.sep + 'makensis$PROGSUFFIX ' +
                                  '/DOUTFILE=$TARGET.abspath $INSTVER $SOURCE')
defenv.Depends(installer_target, '$INSTDISTDIR')
defenv.Sign(installer_target)
defenv.Alias('dist-installer', installer_target)

AlwaysBuild(defenv.AddPostAction(installer_target, Delete('$INSTDISTDIR')))

defenv.Alias('dist', ['dist-zip', 'dist-installer'])

######################################################################
#######  Distribute Basics                                         ###
######################################################################

defenv.Distribute('', 'license.txt')
defenv.Distribute('', 'nsisconf.nsh')

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

	env.DistributeAs('Stubs/%s%s' % (compression, suffix), target)

	defenv.Alias(compression, target)
	defenv.Alias('stubs', target)

for stub in stubs:
	if stub in defenv['SKIPSTUBS']:
		continue

	BuildStub(stub, False)
	BuildStub(stub, True)

defenv.DistributeAs('Stubs/uninst', 'Source/exehead/uninst.ico')

######################################################################
#######  makensis                                                  ###
######################################################################

build_dir = '$BUILD_PREFIX/makensis'
exports = { 'env' : makensis_env }

makensis = defenv.SConscript(dirs = 'Source', build_dir = build_dir, duplicate = False, exports = exports)

makensis_env.SideEffect('%s/makensis.map' % build_dir, makensis)

defenv.Alias('makensis', makensis)

ins = defenv.Distribute('', makensis)
defenv.Alias('install-compiler', ins)

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
		env.DistributeExamples(target, examples)
	if docs:
		env.DistributeDocs(target, docs)

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

	env.Distribute('Plugins', plugin)

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
              nodeflib = False, install = None, install_as = None,
              examples = None, docs = None, cross_platform = False):
	if not cross_platform:
		env = util_env.Copy()
	else:
		env = cp_util_env.Copy()

	AddEnvStandardFlags(env, defines, flags, entry, nodeflib)

	AppendRES(env, source, res, resources)

	util = env.Program(target, source, LIBS = libs)
	defenv.Alias(target, util)
	defenv.Alias('utils', util)

	defenv.Sign(util)

	CleanMap(env, util, target)

	if install is not None:
		ins = env.Distribute(install, util)
		defenv.Alias('install-utils', ins)

	if install_as is not None:
		ins = env.DistributeAs(install_as, util)
		defenv.Alias('install-utils', ins)

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
exports = {'env' : defenv.Copy()}

defenv.SConscript(
	dirs = 'Source/Tests',
	duplicate = False,
	exports = exports,
	build_dir = build_dir
)

defenv.Ignore('$BUILD_PREFIX', '$BUILD_PREFIX/tests')

# test scripts

test_env = defenv.Copy(ENV = os.environ) # env needed for some scripts

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

test = test_env.Command('test-scripts.log', '$TESTDISTDIR', test_scripts)
test_env.Alias('test-scripts', test)

# test all

defenv.Alias('test', ['test-code', 'test-scripts'])
