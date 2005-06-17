## TODO
#
#   * VPatch GenPat & distribution
#   * Write SConscript for NSIS Menu
#    - Use inheritance instead of current wxWidgets patches
#    - Compile for POSIX too? wxWidgets is cross platform after all...
#   * Write SConscript for Library RegTool (needs to compile makensis with smaller configuration)
#
##

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
	# special makensis 'Library',
	'Library/LibraryLocal',
	'MakeLangId',
	'Makensisw',
	'NSIS Menu',
	'NSIS Update',
	'zip2exe'
]

misc = [
	'Graphics',
	'Language files',
	'Modern UI',
	'UIs'
]

defenv = Environment()
defenv.SConsignFile()
Export('defenv')

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
opts.Add(('VERSION', 'Version of NSIS', cvs_version))
opts.Add(PathOption('PREFIX', 'Installation prefix', None))
opts.Add(BoolOption('MSTOOLKIT', 'Use Microsoft Visual C++ Toolkit', 'no'))
opts.Add(BoolOption('DEBUG', 'Build executables with debugging information', 'no'))
opts.Add(BoolOption('CHMDOCS', 'Build CHM documentation, requires hhc.exe', hhc))
opts.Add(PathOption('CPPPATH', 'Path to search for include files', None))
opts.Add(PathOption('LIBPATH', 'Path to search for libraries', None))
opts.Add(ListOption('SKIPSTUBS', 'A list of stubs that will not be built', 'none', stubs))
opts.Add(ListOption('SKIPPLUGINS', 'A list of plug-ins that will not be built', 'none', plugins))
opts.Add(ListOption('SKIPUTILS', 'A list of utilities that will not be built', 'none', utils))
opts.Add(ListOption('SKIPMISC', 'A list of plug-ins that will not be built', 'none', misc))
opts.Update(defenv)

Help(opts.GenerateHelpText(defenv))

######################################################################
#######  Functions                                                 ###
######################################################################

defenv['DISTDIR'] = defenv.Dir('#nsis-$VERSION')

def Distribute(dir, files):
	if defenv.has_key('PREFIX') and defenv['PREFIX']:
		ins = defenv.Install('$PREFIX/%s' % dir, files)
		return ins
	defenv.Install('$DISTDIR/%s' % dir, files)
	return []

def DistributeAs(path, file):
	if defenv.has_key('PREFIX') and defenv['PREFIX']:
		ins = defenv.InstallAs('$PREFIX/%s' % path, file)
		return ins
	defenv.InstallAs('$DISTDIR/%s' % path, file)
	return []

def DistributeExamples(dir, examples):
	return Distribute('Examples/%s' % dir, examples)

def DistributeDocs(dir, docs):
	return Distribute('Docs/%s' % dir, docs)

def DistributeContribs(dir, contribs):
	return Distribute('Contrib/%s' % dir, contribs)

defenv.Distribute = Distribute
defenv.DistributeAs = DistributeAs
defenv.DistributeExamples = DistributeExamples
defenv.DistributeDocs = DistributeDocs
defenv.DistributeContribs = DistributeContribs

######################################################################
#######  Environments                                              ###
######################################################################

if defenv['DEBUG']:
	defenv.Replace(BUILD_PREFIX = 'build/debug')
else:
	defenv.Replace(BUILD_PREFIX = 'build/release')

if defenv['MSTOOLKIT']:
	defenv.Tool('mstoolkit', toolpath = ['SCons/Tools'])

Default(defenv['BUILD_PREFIX'])

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

dist_zip = 'nsis-${VERSION}.zip'
zip_target = defenv.Zip(dist_zip, '$DISTDIR')
delete_action = defenv.AddPostAction(zip_target, Delete('$DISTDIR'))
AlwaysBuild(delete_action)
defenv.Alias('dist', dist_zip)

######################################################################
#######  Stubs                                                     ###
######################################################################

for stub in stubs:
	if stub in defenv['SKIPSTUBS']:
		continue

	build_dir = '$BUILD_PREFIX/stub_%s' % stub
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '$MAP_FLAG')
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 0 }

	target = defenv.SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect('%s/stub_%s.map' % (build_dir, stub), target)

	defenv.DistributeAs('Stubs/%s' % stub, target)

	build_dir = '$BUILD_PREFIX/stub_%s_solid' % stub
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '$MAP_FLAG')
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 1 }

	solid_target = defenv.SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect('%s/stub_%s.map' % (build_dir, stub), solid_target)

	defenv.DistributeAs('Stubs/%s_solid' % stub, solid_target)

	defenv.Alias(stub, target + solid_target)
	defenv.Alias('stubs', target + solid_target)

defenv.DistributeAs('Stubs/uninst', 'Source/exehead/uninst.ico')

######################################################################
#######  makensis                                                  ###
######################################################################

build_dir = '$BUILD_PREFIX/makensis'
exports = { 'env' : makensis_env }

makensis_env.Append(LINKFLAGS = '$MAP_FLAG')

makensis = defenv.SConscript(dirs = 'Source', build_dir = build_dir, duplicate = 0, exports = exports)

makensis_env.SideEffect('%s/makensis.map' % build_dir, makensis)

defenv.Alias('makensis', makensis)

ins = defenv.Distribute('', makensis)
defenv.Alias('install-compiler', ins)

######################################################################
#######  Plug-ins                                                  ###
######################################################################

def BuildPlugin(target, source, libs, examples = None, docs = None,
                entry = 'DllMain', res = None, res_target = None,
                resources = None, defines = None, flags = None, 
                nodeflib = 1, cppused = 0):
	env = plugin_env.Copy()

	if cppused and env['CPP_REQUIRES_STDLIB']:
		nodeflib = 0

	if defines:
		env.Append(CPPDEFINES = defines)
	if flags:
		env.Append(CCFLAGS = flags)

	if entry:
		env.Append(LINKFLAGS = '${ENTRY_FLAG("%s")}' % entry)

	if nodeflib:
		env.Append(LINKFLAGS = '$NODEFLIBS_FLAG') # no default libraries

	env.Append(LINKFLAGS = '$MAP_FLAG')

	if res:
		target_res = env.RES(res_target, res)
		if resources:
			env.Depends(target_res, resources)
		source = source + target_res

	plugin = env.SharedLibrary(target, source, LIBS = libs)
	defenv.Alias(target, plugin)
	defenv.Alias('plugins', plugin)

	env.Clean(plugin, File(target + '.map'))

	env.Distribute('Plugins', plugin)

	if examples:
		env.DistributeExamples(target, examples)
	if docs:
		env.DistributeDocs(target, docs)

for plugin in plugins:
	if plugin in defenv['SKIPPLUGINS']:
		continue

	path = 'Contrib/' + plugin
	build_dir = '$BUILD_PREFIX/' + plugin
	exports = {'BuildPlugin' : BuildPlugin, 'env' : plugin_env.Copy()}

	defenv.SConscript(dirs = path, build_dir = build_dir, duplicate = 0, exports = exports)

######################################################################
#######  Utilities                                                 ###
######################################################################

def BuildUtil(target, source, libs, entry = None, res = None, 
              resources = None, defines = None, flags = None,
              install = None):
	env = util_env.Copy()

	if defines:
		env.Append(CPPDEFINES = defines)
	if flags:
		env.Append(CCFLAGS = flags)

	if entry:
		env.Append(LINKFLAGS = '${ENTRY_FLAG("%s")}' % entry)

	env.Append(LINKFLAGS = '$MAP_FLAG')

	if res:
		target_res = env.RES(res)
		if resources:
			env.Depends(target_res, resources)
		source = source + target_res

	util = env.Program(target, source, LIBS = libs)
	defenv.Alias(target, util)
	defenv.Alias('utils', util)

	env.Clean(util, File(target + '.map'))

	if install is not None:
		ins = env.Distribute(install, util)
		defenv.Alias('install-utils', ins)

for util in utils:
	if util in defenv['SKIPUTILS']:
		continue

	path = 'Contrib/' + util
	build_dir = '$BUILD_PREFIX/' + util
	exports = {'BuildUtil' : BuildUtil, 'env' : util_env.Copy()}

	defenv.SConscript(dirs = path, build_dir = build_dir, duplicate = 0, exports = exports)

######################################################################
#######  Documentation                                             ###
######################################################################

halibut = defenv.SConscript(
	dirs = 'Docs/src/bin/halibut',
	build_dir = '$BUILD_PREFIX/halibut',
	duplicate = 0,
	exports = {'env' : defenv.Copy()}
)

if defenv['CHMDOCS']:
	defenv.SConscript(
		dirs = 'Docs/src',
		build_dir = '$BUILD_PREFIX/Docs/chm',
		duplicate = 0,
		exports = {'halibut' : halibut, 'env' : defenv.Copy(), 'build_chm' : 1}
	)
else:
	defenv.SConscript(
		dirs = 'Docs/src',
		build_dir = '$BUILD_PREFIX/Docs/html',
		duplicate = 0,
		exports = {'halibut' : halibut, 'env' : defenv.Copy(), 'build_chm' : 0}
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
