## TODO
#
#   * VPatch GenPat (wait for v3 written in C)
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
	# wxWidgets 'NSIS Menu',
	'NSIS Update',
	'zip2exe'
]

import os

defenv = Environment()
Export('defenv')

######################################################################
#######  options                                                   ###
######################################################################

opts = Options()
opts.Add(PathOption('PREFIX', 'Installation prefix', GetLaunchDir()))
opts.Add(('MINGWPREFIX', 'MinGW toolset prefix', 0))
opts.Add(BoolOption('MSTOOLKIT', 'Use Microsoft Visual C++ Toolkit', 'no'))
opts.Add(BoolOption('DEBUG', 'Build executables with debugging information', 'no'))
opts.Add(PathOption('CPPPATH', 'Path to search for include files', None))
opts.Add(PathOption('LIBPATH', 'Path to search for libraries', None))
opts.Update(defenv)

Help(opts.GenerateHelpText(defenv))

if defenv['DEBUG']:
	defenv.Replace(BUILD_PREFIX = 'build/debug')
else:
	defenv.Replace(BUILD_PREFIX = 'build/release')

if defenv['MSTOOLKIT']:
	defenv.Tool('mstoolkit', toolpath = ['SCons/Tools'])

######################################################################
#######  environments                                              ###
######################################################################

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
#######  aliases                                                   ###
######################################################################

defenv.Alias('install', '$PREFIX')

######################################################################
#######  stubs                                                     ###
######################################################################

for stub in stubs:
	build_dir = '$BUILD_PREFIX/stub_%s' % stub
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % ('stub_' + stub))
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 0 }

	target = defenv.SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect('%s/stub_%s.map' % (build_dir, stub), target)

	ins_target = defenv.InstallAs('$PREFIX/Stubs/%s' % stub, target)

	build_dir = '$BUILD_PREFIX/stub_%s_solid' % stub
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % ('stub_' + stub))
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 1 }

	solid_target = defenv.SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect('%s/stub_%s.map' % (build_dir, stub), solid_target)

	ins_solid_target = defenv.InstallAs('$PREFIX/Stubs/%s_solid' % stub, solid_target)

	env.Alias(stub, target + solid_target)

uninst_icon = defenv.InstallAs('$PREFIX/Stubs/uninst', 'Source/exehead/uninst.ico')

Alias('stubs', [stubs, uninst_icon])

######################################################################
#######  makensis                                                  ###
######################################################################

build_dir = '$BUILD_PREFIX/makensis'
exports = { 'env' : makensis_env }

makensis_env.Append(LINKFLAGS = '${MAP_FLAG("makensis")}')

makensis = defenv.SConscript(dirs = 'Source', build_dir = build_dir, duplicate = 0, exports = exports)

makensis_env.SideEffect('%s/makensis.map' % build_dir, makensis)

Alias('makensis', makensis)

defenv.Install('$PREFIX', makensis)

######################################################################
#######  Plug-ins                                                  ###
######################################################################

def PluginEnv(target, entry = 'DllMain', nodeflib = 1):
	env = plugin_env.Copy()

	if nodeflib:
		env.Append(LINKFLAGS = '$NODEFLIBS_FLAG') # no default libraries

	env.Append(LINKFLAGS = '${ENTRY_FLAG("%s")}' % entry) # entry function
	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % target)  # generate map file

	env.SideEffect(File(target + '.map'), target)

	return env

for plugin in plugins:
	path = 'Contrib/' + plugin
	build_dir = '$BUILD_PREFIX/' + plugin
	exports = 'PluginEnv'

	plugin_dll = defenv.SConscript(dirs = path, build_dir = build_dir, duplicate = 0, exports = exports)

	defenv.Install('$PREFIX/Plugins', plugin_dll)

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
		env.Append(CPPFLAGS = flags)

	if entry:
		env.Append(LINKFLAGS = '${ENTRY_FLAG("%s")}' % entry)

	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % target)

	if res:
		target_res = env.RES(res)
		if resources:
			env.Depends(target_res, resources)
		source = source + target_res

	util = env.Program(target, source, LIBS = libs)
	Alias(target, util)

	env.Clean(util, File(target + '.map'))

	if install is not None:
		defenv.Install('$PREFIX/%s' % install, util)

for util in utils:
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
	duplicate = 0
)

defenv.SConscript(
	dirs = 'Docs/src',
	build_dir = '$BUILD_PREFIX/Docs',
	duplicate = 0,
	exports = {'halibut' : halibut, 'env' : defenv.Copy()}
)
