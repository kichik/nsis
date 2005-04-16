## TODO
#
#   * VPatch GenPat (wait for v3 written in C)
#   * Get Math to Compile
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
	# compile errors 'Math',
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

prefix = GetLaunchDir() + os.sep
build_prefix = 'build' + os.sep

######################################################################
#######  environments                                              ###
######################################################################

defenv = Environment()
Export('defenv')

opts = Options()
opts.Add(('MINGWPREFIX', 'MinGW toolset prefix'))
opts.Add(BoolOption('MSTOOLKIT', 'Use Microsoft Visual C++ Toolkit', 'no'))
opts.Add(BoolOption('DEBUG', 'Build executables with debugging information', 'no'))
opts.Update(defenv)

Help(opts.GenerateHelpText(defenv))

if defenv['MSTOOLKIT']:
	defenv.Tool('mstoolkit', toolpath = ['SCons/Tools'])

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
#######  stubs                                                     ###
######################################################################

for stub in stubs:
	build_dir = build_prefix + 'stub_' + stub
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % ('stub_' + stub))
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 0 }

	target = SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect(File(build_dir + os.sep + 'stub_' + stub + '.map'), target)

	ins_target = InstallAs(prefix + '/Stubs/' + stub, target)

	build_dir = build_prefix + 'stub_' + stub + '_solid'
	env = stub_env.Copy()
	env.Append(LINKFLAGS = '${MAP_FLAG("%s")}' % ('stub_' + stub))
	exports = { 'env' : env, 'compression' : stub, 'solid_compression' : 1 }

	solid_target = SConscript(dirs = 'Source/exehead', build_dir = build_dir, duplicate = 0, exports = exports)
	env.SideEffect(File(build_dir + os.sep + 'stub_' + stub + '.map'), target)

	ins_solid_target = InstallAs(prefix + '/Stubs/' + stub + '_solid', solid_target)

	env.Alias(stub, ins_target + ins_solid_target)

uninst_icon = InstallAs(prefix + '/Stubs/' + 'uninst', 'Source/exehead/uninst.ico')

Alias('stubs', [stubs, uninst_icon])

######################################################################
#######  makensis                                                  ###
######################################################################

build_dir = build_prefix + 'makensis'
exports = { 'env' : makensis_env }

makensis_env.Append(LINKFLAGS = '${MAP_FLAG("makensis")}')

makensis = SConscript(dirs = 'Source', build_dir = build_dir, duplicate = 0, exports = exports)

makensis_env.Clean(makensis, 'makensis.map')

Alias('makensis', Install(prefix, makensis))

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
	path = 'Contrib' + os.sep + plugin
	build_dir = build_prefix + plugin
	exports = 'PluginEnv'

	plugin_dll = SConscript(dirs = path, build_dir = build_dir, duplicate = 0, exports = exports)

	Alias(plugin, Install(prefix + 'Plugins', plugin_dll))

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

	env.Clean(util, File(target + '.map'))

	if install is not None:
		ins = Install(prefix + install, util)
		Alias(target, ins)
	else:
		Alias(target, util)

for util in utils:
	path = 'Contrib' + os.sep + util
	build_dir = build_prefix + util
	exports = {'BuildUtil' : BuildUtil, 'env' : util_env.Copy()}

	SConscript(dirs = path, build_dir = build_dir, duplicate = 0, exports = exports)

######################################################################
#######  Documentation                                             ###
######################################################################

halibut = SConscript(
	dirs = 'Docs/src/bin/halibut',
	build_dir = build_prefix + 'halibut',
	duplicate = 0
)

SConscript(
	dirs = 'Docs/src',
	build_dir = build_prefix + 'Docs',
	duplicate = 0,
	exports = {'halibut' : halibut, 'prefix' : prefix}
)
