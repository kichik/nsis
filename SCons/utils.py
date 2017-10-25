def AddAvailableLibs(env, libs):
	"""
	Scans through a list of libraries and adds
	available libraries to the environment.
	"""
	conf = env.Configure()

	for lib in libs:
		conf.CheckLib(lib)

	conf.Finish()

def AddZLib(env, platform, alias='install-utils'):
	"""
	Checks for platform specific zlib and adds the 
	appropriate compiler and linker options to the environment
	"""
	zlib = 'z'
	if platform == 'win32':
		if 'ZLIB_W32' in env:
			# Add include and library path of zlib for Win32
			env.Append(CPPPATH = env['ZLIB_W32_INC'])
			env.Append(LIBPATH = env['ZLIB_W32_LIB'])
			zlib = ['zdll', 'z']
			if 'ZLIB_W32_DLL' in env and env['ZLIB_W32_DLL']:
				env.DistributeW32Bin(env['ZLIB_W32_DLL'], alias=alias)
			if 'ZLIB_W32_NEW_DLL' in env and env['ZLIB_W32_NEW_DLL']:
				env.DistributeW32Bin(env['ZLIB_W32_NEW_DLL'], alias=alias)
		else:
			print('Please specify folder of zlib for Win32 via ZLIB_W32')
			Exit(1)

	# Avoid unnecessary configuring when cleaning targets 
	# and a clash when scons is run in parallel operation.
	if not env.GetOption('clean'):
		conf = env.Configure()
		if not conf.CheckLibWithHeader(zlib, 'zlib.h', 'c'):
			print('zlib (%s) is missing!' % (platform))
			Exit(1)

		env = conf.Finish()


def GetAvailableLibs(env, libs):
	"""
	Scans through a list list of libraries and adds
	available libraries to the environment.
	"""
	conf = env.Configure()
	avail_libs = []

	for lib in libs:
		if conf.CheckLib(lib):
			avail_libs.append(lib)

	conf.Finish()

	return avail_libs

def check_compile_flag(ctx, flag):
	"""
	Checks if a compiler flag is valid.
	"""
	ctx.Message('Checking for compiler flag %s... ' % flag)

	old_flags = ctx.env['CCFLAGS']
	ctx.env.Append(CCFLAGS = [flag])

	test = """
		int main() {
			return 0;
		}
	"""

	result = ctx.TryCompile(test, '.c')
	ctx.Result(result)

	if not result:
		ctx.env.Replace(CCFLAGS = [old_flags])

	return result

def check_link_flag(ctx, flag, run = 0, extension = '.c', code = None):
	"""
	Checks if a linker flag is valid.
	"""
	ctx.Message('Checking for linker flag %s... ' % flag)

	old_flags = ctx.env['LINKFLAGS']
	ctx.env.Append(LINKFLAGS = [flag])

	if code:
		test =  code
	else:
		test = """
			int main() {
				return 0;
			}
		"""

	result = ctx.TryLink(test, extension)

	if run:
		result = result and ctx.TryRun(test, extension)[0]

	ctx.Result(result)

	if not result:
		ctx.env.Replace(LINKFLAGS = [old_flags])

	return result

def FlagsConfigure(env):
	"""
	Wrapper for env.Configure which adds two new tests:
	  CheckCompileFlag - checks for a compiler flag
		CheckLinkFlag    - checks for a linker flag
	"""
	return env.Configure(custom_tests = { 'CheckCompileFlag' : check_compile_flag, 'CheckLinkFlag': check_link_flag })

def GetOptionOrEnv(name, defval = None):
	"""
	Get option set on scons command line or in os.environ
	"""
	import os
	#if optenv and optenv.has_key(name):
	#	return optenv[name]
	if ARGUMENTS.has_key(name):
		return ARGUMENTS[name]
	if os.environ.has_key(name):
		return os.environ[name]
	return defval

def EnablePESecurityFlagsHelper(filepath):
	"""
	Sets the [HE]ASLR, DEP and LAA flags in the PE header
	"""
	import struct
	def ReadU16LE(f, fpos):
		if not fpos is None: f.seek(fpos)
		return struct.unpack("<H", f.read(2))[0]
	def ReadU32LE(f, fpos):
		if not fpos is None: f.seek(fpos)
		return struct.unpack("<I", f.read(4))[0]
	def WriteU16LE(f, v, fpos):
		if not fpos is None: f.seek(fpos)
		f.write(struct.pack("<H", v))
	f = open(filepath, "r+b")
	try:
		if not 0x5A4D == ReadU16LE(f, 0): return
		pepos = ReadU32LE(f, 60)
		if not 0x00004550 == ReadU32LE(f, pepos): return
		pe64 = 0x20b == ReadU16LE(f, pepos+4+20) # IMAGE_NT_OPTIONAL_HDR64_MAGIC?
		ifh_c = ReadU16LE(f, pepos+4+18)
		ifh_c |= 0x0020 # +IMAGE_FILE_LARGE_ADDRESS_AWARE
		WriteU16LE(f, ifh_c, pepos+4+18)
		ioh_dc = ReadU16LE(f, pepos+4+20+70)
		ioh_dc |= 0x0100 # +IMAGE_DLLCHARACTERISTICS_NX_COMPAT
		ioh_dc |= 0x0400 # +IMAGE_DLLCHARACTERISTICS_NO_SEH
		if not (ifh_c & 0x0001): # IMAGE_FILE_RELOCS_STRIPPED?
			ioh_dc |= 0x0040 # +IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
			if pe64: ioh_dc |= 0x0020 # +IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
		# TODO: IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE?
		WriteU16LE(f, ioh_dc, pepos+4+20+70)
	finally:
		f.close()

Export('AddAvailableLibs AddZLib FlagsConfigure GetAvailableLibs GetOptionOrEnv EnablePESecurityFlagsHelper')
