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

import struct
def FileUnpackRead(pack, size, f, fpos=None, defval=None):
	r = defval
	try:
		if not fpos is None: f.seek(fpos)
		r = struct.unpack(pack, f.read(size))[0]
	finally:
		return r
def ReadU16LE(f, fpos=None, defval=None):
	return FileUnpackRead("<H", 2, f, fpos, defval)
def ReadU32LE(f, fpos=None, defval=None):
	return FileUnpackRead("<I", 4, f, fpos, defval)
def WriteU16LE(f, v, fpos):
	if not fpos is None: f.seek(fpos)
	f.write(struct.pack("<H", v))
def WriteU32LE(f, v, fpos):
	if not fpos is None: f.seek(fpos)
	f.write(struct.pack("<I", v))

class MSPE:
	def __init__(self, path=None, open_for_write=False):
		self._f = None
		self._path = None
		self.NTHOffset = 0
		self.NTOHMagic = None
		self.IsPEP = False # PE+ A.K.A IMAGE_NT_HEADERS64
		if not path is None:
			self.Open(path, open_for_write)
	def __del__(self): self.Close()
	def Close(self):
		if getattr(self, '_f', None):
			self._f.close()
			self._f = None
	def Open(self, path, open_for_write=False):
		mode = "rb"
		if open_for_write: mode = "r+b"
		self._path = path
		f = self._f = open(path, mode)
		if not 0x5A4D == ReadU16LE(f, 0): return # IMAGE_DOS_SIGNATURE?
		fanew = ReadU32LE(f, 60)
		if not 0x00004550 == ReadU32LE(f, fanew): return # IMAGE_NT_SIGNATURE?
		self.NTHOffset = fanew
		self.NTOHMagic = ReadU16LE(f, fanew+4+20)
		self.IsPEP = 0x20b == self.NTOHMagic # IMAGE_NT_OPTIONAL_HDR64_MAGIC?
	def ReadMachine(self):
		return ReadU16LE(self._f, self.NTHOffset+4+0)
	def ReadCharacteristics(self):
		return ReadU16LE(self._f, self.NTHOffset+4+18)
	def WriteCharacteristics(self, value):
		WriteU16LE(self._f, value, self.NTHOffset+4+18)
	def ReadDllCharacteristics(self):
		return ReadU16LE(self._f, self.NTHOffset+4+20+70)
	def WriteDllCharacteristics(self, value):
		WriteU16LE(self._f, value, self.NTHOffset+4+20+70)
	def WriteChecksum(self, value):
		WriteU32LE(self._f, value, self.NTHOffset+4+20+64)

def IsPEExecutable(pe):
	if not isinstance(pe, MSPE): pe = MSPE(pe)
	if int(pe.ReadCharacteristics() or 0) & 0x0002: return True # IMAGE_FILE_EXECUTABLE_IMAGE?

def SetPESecurityFlagsWorker(filepath):
	"""
	Sets the [HE]ASLR, DEP and LAA flags in the PE header
	"""
	pe = MSPE(filepath, open_for_write=True)
	try:
		if not IsPEExecutable(pe): return
		ifh_c = pe.ReadCharacteristics()
		ifh_c |= 0x0020 # +IMAGE_FILE_LARGE_ADDRESS_AWARE
		pe.WriteCharacteristics(ifh_c)
		ioh_dc = pe.ReadDllCharacteristics()
		ioh_dc |= 0x0100 # +IMAGE_DLLCHARACTERISTICS_NX_COMPAT (DEP)
		if pe.ReadMachine() != 0xaa64: # ARM64 forces exception directory?
			ioh_dc |= 0x0400 # +IMAGE_DLLCHARACTERISTICS_NO_SEH
		ioh_dc |= 0x8000 # +IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE (TODO: Should we set this on .DLLs?)
		if not (ifh_c & 0x0001): # IMAGE_FILE_RELOCS_STRIPPED?
			ioh_dc |= 0x0040 # +IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE (ASLR)
			if pe.IsPEP: ioh_dc |= 0x0020 # +IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA (HEASLR)
		pe.WriteDllCharacteristics(ioh_dc)
		pe.WriteChecksum(0)
	finally:
		return

Export('AddAvailableLibs AddZLib FlagsConfigure GetAvailableLibs GetOptionOrEnv IsPEExecutable SetPESecurityFlagsWorker')
