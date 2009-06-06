def AddAvailableLibs(env, libs):
	"""
	Scans through a list list of libraries and adds
	available libraries to the environment.
	"""
	conf = env.Configure()

	for lib in libs:
		conf.CheckLib(lib)

	conf.Finish()

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

Export('AddAvailableLibs FlagsConfigure GetAvailableLibs')
