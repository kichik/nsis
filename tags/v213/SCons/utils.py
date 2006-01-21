"""
Scans through a list list of libraries and adds
available libraries to the environment.
"""
def AddAvailableLibs(env, libs):
	conf = env.Configure()

	for lib in libs:
		conf.CheckLib(lib)

	conf.Finish()

Export('AddAvailableLibs')
