SCRIPT = """
  Name icon
  OutFile "%(icon)s X %(unicon)s.exe"
  SilentInstall silent
  SilentUninstall silent

  Icon "%(icon)s"
  UninstallIcon "%(unicon)s"

  Section
  WriteUninstaller "$EXEDIR\%(unicon)s X %(icon)s X uninst.exe"
  SectionEnd

  Section uninstall
  SectionEnd
"""

def build(icon, unicon):
  from os import popen2
  pw, pr = popen2(r'C:\Progra~1\NSIS\makensis.exe -')
  pw.write(SCRIPT % locals())
  pw.close()
  print pr.read()

from glob import glob
from os import system

icons = glob('*.ico')

for i in icons:
  for j in icons:
    build(i, j)
    system('"%s X %s.exe"' % (i, j))
