/*
 * manifest.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "Platform.h"
#include "manifest.h"
#include "version.h"

namespace manifest
{

using namespace std;

string generate(comctl comctl_selection, exec_level exec_level_selection)
{
  if (comctl_selection == comctl_old && exec_level_selection == exec_level_none)
    return "";

  string xml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\"><assemblyIdentity version=\"1.0.0.0\" processorArchitecture=\"X86\" name=\"Nullsoft.NSIS.exehead\" type=\"win32\"/><description>Nullsoft Install System " NSIS_VERSION "</description>";

  if (comctl_selection == comctl_xp)
  {
    xml += "<dependency><dependentAssembly><assemblyIdentity type=\"win32\" name=\"Microsoft.Windows.Common-Controls\" version=\"6.0.0.0\" processorArchitecture=\"X86\" publicKeyToken=\"6595b64144ccf1df\" language=\"*\" /></dependentAssembly></dependency>";
  }

  if (exec_level_selection != exec_level_none)
  {
    string level = "";

    switch (exec_level_selection)
    {
    case exec_level_none:
      break;
    case exec_level_user:
      level = "asInvoker";
      break;
    case exec_level_highest:
      level = "highestAvailable";
      break;
    case exec_level_admin:
      level = "requireAdministrator";
      break;
    }

    xml += "<trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\"><security><requestedPrivileges><requestedExecutionLevel level=\"";
    xml += level;
    xml += "\" uiAccess=\"false\"/></requestedPrivileges></security></trustInfo>";
  }

  xml += "</assembly>";

  return xml;
}

};
