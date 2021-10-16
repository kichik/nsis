/*
 * manifest.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/22/2007 
 */

#include "Platform.h"
#include "manifest.h"
#include <nsis-version.h>
#include "tstring.h"
#include "util.h" // RawTStrToASCII
#include <vector>

// Jim Park: The manifest must stay UTF-8.  Do not convert.

namespace manifest
{

using namespace std;

static bool isstrhexchars(const TCHAR*s,UINT cch)
{
  while(cch-- && *s)
  {
    const TCHAR c = *s++, clw = ((TCHAR)c) | 32;
    if (!(c >= '0' && c <= '9') && !(clw >= 'a' && clw <= 'f')) return false;
  }
  return true;
}

static const struct { const TCHAR *name, *guidstr; } g_soslmap[] = {
  { _T("WinVista"), _T("{e2011457-1546-43c5-a5fe-008deee3d3f0}") }, //msdn.microsoft.com/en-us/library/aa374191
  { _T("Win7"),     _T("{35138b9a-5d96-4fbd-8e2d-a2440225f93a}") }, //msdn.microsoft.com/en-us/library/dd371711
  { _T("Win8"),     _T("{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}") }, //msdn.microsoft.com/en-us/library/hh848036
  { _T("Win8.1"),   _T("{1f676c76-80e1-4239-95bb-83d0f6d0da78}") }, //msdn.microsoft.com/en-us/library/windows/desktop/dn481241
  { _T("Win10"),    _T("{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}") }  //blogs.msdn.com/b/chuckw/archive/2013/09/10/manifest-madness.aspx
};

bool SupportedOSList::addall()
{
  m_isdefaultlist = false;
  for (UINT i = 0; i < COUNTOF(g_soslmap); ++i)
    if (!append(g_soslmap[i].name))
      return false;
  return true;
}

bool SupportedOSList::append(const TCHAR* osid)
{
  const TCHAR *guid = 0;
  if ('{' == *osid)
  {
    if (38 == _tcsclen(osid) && '}' == osid[37]
     && '-' == osid[9] && '-' == osid[14] && '-' == osid[19] && '-' == osid[24]
     && isstrhexchars(osid+1,8) && isstrhexchars(osid+10,4)
     && isstrhexchars(osid+15,4) && isstrhexchars(osid+20,4)
     && isstrhexchars(osid+25,12)
     )
    {
      guid = osid;
    }
  }
  else
  {
    for (UINT i = 0; i < COUNTOF(g_soslmap); ++i)
      if (!_tcsicmp(osid, g_soslmap[i].name))
      {
        guid = g_soslmap[i].guidstr;
        break;
      }
  }

  if (guid)
  {
    m_list.add(guid,0);
    m_isdefaultlist = false;
    return true;
  }
  return false;
}


static const TCHAR*g_appendpaths[] = { // Basic simulated XPath support
  _T("/"),
  _T("/assembly"),
  _T("/assembly/dependency"),
  _T("/assembly/dependency/dependentAssembly"),
  _T("/assembly/compatibility/application"),
  _T("/assembly/application/windowsSettings")
};
std::vector<string> g_appendstrings[COUNTOF(g_appendpaths)];

static int isvalidappendpath(const TCHAR*path)
{
  for (int i = 0; i < (int) COUNTOF(g_appendpaths); ++i)
    if (!_tcsicmp(path, g_appendpaths[i]))
      return i;
  return -1;
}

bool addappendstring(const TCHAR*path, const TCHAR*data)
{
  int i = isvalidappendpath(path);
  if (i >= 0)
  {
    string str = "";
    str += TtoCString(data);
    g_appendstrings[i].push_back(str);
    return true;
  }
  return false;
}

static bool append(string& xml, const TCHAR*path, const char*prefix = 0, const char*suffix = 0)
{
  bool any = false;
  int i = isvalidappendpath(path);
  if (i >= 0)
    for (size_t j = 0; j < g_appendstrings[i].size(); ++j)
    {
      if (!any && prefix) xml += prefix, any = true;
      xml += g_appendstrings[i][j];
    }
  if (any && suffix)
    xml += suffix;
  return any;
}

string generate(comctl comctl_selection, exec_level exec_level_selection, const SPECIFICATION&spec)
{
  flags featureflags = spec.Flags;
  dpiaware dpia = spec.DPIA;
  const TCHAR *dpia2 = spec.DPIA2;
  longpathaware lpa = spec.lpaware;
  SupportedOSList& sosl = *spec.pSOSL;
  const TCHAR *mvt = spec.MaxVersionTested;

  bool default_or_empty_sosl = sosl.isdefaultlist() || !sosl.getcount(), any;
  if (comctl_selection == comctl_old && exec_level_selection == exec_level_none && default_or_empty_sosl && dpiaware_notset == dpia)
    return "";

  string xmltmp;
  string xml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\"><assemblyIdentity version=\"1.0.0.0\" processorArchitecture=\"*\" name=\"Nullsoft.NSIS.exehead\" type=\"win32\"/><description>Nullsoft Install System ";
  xml += TtoCString(NSIS_VERSION);
  xml += "</description>";

  if (comctl_selection == comctl_xp)
  {
    addappendstring(_T("/assembly/dependency/dependentAssembly"), _T("<assemblyIdentity type=\"win32\" name=\"Microsoft.Windows.Common-Controls\" version=\"6.0.0.0\" processorArchitecture=\"*\" publicKeyToken=\"6595b64144ccf1df\" language=\"*\" />"));
  }
  xmltmp = "<dependency>";
  any = append(xmltmp, _T("/assembly/dependency"));
  any |= append(xmltmp, _T("/assembly/dependency/dependentAssembly"), "<dependentAssembly>", "</dependentAssembly>");
  if (any)
  {
    xmltmp += "</dependency>";
    xml += xmltmp;
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
  else if (sosl.isdefaultlist())
  {
    // Don't add supportedOS list for exec_level_none to remain compatible with v2.46
    sosl.deleteall();
  }

  xmltmp = "", append(xmltmp, _T("/assembly/compatibility/application"));
  int soslcount = sosl.getcount();
  if (!xmltmp.empty() || soslcount || *mvt)
  {
    char buf[38+1];
    xml += "<compatibility xmlns=\"urn:schemas-microsoft-com:compatibility.v1\"><application>";
    while(soslcount--)
    {
      xml += "<supportedOS Id=\"";
      xml += (RawTStrToASCII(sosl.get(soslcount), buf, COUNTOF(buf)), buf);
      xml += "\"/>";
    }
    if (*mvt)
    {
      xml += "<maxVersionTested Id=\"";
      xml += TtoCString(mvt);
      xml += "\"/>";
    }
    xml += xmltmp;
    xml += "</application></compatibility>";
  }

  string xml_aws = ""; // <application><windowsSettings>
  if (featureflags & disablewindowfiltering)
  {
    xml_aws += "<disableWindowFiltering xmlns=\"http://schemas.microsoft.com/SMI/2011/WindowsSettings\">";
    xml_aws += "true";
    xml_aws += "</disableWindowFiltering>";
  }
  if (featureflags & gdiscaling)
  {
    xml_aws += "<gdiScaling xmlns=\"http://schemas.microsoft.com/SMI/2017/WindowsSettings\">";
    xml_aws += "true";
    xml_aws += "</gdiScaling>";
  }
  if (dpiaware_notset != dpia)
  {
    xml_aws += "<dpiAware xmlns=\"http://schemas.microsoft.com/SMI/2005/WindowsSettings\">";
    xml_aws += dpia == dpiaware_explorer ? "Explorer" : dpia >= dpiaware_permonitor ? "True/PM" : dpiaware_false != dpia ? "true" : "false";
    xml_aws += "</dpiAware>";
  }
  if (*dpia2)
  {
    xml_aws += "<dpiAwareness xmlns=\"http://schemas.microsoft.com/SMI/2016/WindowsSettings\">";
    xml_aws += TtoCString(dpia2);
    xml_aws += "</dpiAwareness>";
  }
  if (lpaware_notset != lpa)
  {
    xml_aws += "<longPathAware xmlns=\"http://schemas.microsoft.com/SMI/2016/WindowsSettings\">";
    xml_aws += lpaware_false != lpa ? "true" : "false"; 
    xml_aws += "</longPathAware>";
  }
  append(xml_aws, _T("/assembly/application/windowsSettings"));
  if (!xml_aws.empty())
  {
    xml += "<application xmlns=\"urn:schemas-microsoft-com:asm.v3\"><windowsSettings>";
    xml += xml_aws;
    xml += "</windowsSettings></application>";
  }

  append(xml, _T("/assembly"));
  xml += "</assembly>";
  append(xml, _T("/"));

  return xml;
}

};
