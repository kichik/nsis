/////////////////////////////////////////////////////////////////////////////
// NSIS MENU
//
// Reviewed for Unicode support by Jim Park -- 08/23/2007
// Basically, compiling wxWidgets as Unicode should do it.
/////////////////////////////////////////////////////////////////////////////

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

// for all others, include the necessary headers (this file is usually all you
// need because it includes almost all "standard" wxWindows headers
#ifndef WX_PRECOMP
    #include <wx/wx.h>
#endif

#include <wx/event.h>
#include <wx/filefn.h>
#include <wx/image.h>
#include <wx/html/htmlwin.h>
#include <wx/html/htmlproc.h>
#include <wx/stdpaths.h>
#include <wx/utils.h>

#include <nsis-sconf.h>

// ----------------------------------------------------------------------------
// private classes
// ----------------------------------------------------------------------------

#if !wxCHECK_VERSION(2, 9, 0) && !defined(wxLaunchDefaultApplication)
#define wxLaunchDefaultApplication wxLaunchDefaultBrowser
#endif
#ifdef __WXMSW__
#define CanOpenChm() true
#else
#define CanOpenChm() false
#endif

typedef enum { SUT_UNKNOWN = 0, SUT_BIN = 0x14, SUT_DOC = 0x24, SUT_WEB = 0x34 } SPECIALURLTYPE; // The low nibble contains the "protocol" length!
SPECIALURLTYPE GetSpecialUrlType(const wxString&Url)
{
  const wxString l4 = Url.Left(4);
  if (0 == l4.CmpNoCase(wxT("BIN:"))) return SUT_BIN;
  if (0 == l4.CmpNoCase(wxT("DOC:"))) return SUT_DOC;
  if (0 == l4.CmpNoCase(wxT("WEB:"))) return SUT_WEB;
  return SUT_UNKNOWN;
}

static bool PathExists(const wxString&Path) { return wxFileExists(Path) || wxDirExists(Path); }
static wxString BuildPathWorker(const wxChar*a, const wxChar*b)
{
  wxString path(a);
  if (path.Last() != wxFileName::GetPathSeparator()) path.Append(wxFileName::GetPathSeparator());
  return (path.Append(b), path);
}
static const wxChar*GetCStr(const wxString&t) { return t.c_str(); }
static const wxChar*GetCStr(const wxChar*t) { return t; }
template<class A, class B> wxString BuildPath(const A&a, const B&b) { return BuildPathWorker(GetCStr(a), GetCStr(b)); }
template<class A, class B, class C> wxString BuildPath(const A&a, const B&b, const C&c) { return BuildPath(BuildPath(GetCStr(a), GetCStr(b)), GetCStr(c)); }

wxString GetMenuHtmlFile(const wxChar*file)
{
#ifdef __WXMSW__
  wxString dataroot(wxPathOnly(wxStandardPaths::Get().GetExecutablePath()));
#else
  const wxChar*dataroot = wxT(PREFIX_DATA);
#endif
  return BuildPath(dataroot, wxT("Menu"), file);
}

SPECIALURLTYPE TransformUrl(wxString&Url)
{
  SPECIALURLTYPE ut = GetSpecialUrlType(Url);
  const wxString location = Url.Mid(ut & 0x0F);
  const wxString exePath = wxPathOnly(wxStandardPaths::Get().GetExecutablePath());

  if (SUT_BIN == ut)
  {
#ifdef __WXMSW__
    Url = BuildPath(exePath, location) + wxT(".exe");
    if (!PathExists(Url))
      Url = BuildPath(exePath, wxT("Bin"), location) + wxT(".exe");
#else
    Url = BuildPath(exePath, location);
#endif
  }
  else if (SUT_DOC == ut)
  {
#ifdef __WXMSW__
    wxString path = BuildPath(exePath, location);
#else
    wxString path = BuildPath(wxT(PREFIX_DOC), location);
#endif
    if ((!CanOpenChm() || !wxFileExists(path)) && 0 == location.CmpNoCase(wxT("NSIS.chm")))
    {
      path = BuildPath(wxPathOnly(path), wxT("Docs"), wxT("Manual.html")); // DOCTYPES=htmlsingle
      if (!wxFileExists(path))
        path = BuildPath(wxPathOnly(path), wxT("Contents.html")); // DOCTYPES=html (Not adding /Docs/ because it has already been appended)
    }
    Url = path;
  }
  else if (SUT_WEB == ut)
  {
    Url = location;
  }
  return ut;
}


// Define a new application type, each program should derive a class from wxApp
class MyApp : public wxApp
{
public:
 // override base class virtuals
 // ----------------------------

 // this one is called on application startup and is a good place for the app
 // initialization (doing it here and not in the ctor allows to have an error
 // return: if OnInit() returns false, the application terminates)
   virtual bool OnInit();
};

// Define a new frame type: this is going to be our main frame
class MyFrame : public wxFrame
{
public:
 // ctor(s)
    MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size);
 // event handler(s)
    void OnLink(wxHtmlLinkEvent& event);
    void OnClose(wxCloseEvent& event);

private:
     wxHtmlWindow *m_Html;

 // any class wishing to process wxWindows events must use this macro
 DECLARE_EVENT_TABLE()
};

// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

#define HTMLW 598 // release.py generates a 598x45 header image
#define HTMLH 365

// IDs for the controls and the menu commands
   enum
   {
    // controls start here (the numbers are, of course, arbitrary)
   HtmlControl = 1000
   };

// ----------------------------------------------------------------------------
// event tables and other macros for wxWindows
// ----------------------------------------------------------------------------

// the event tables connect the wxWindows events with the functions (event
// handlers) which process them. It can be also done at run-time, but for the
// simple menu events like this the static method is much simpler.
   BEGIN_EVENT_TABLE(MyFrame, wxFrame)
     EVT_HTML_LINK_CLICKED(HtmlControl, MyFrame::OnLink)
   END_EVENT_TABLE()
   
   // Create a new application object: this macro will allow wxWindows to create
   // the application object during program execution (it's better than using a
   // static object for many reasons) and also declares the accessor function
   // wxGetApp() which will return the reference of the right type (i.e. MyApp and
   // not wxApp)
   IMPLEMENT_APP(MyApp)
   
   // ============================================================================
   // implementation
   // ============================================================================
   
   // ----------------------------------------------------------------------------
   // the application class
   // ----------------------------------------------------------------------------
   // `Main program' equivalent: the program execution "starts" here
   bool MyApp::OnInit()
   {
     wxInitAllImageHandlers();

     // Create the main application window
     MyFrame *frame = new MyFrame(_("NSIS Menu"),
         wxPoint(50, 50), wxSize(HTMLW + wxSystemSettings::GetMetric(wxSYS_FRAMESIZE_X), HTMLH + wxSystemSettings::GetMetric(wxSYS_FRAMESIZE_Y)));
   
     // Show it and tell the application that it's our main window

     frame->SetClientSize(HTMLW, HTMLH);
     frame->Show(TRUE);
     SetTopWindow(frame);
   
     // success: wxApp::OnRun() will be called which will enter the main message
     // loop and the application will run. If we returned FALSE here, the
     // application would exit immediately.
     return TRUE;
   }

// ----------------------------------------------------------------------------
// main frame
// ----------------------------------------------------------------------------
#ifdef NSISMENU_NOLINKTAGHANDLER
static wxHtmlWindow*g_pHtmlWindow = 0;
#endif

// frame constructor
   MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
   : wxFrame((wxFrame *)NULL, -1, title, pos, size, wxCLOSE_BOX | wxMINIMIZE_BOX | wxSYSTEM_MENU | wxCAPTION,
             wxT("nsis_menu"))
   {  
      m_Html = new wxHtmlWindow(this, HtmlControl, wxPoint(0, 0), wxSize(HTMLW, HTMLH), wxHW_SCROLLBAR_NEVER|wxHW_NO_SELECTION);
      m_Html->SetRelatedFrame(this, wxT("%s")); // Dialog caption comes from the html title element or filename
      m_Html->SetBorders(0);
      m_Html->EnableScrolling(false, false);
#ifdef NSISMENU_NOLINKTAGHANDLER
      g_pHtmlWindow = m_Html;
#endif
      
      // Set font size
      wxSize DialogSize(1000, 1000);
      DialogSize = this->ConvertDialogToPixels(DialogSize);
      int fonts[7] = {0, 0, 14000 / (DialogSize.GetWidth()), 19000 / (DialogSize.GetWidth()), 0, 0, 0};
      m_Html->SetFonts(wxString(), wxString(), fonts);
      
#ifdef __WXMSW__
      this->SetIcon(wxICON(nsisicon));
#else
      wxIcon icon(wxT("nsisicon.ico"), wxBITMAP_TYPE_ICO);
      this->SetIcon(icon);
#endif
      m_Html->LoadPage(GetMenuHtmlFile(wxT("index.html")));
      this->Centre(wxBOTH);
   }

// event handler

void MyFrame::OnLink(wxHtmlLinkEvent& event)
{
  const wxMouseEvent *e = event.GetLinkInfo().GetEvent();
  if (e == NULL || e->LeftUp())
  {
    int notinstalled = false;
    wxString href = event.GetLinkInfo().GetHref(), url = href;
    SPECIALURLTYPE ut = TransformUrl(url);
    switch(ut)
    {
    case SUT_BIN:
      if (PathExists(url)) wxExecute(url); else ++notinstalled;
      break;
    case SUT_DOC:
      if (PathExists(url)) wxLaunchDefaultApplication(url); else ++notinstalled;
      break;
    case SUT_WEB:
      wxLaunchDefaultBrowser(url);
      break;
    default:
      event.Skip();
    }
#ifdef NSISMENU_NOLINKTAGHANDLER
    if (notinstalled && g_pHtmlWindow)
      g_pHtmlWindow->LoadPage(GetMenuHtmlFile(wxT("notinstalled.html")));
#endif
  }
}

