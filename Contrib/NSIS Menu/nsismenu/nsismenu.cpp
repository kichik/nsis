/////////////////////////////////////////////////////////////////////////////
// NSIS MENU
/////////////////////////////////////////////////////////////////////////////

#if defined(__GNUG__) && !defined(__APPLE__)
    #pragma implementation "test.cpp"
    #pragma interface "test.cpp"
#endif

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

// for all others, include the necessary headers (this file is usually all you
// need because it includes almost all "standard" wxWindows headers
#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include "wx/image.h"
#include "wx/html/htmlwin.h"
#include "wx/html/htmlproc.h"

// ----------------------------------------------------------------------------
// private classes
// ----------------------------------------------------------------------------

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

private:
     wxHtmlWindow *m_Html;

 // any class wishing to process wxWindows events must use this macro
 DECLARE_EVENT_TABLE()
};

// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

// IDs for the controls and the menu commands
   enum
   {
    // controls start here (the numbers are, of course, arbitrary)
   Minimal_Text = 1000
   };

// ----------------------------------------------------------------------------
// event tables and other macros for wxWindows
// ----------------------------------------------------------------------------

// the event tables connect the wxWindows events with the functions (event
// handlers) which process them. It can be also done at run-time, but for the
// simple menu events like this the static method is much simpler.
   BEGIN_EVENT_TABLE(MyFrame, wxFrame)
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
         wxPoint(50, 50), wxSize(612 + (GetSystemMetrics(SM_CXEDGE) * 2), 353 + GetSystemMetrics(SM_CYSIZE) + (GetSystemMetrics(SM_CXEDGE) * 2)));
   
    // Show it and tell the application that it's our main window

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


// frame constructor
   MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
   : wxFrame((wxFrame *)NULL, -1, title, pos, size, wxMINIMIZE_BOX | wxSYSTEM_MENU | wxCAPTION,
             wxT("nsis_menu"))
   {  
      m_Html = new wxHtmlWindow(this);
      m_Html->SetRelatedFrame(this, _("NSIS Menu"));
	  m_Html->SetBorders(0);
	  
	  // Set font size
	  wxWindow UnitConvert;
	  wxSize DialogSize(1000, 1000);
	  DialogSize = UnitConvert.ConvertDialogToPixels(DialogSize);
	  int fonts[7] = {0, 0, 20000 / (DialogSize.GetWidth()), 25000 / (DialogSize.GetWidth()), 0, 0, 0};
	  m_Html->SetFonts("", "", fonts);
	  
	  m_Html->LoadPage(wxT("Menu/index.html"));
	  
	  this->Centre(wxBOTH);
	  this->SetIcon(wxICON(nsisicon));

   }