/////////////////////////////////////////////////////////////////////////////
// Name:        src/html/m_links.cpp
// Purpose:     wxHtml module for links & anchors
// Author:      Vaclav Slavik
// RCS-ID:      $Id: nslinks.cpp,v 1.2 2007/03/08 01:47:14 pabs3 Exp $
// Copyright:   (c) 1999 Vaclav Slavik
// Licence:     wxWindows licence
//
// Reviewed for Unicode support by Jim Park -- 08/23/2007
// Basically, compiling wxWidgets as Unicode should do it.
/////////////////////////////////////////////////////////////////////////////

#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#if wxUSE_HTML && wxUSE_STREAMS && !defined(NSISMENU_NOLINKTAGHANDLER)

#ifndef WXPRECOMP
#endif

#include "wx/html/forcelnk.h"
#include "wx/html/m_templ.h"

#include <wx/filefn.h>
#include <wx/stdpaths.h>

#include <nsis-sconf.h>

FORCE_LINK_ME(nslinks)

// TODO: These helper function declarations should be in a header file
typedef enum { SUT_UNKNOWN = 0, SUT_BIN = 0x14, SUT_DOC = 0x24, SUT_WEB = 0x34 } SPECIALURLTYPE;
SPECIALURLTYPE TransformUrl(wxString&Url);

class wxHtmlAnchorCell : public wxHtmlCell
{
private:
    wxString m_AnchorName;

public:
    wxHtmlAnchorCell(const wxString& name) : wxHtmlCell()
        { m_AnchorName = name; }
    void Draw(wxDC& WXUNUSED(dc),
              int WXUNUSED(x), int WXUNUSED(y),
              int WXUNUSED(view_y1), int WXUNUSED(view_y2),
              wxHtmlRenderingInfo& WXUNUSED(info)) {}

    DECLARE_NO_COPY_CLASS(wxHtmlAnchorCell)
};



TAG_HANDLER_BEGIN(A, "A")
    TAG_HANDLER_CONSTR(A) { }

    TAG_HANDLER_PROC(tag)
    {
        if (tag.HasParam( wxT("HREF") ))
        {
            wxHtmlLinkInfo oldlnk = m_WParser->GetLink();
            wxColour oldclr = m_WParser->GetActualColor();
            wxString href(tag.GetParam( wxT("HREF") )), target;

            if (tag.HasParam( wxT("TARGET") )) target = tag.GetParam( wxT("TARGET") );

            wxColour colour = m_WParser->GetLinkColor();
            wxHtmlLinkInfo linkInfo(href, target);
            
            wxString location = href;
            SPECIALURLTYPE ut = TransformUrl(location);
            if (ut == SUT_BIN || ut == SUT_DOC)
            {
                if (!wxFileExists(location) && !wxDirExists(location))
                {
                    colour = wxColour(0x80, 0x80, 0x80);
                    linkInfo = wxHtmlLinkInfo(wxT("notinstalled.html"), target);
                }
            }

            m_WParser->SetActualColor(colour);
            m_WParser->GetContainer()->InsertCell(new wxHtmlColourCell(colour));
            m_WParser->GetContainer()->InsertCell(new wxHtmlFontCell(m_WParser->CreateCurrentFont()));
            m_WParser->SetLink(linkInfo);

            ParseInner(tag);

            m_WParser->SetLink(oldlnk);
            m_WParser->GetContainer()->InsertCell(new wxHtmlFontCell(m_WParser->CreateCurrentFont()));
            m_WParser->SetActualColor(oldclr);
            m_WParser->GetContainer()->InsertCell(new wxHtmlColourCell(oldclr));

            return true;
        }
        else return false;
    }

TAG_HANDLER_END(A)


#if wxCHECK_VERSION(3, 0, 0)
TAGS_MODULE_BEGIN(Links)
#else
TAGS_MODULE_BEGIN(CustomLinks)
#endif

    TAGS_MODULE_ADD(A)

#if wxCHECK_VERSION(3, 0, 0)
TAGS_MODULE_END(Links)
#else
TAGS_MODULE_END(CustomLinks)
#endif


#endif
