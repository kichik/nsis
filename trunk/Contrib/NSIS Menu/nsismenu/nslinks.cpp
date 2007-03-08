/////////////////////////////////////////////////////////////////////////////
// Name:        src/html/m_links.cpp
// Purpose:     wxHtml module for links & anchors
// Author:      Vaclav Slavik
// RCS-ID:      $Id: nslinks.cpp,v 1.1 2007/02/24 18:57:09 kichik Exp $
// Copyright:   (c) 1999 Vaclav Slavik
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#if wxUSE_HTML && wxUSE_STREAMS

#ifndef WXPRECOMP
#endif

#include "wx/html/forcelnk.h"
#include "wx/html/m_templ.h"

#include <wx/filefn.h>
#include <wx/stdpaths.h>

FORCE_LINK_ME(nslinks)

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
            wxString name(tag.GetParam( wxT("HREF") )), target;

            if (tag.HasParam( wxT("TARGET") )) target = tag.GetParam( wxT("TARGET") );

            wxColour colour = m_WParser->GetLinkColor();
            wxHtmlLinkInfo linkInfo(name, target);

            if (name.Left(3).IsSameAs((const wxChar*)"EX:", false))
            {
                wxString url = name.Mid(3);
                if (!url.Left(7).IsSameAs((const wxChar*)"http://", false) && !url.Left(6).IsSameAs((const wxChar*)"irc://", false))
                {
                    wxString exePath = wxStandardPaths::Get().GetExecutablePath();
                    wxString path = ::wxPathOnly(exePath);
                    path.Append(wxFileName::GetPathSeparators()[0]);
                    path.Append(url);

                    if (!::wxFileExists(path) && !::wxDirExists(path))
                    {
                        colour = wxColour(0x80, 0x80, 0x80);
                        linkInfo = wxHtmlLinkInfo("notinstalled.html", target);
                    }
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



TAGS_MODULE_BEGIN(CustomLinks)

    TAGS_MODULE_ADD(A)

TAGS_MODULE_END(CustomLinks)


#endif
