/////////////////////////////////////////////////////////////////////////////
// Name:        m_links.cpp
// Purpose:     wxHtml module for links & anchors
// Author:      Vaclav Slavik
// RCS-ID:      $Id: m_links.cpp,v 1.1 2003/05/26 17:53:58 joostverburg Exp $
// Copyright:   (c) 1999 Vaclav Slavik
// Licence:     wxWindows Licence
/////////////////////////////////////////////////////////////////////////////

#ifdef __GNUG__
#pragma implementation
#endif

#include "wx/wxprec.h"

#include "wx/defs.h"
#if wxUSE_HTML && wxUSE_STREAMS

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WXPRECOMP
#endif

#include "wx/html/forcelnk.h"
#include "wx/html/m_templ.h"


FORCE_LINK_ME(m_links)


class wxHtmlAnchorCell : public wxHtmlCell
{
    private:
        wxString m_AnchorName;

    public:
        wxHtmlAnchorCell(const wxString& name) : wxHtmlCell() {m_AnchorName = name;}
        virtual const wxHtmlCell* Find(int condition, const void* param) const
        {
            if ((condition == wxHTML_COND_ISANCHOR) && (m_AnchorName == (*((const wxString*)param))))
                return this;
            else
                return wxHtmlCell::Find(condition, param);
        }
};



TAG_HANDLER_BEGIN(A, "A")

    TAG_HANDLER_PROC(tag)
    {
        if (tag.HasParam( wxT("NAME") ))
        {
            m_WParser->GetContainer()->InsertCell(new wxHtmlAnchorCell(tag.GetParam( wxT("NAME") )));
        }

        if (tag.HasParam( wxT("HREF") ))
        {
            wxHtmlLinkInfo oldlnk = m_WParser->GetLink();
            wxColour oldclr = m_WParser->GetActualColor();
            int oldund = m_WParser->GetFontUnderlined();
            wxString name(tag.GetParam( wxT("HREF") )), target;

			wxColor LinkColor;
			LinkColor.Set(0x6A, 0x80, 0x98);

			char apppath[MAX_PATH],*temp;
			GetModuleFileName(NULL,apppath,sizeof(apppath));
			temp=strrchr(apppath,'\\');
			if(temp!=NULL) *temp=0;

			char fullpath[MAX_PATH];

			if (name.Left(3).Cmp("EX:") == 0)
			{
				// External
				
				if ((name.Left(10).Cmp("EX:http://") == 0) || (name.Left(9).Cmp("EX:irc://") == 0))
				{

				}
				else
				{
				
					_snprintf(fullpath,sizeof(fullpath),"%s\\%s",apppath,name.Mid(3));

					if (wxFileExists(fullpath) || wxDirExists(fullpath))
					{

					}
					else
					{
						// Feature not installed
						LinkColor.Set(0x80, 0x80, 0x80);
					}
				
				}
			
			}
			else
			{
			
			}
			
            if (tag.HasParam( wxT("TARGET") )) target = tag.GetParam( wxT("TARGET") );
            m_WParser->SetActualColor(LinkColor);
            m_WParser->GetContainer()->InsertCell(new wxHtmlColourCell(LinkColor));
            m_WParser->GetContainer()->InsertCell(new wxHtmlFontCell(m_WParser->CreateCurrentFont()));
            m_WParser->SetLink(wxHtmlLinkInfo(name, target));

            ParseInner(tag);

            m_WParser->SetLink(oldlnk);
            m_WParser->SetFontUnderlined(oldund);
            m_WParser->GetContainer()->InsertCell(new wxHtmlFontCell(m_WParser->CreateCurrentFont()));
            m_WParser->SetActualColor(oldclr);
            m_WParser->GetContainer()->InsertCell(new wxHtmlColourCell(oldclr));

            return TRUE;
        }
        else return FALSE;
    }

TAG_HANDLER_END(A)



TAGS_MODULE_BEGIN(Links)

    TAGS_MODULE_ADD(A)

TAGS_MODULE_END(Links)


#endif
