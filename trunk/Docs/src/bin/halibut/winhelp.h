/*
 * winhelp.h   header file for winhelp.c
 */

typedef struct WHLP_tag *WHLP;

typedef struct WHLP_TOPIC_tag *WHLP_TOPIC;

/*
 * Initialise a new WHlp context and begin accumulating data in it.
 */
WHLP whlp_new (void);

/*
 * Close a WHlp context and write out the help file it has created.
 */
void whlp_close (WHLP h, char *filename);

/*
 * Abandon and free a WHlp context without writing out anything.
 */
void whlp_abandon (WHLP h);

/*
 * Specify the title and copyright notice of a help file. Also
 * specify Help macros to be run on loading.
 */
void whlp_title (WHLP h, char *title);
void whlp_copyright (WHLP h, char *copyright);
void whlp_start_macro (WHLP h, char *macro);

/*
 * Register a help topic. Irritatingly, due to weird phase-order
 * issues with the whole file format, you have to register all your
 * topics _before_ actually outputting your text. This seems likely
 * to require two passes over the source document.
 * 
 * If you want to specify a particular context string (for
 * reference from other programs, to provide context-sensitive
 * help), you can supply it here. Otherwise, just pass NULL and a
 * nondescript one will be allocated automatically.
 *
 * If you specify two context strings which clash under the Windows
 * help file hash algorithm, this function will return NULL and
 * provide a pointer to the other context string that this one
 * clashed with, and you must tell your user to fix the clash.
 * Sadly this is the only way to do it; despite HLP files having a
 * perfectly good method of mapping arbitrary strings to things,
 * they didn't see fit to use that method for help contexts, so
 * instead they hash the context names and expect the hashes to be
 * unique. Sigh.
 * 
 * On success (i.e. in any circumstance other than a hash clash), a
 * valid WHLP_TOPIC is returned for later use.
 */
WHLP_TOPIC whlp_register_topic (WHLP h, char *context_name, char **clash);

/*
 * Link two topics together in a browse sequence. Automatically
 * takes care of the forward and reverse links.
 */
void whlp_browse_link (WHLP h, WHLP_TOPIC before, WHLP_TOPIC after);

/*
 * After calling whlp_register_topic for all topics, you should
 * call this, which will sort out all loose ends and allocate
 * context names for all anonymous topics. Then you can start
 * writing actual text.
 */
void whlp_prepare (WHLP h);

/*
 * Create a link from an index term to a topic.
 */
void whlp_index_term (WHLP h, char *index, WHLP_TOPIC topic);

/*
 * Call this if you need the id of a topic and you don't already
 * know it (for example, if whlp_prepare has allocated it
 * anonymously for you). You might need this, for example, in
 * creating macros for button-bar bindings.
 * 
 * The string returned will be freed when the WHLP context is
 * closed. You should not free it yourself.
 * 
 * Do not call this before calling whlp_prepare().
 */
char *whlp_topic_id (WHLP_TOPIC topic);

/*
 * Call this to specify which help topic will be the first one
 * displayed when the help file is loaded.
 */
void whlp_primary_topic (WHLP h, WHLP_TOPIC topic);

/*
 * Call this when about to begin writing out the text for a topic.
 * 
 * Any additional arguments are Help macros, terminated with a
 * NULL. So the minimum call sequence is
 * 
 *   whlp_begin_topic(helpfile, mytopic, "Title", NULL);
 */
void whlp_begin_topic (WHLP h, WHLP_TOPIC topic, char *title, ...);

/*
 * Call this to set up a font descriptor. You supply the font name,
 * the font size (in half-points), the graphic rendition flags
 * (bold, italic etc), and the general font family (for Windows to
 * select a fallback font if yours is unavailable). You can also
 * specify a foreground colour for the text (but unfortunately not
 * a background).
 * 
 * Font descriptors are identified in whlp_set_font() by small
 * integers, which are allocated from 0 upwards in the order you
 * call whlp_create_font(). For your convenience,
 * whlp_create_font() returns the integer allocated to each font
 * descriptor you create, but you could work this out just as
 * easily yourself by counting.
 */
enum
{
  WHLP_FONT_BOLD = 1,
  WHLP_FONT_ITALIC = 2,
  WHLP_FONT_UNDERLINE = 4,
  WHLP_FONT_STRIKEOUT = 8,
  WHLP_FONT_DOUBLEUND = 16,
  WHLP_FONT_SMALLCAPS = 32
};
enum
{
  WHLP_FONTFAM_FIXED = 1,
  WHLP_FONTFAM_SERIF = 2,
  WHLP_FONTFAM_SANS = 3,
  WHLP_FONTFAM_SCRIPT = 4,
  WHLP_FONTFAM_DECOR = 5
};
int whlp_create_font (WHLP h, char *font, int family, int halfpoints,
                      int rendition, int r, int g, int b);

/*
 * Routines to output paragraphs and actual text (at last).
 * 
 * You should start by calling whlp_para_attr() to set any
 * paragraph attributes that differ from the standard settings.
 * Next call whlp_begin_para() to start the paragraph. Then call
 * the various in-paragraph functions until you have output the
 * whole paragraph, and finally call whlp_end_para() to finish it
 * off.
 */
enum
{
  WHLP_PARA_SPACEABOVE = 1, WHLP_PARA_SPACEBELOW, WHLP_PARA_SPACELINES,
  WHLP_PARA_LEFTINDENT, WHLP_PARA_RIGHTINDENT, WHLP_PARA_FIRSTLINEINDENT,
  WHLP_PARA_ALIGNMENT
};
enum
{
  WHLP_ALIGN_LEFT, WHLP_ALIGN_RIGHT, WHLP_ALIGN_CENTRE
};
enum
{
  WHLP_PARA_SCROLL, WHLP_PARA_NONSCROLL
};
void whlp_para_attr (WHLP h, int attr_id, int attr_param);
void whlp_set_tabstop (WHLP h, int tabstop, int alignment);
void whlp_begin_para (WHLP h, int para_type);
void whlp_end_para (WHLP h);
void whlp_set_font (WHLP h, int font_id);
void whlp_text (WHLP h, char *text);
void whlp_start_hyperlink (WHLP h, WHLP_TOPIC target);
void whlp_end_hyperlink (WHLP h);
void whlp_tab (WHLP h);
