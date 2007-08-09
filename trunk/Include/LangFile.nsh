/*

LangFile.nsh

Header file to create langauge file that can be
included with a single command.

When LANGFILE_DEFAULT is set, missing strings will
automatically be loaded from a default file.

Copyright © 2007 Joost Verburg

*/

!macro LANGFILE_INCLUDE FILENAME

  ;Called from script: include a langauge file

  !ifdef LangFileString
    !undef LangFileString
  !endif

  !define LangFileString "!insertmacro LANGFILE_SETSTRING"

  !define LANGFILE_SETNAMES  
  !include "${FILENAME}"
  !undef LANGFILE_SETNAMES

  ;Include default language for missing strings

  !ifdef LANGFILE_DEFAULT
    !include "${LANGFILE_DEFAULT}"
  !endif
  
  ;Create language strings

  !undef LangFileString
  !define LangFileString "!insertmacro LANGFILE_LANGSTRING"

  !ifdef LANGFILE_DEFAULT
    !include "${LANGFILE_DEFAULT}"
  !else
    !include "${FILENAME}"
  !endif

!macroend

!macro LANGFILE IDNAME NAME

  ;Start of langauge file, set names

  !ifdef LANGFILE_SETNAMES

    !ifdef LANGFILE_IDNAME
      !undef LANGFILE_IDNAME
    !endif

    !ifdef "LANGFILE_${IDNAME}_NAME"
      !undef "LANGFILE_${IDNAME}_NAME"
    !endif

    !define LANGFILE_IDNAME "${IDNAME}"
    !define "LANGFILE_${IDNAME}_NAME" "${NAME}"  

  !endif

!macroend

!macro LANGFILE_SETSTRING NAME VALUE

  ;Set define with translated string

  !ifndef ${NAME}
    !define "${NAME}" "${VALUE}"
  !endif

!macroend

!macro LANGFILE_LANGSTRING NAME DUMMY

  ;Create a language string from a define and undefine

  LangString "${NAME}" "${LANG_${LANGFILE_IDNAME}}" "${${NAME}}"
  !undef "${NAME}"

!macroend
