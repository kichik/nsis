/*
 * components.c
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "../Platform.h"
#include "config.h"
#include "ui.h"
#include "fileform.h"

void NSISCALL SectionFlagsChanged(unsigned int index) {
  section *sections = g_sections;

  int flags = sections[index].flags;

  if (flags & SF_SECGRP) {
    unsigned int i = index + 1;
    unsigned int level = 0;

    for (; i < (unsigned int) num_sections; i++) {
      if (sections[i].flags & SF_SECGRP) {
        level++;
        continue;
      }

      if (sections[i].flags & SF_SECGRPEND) {
        if (level-- == 0) {
          break;
        }

        continue;
      }

      if ((sections[i].flags & SF_RO) == 0) {
        sections[i].flags &= ~SF_SELECTED;
        sections[i].flags |= (flags & SF_SELECTED);
      }
    }
  }
}

unsigned int NSISCALL _RefreshSectionGroups(unsigned int i, int not_first_call) {
  unsigned int selected = 0;
  unsigned int not_selected = 0;

  section *sections = g_sections;

  section *sec = &sections[i];

  if (sec->flags & SF_SECGRP) {
    if (not_first_call) {
      sec->flags &= ~(SF_SELECTED | SF_PSELECTED);
      i++;
    }
  }

  while (i < (unsigned int) num_sections) {
    int flags = sections[i].flags;
    int ni = i + 1;

    if (flags & SF_SECGRP) {
      ni = _RefreshSectionGroups(i, 1);
      flags = sections[i].flags;
    }

    if (flags & SF_SECGRPEND) {
      if (selected) {
        if (not_selected) {
          sec->flags |= SF_PSELECTED;
        } else {
          sec->flags |= SF_SELECTED;
          sec->flags &= ~SF_TOGGLED;
        }
      }

      return ni;
    }

    if (flags & SF_PSELECTED) {
      selected++;
    }

    if (flags & SF_SELECTED) {
      selected++;
    } else {
      not_selected++;
    }

    i = ni;
  }

  return 0;
}

#ifdef NSIS_CONFIG_COMPONENTPAGE

void NSISCALL SetInstType(int inst_type) {
  unsigned int i = 0;

  section *sections = g_sections;

  if ((unsigned int) inst_type >= NSIS_MAX_INST_TYPES) {
    return;
  }

  for (; i < (unsigned int) num_sections; i++) {
    if (sections[i].flags & (SF_SECGRP | SF_SECGRPEND)) {
      continue;
    }

    if (sections[i].install_types & (1 << inst_type)) {
      sections[i].flags |= SF_SELECTED;
    } else {
      sections[i].flags &= ~SF_SELECTED;
    }
  }
}

unsigned int NSISCALL GetInstType(HTREEITEM *items) {
  unsigned int i, j;

  section *sections = g_sections;

  for (i = 0; i < NSIS_MAX_INST_TYPES; i++) {
    if (!g_header->install_types[i]) {
      continue;
    }

    for (j = 0; j < (unsigned int) num_sections; j++) {
      if (sections[j].flags & (SF_SECGRP | SF_SECGRPEND)) {
        continue;
      }

      if (items && !items[j]) {
        continue;
      }

      if ((sections[j].install_types & (1 << i)) == ((sections[j].flags & SF_SELECTED) << i)) {
        continue;
      } else {
        break;
      }
    }

    if (j == (unsigned int) num_sections) {
      break;
    }
  }

  return i;
}

#endif//NSIS_CONFIG_COMPONENTPAGE
