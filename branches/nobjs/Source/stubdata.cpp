/*
 * stubdata.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2008 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "stubdata.h"

stub_data::stub_data() {
/*
  memset(&build_header,-1,sizeof(build_header));

  build_header.install_reg_rootkey=0;
  build_header.flags=CH_FLAGS_NO_ROOT_DIR;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  build_header.lb_bg=RGB(0,0,0);
  build_header.lb_fg=RGB(0,255,0);
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  build_header.license_bg=-COLOR_BTNFACE;
#endif
  build_header.install_directory_ptr=0;
  build_header.install_directory_auto_append=0;
  build_header.install_reg_key_ptr=0;
  build_header.install_reg_value_ptr=0;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  memset(build_header.install_types,0,sizeof(build_header.install_types));
#endif
  memset(&build_header.blocks,0,sizeof(build_header.blocks));

  memset(&build_uninst,-1,sizeof(build_uninst));

  build_header.install_reg_rootkey=0;
  build_uninst.flags=0;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  build_uninst.lb_bg=RGB(0,0,0);
  build_uninst.lb_fg=RGB(0,255,0);
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  build_uninst.license_bg=-COLOR_BTNFACE;
#endif
  build_uninst.install_directory_ptr=0;
  build_uninst.install_directory_auto_append=0;
  build_uninst.install_reg_key_ptr=0;
  build_uninst.install_reg_value_ptr=0;
#ifdef NSIS_CONFIG_COMPONENTPAGE
  memset(build_uninst.install_types,0,sizeof(build_uninst.install_types));
#endif
  memset(&build_uninst.blocks,0,sizeof(build_uninst.blocks));

  uninstaller_writes_used=0;

  build_strlist.add("",0);
  ubuild_strlist.add("",0);

  build_langstring_num=0;
  ubuild_langstring_num=0;

  build_font[0]=0;
  build_font_size=0;

  m_unicon_size=0;

  branding_image_found=false;

  no_space_texts=false;

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  build_plugin_unload=0;
  plugins_processed=0;
#endif

  last_used_lang=NSIS_DEFAULT_LANG;

  res_editor=0;

  manifest_comctl = manifest::comctl_old;
  manifest_exec_level = manifest::exec_level_none;

  enable_last_page_cancel=0;
  uenable_last_page_cancel=0;

  license_res_id=IDD_LICENSE;

  disable_window_icon=0;

  notify_hwnd=0;

#ifdef NSIS_SUPPORT_BGBG
  bg_default_font.lfHeight=40;
  bg_default_font.lfWidth=0;
  bg_default_font.lfEscapement=0;
  bg_default_font.lfOrientation=0;
  bg_default_font.lfWeight=FW_BOLD;
  bg_default_font.lfItalic=TRUE;
  bg_default_font.lfUnderline=FALSE;
  bg_default_font.lfStrikeOut=FALSE;
  bg_default_font.lfCharSet=DEFAULT_CHARSET;
  bg_default_font.lfOutPrecision=OUT_DEFAULT_PRECIS;
  bg_default_font.lfClipPrecision=CLIP_DEFAULT_PRECIS;
  bg_default_font.lfQuality=DEFAULT_QUALITY;
  bg_default_font.lfPitchAndFamily=DEFAULT_PITCH;
  strncpy(bg_default_font.lfFaceName,"Times New Roman",LF_FACESIZE);
  memcpy(&bg_font,&bg_default_font,sizeof(LOGFONT));
#endif
*/
}

