#ifndef __NS_DIALOGS__DEFS_H__
#define __NS_DIALOGS__DEFS_H__

#include <windows.h>

#define NSDFUNC __stdcall

typedef int nsFunction;

enum nsControlType
{
  NSCTL_UNKNOWN,
  NSCTL_BUTTON,
  NSCTL_EDIT,
  NSCTL_COMBOBOX,
  NSCTL_LISTBOX,
  NSCTL_RICHEDIT,
  NSCTL_RICHEDIT2,
  NSCTL_STATIC
};

struct nsDialogCallbacks
{
  nsFunction onBack;
};

#define DLG_CALLBACK_IDX(x) (FIELD_OFFSET(struct nsDialogCallbacks, x)/sizeof(nsFunction))

struct nsControlCallbacks
{
  nsFunction onClick;
  nsFunction onChange;
  nsFunction onNotify;
};

#define CTL_CALLBACK_IDX(x) (FIELD_OFFSET(struct nsControlCallbacks, x)/sizeof(nsFunction))

#define USERDATA_SIZE 1024

struct nsControl
{
  HWND               window;
  enum nsControlType type;
  char userData[USERDATA_SIZE];
  struct nsControlCallbacks callbacks;
};

struct nsDialog
{
  HWND hwDialog;
  HWND hwParent;

  WNDPROC parentOriginalWndproc;

  struct nsDialogCallbacks callbacks;

  unsigned controlCount;

  struct nsControl* controls;
};

#define NSCONTROL_ID_PROP "NSIS: nsControl pointer property"

#endif//__NS_DIALOGS__DEFS_H__
