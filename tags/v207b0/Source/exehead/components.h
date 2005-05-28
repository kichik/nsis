#ifndef ___COMPONENTS_H___
#define ___COMPONENTS_H___

void NSISCALL SectionFlagsChanged(unsigned int index);
void NSISCALL RefreshSectionGroups();
#ifdef NSIS_CONFIG_COMPONENTPAGE
void NSISCALL SetInstType(int inst_type);
unsigned int NSISCALL GetInstType(HTREEITEM *items);
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//!___COMPONENTS_H___
