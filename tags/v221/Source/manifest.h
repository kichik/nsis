#ifndef ___MANIFEST_H___
#define ___MANIFEST_H___

#include <string>

namespace manifest
{
  enum comctl
  {
    comctl_old,
    comctl_xp
  };

  enum exec_level
  {
    exec_level_none,
    exec_level_user,
    exec_level_admin
  };

  std::string generate(comctl, exec_level);
};

#endif//!___MANIFEST_H___
