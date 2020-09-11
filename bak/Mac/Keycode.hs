
#ifdef darwin_HOST_OS
  | KeyFn
  | KeyLaunchpad
  | KeyMissionCtrl
  | KeyBacklightDown
  | KeyBacklightUp
  | KeyError
#endif



#ifdef darwin_HOST_OS
  , (KeyLaunchpad,      ["lp"])
  , (KeyMissionCtrl,    ["mctl"])
  , (KeyBacklightDown,  ["bldn"])
  , (KeyBacklightUp,    ["blup"])
#endif
