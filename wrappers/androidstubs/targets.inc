  {$if defined(amd64_darwin) OR defined(arm64_darwin)}
  MIN_SDK_VERSION='10.8';
  SDK_VERSION='11.0';
  SDK_VERSION_12='12.3';
  BASETRIPLE='-apple-darwin19';
  BASEOS=OSX_NAME;
  {$endif}
  {$if defined(i386_darwin)}
  MIN_SDK_VERSION='10.4';
  SDK_VERSION='10.13';
  BASETRIPLE='-apple-darwin19';
  BASEOS=OSX_NAME;
  {$endif}
  {$ifdef arm64_ios}
  MIN_SDK_VERSION='9.0';
  SDK_VERSION='13.7';
  BASETRIPLE='-apple-ios14';
  BASEOS=IOS_NAME;
  {$endif}
  {$ifdef arm_ios}
  MIN_SDK_VERSION='6.0';
  SDK_VERSION='10.0';
  BASETRIPLE='-apple-ios10';
  BASEOS=IOS_NAME;
  {$endif}

  {$ifdef amd64_darwin}
  TARGETCPU='x86_64';
  {$endif}
  {$if defined(arm64_darwin) OR defined(arm64_ios)}
  TARGETCPU='aarch64';
  {$endif}
  {$ifdef i386_darwin}
  TARGETCPU='i386';
  {$endif}
  {$ifdef arm_ios}
  TARGETCPU='arm';
  {$endif}

  {$ifdef CLANG}
  EXEWRAPPER=CLANG_NAME;
  {$endif}
  {$ifdef CLANGPP}
  EXEWRAPPER=CLANGPP_NAME;
  {$endif}
  {$ifdef DSYMUTIL}
  EXEWRAPPER=DSYMUTIL_NAME;
  {$endif}
  {$ifdef LD}
  EXEWRAPPER=LD_NAME;
  {$endif}
  {$ifdef STRIP}
  EXEWRAPPER=STRIP_NAME;
  {$endif}

  {$if DECLARED(TARGETCPU) AND DECLARED(BASETRIPLE)}
  TARGETTRIPLE=TARGETCPU+BASETRIPLE;
  {$endif}
  {$if DECLARED(BASEOS) AND DECLARED(SDK_VERSION)}
  SDKDIR=BASEOS+SDK_VERSION+'.sdk';
  {$endif}
  {$if DECLARED(BASEOS) AND DECLARED(SDK_VERSION_12)}
  SDKDIR_12=BASEOS+SDK_VERSION_12+'.sdk';
  {$endif}


