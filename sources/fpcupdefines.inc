{$ifndef DisableRemoteLog}
{$define RemoteLog}
{$endif}

{$define FORCEREVISION}
{$define FORCELAZBUILD}
{$define MACOSXVERSIONMAGIG}
{.$define DISABLELAZBUILDJOBS}

{.$define crosssimple}

{$IF (DEFINED(WINDOWS)) OR (DEFINED(LINUX))}
{.$DEFINE USEMORMOT}
{$ENDIF}

{$ifdef LCL}
{$define THREADEDEXECUTE}
{$endif}

{$define ENABLEWGET}
{$define ENABLECURL}
{$define ENABLENATIVE}

{.$define ENABLEEMAIL}

{$ifdef Haiku}
// synaser does not compile under Haiku
{$undef ENABLENATIVE}
{$endif}
{$ifdef OpenBSD}
// synaser does not work under OpenBSD
{$undef ENABLENATIVE}
{$endif}
{$IF DEFINED(MORPHOS) OR DEFINED(AROS)}
// libcurl does not work under AROS and Morphos
{$undef ENABLECURL}
// synaser does not work under AROS and Morphos
{$undef ENABLENATIVE}
{$ENDIF}

{$ifdef Darwin}
// Do not use wget and family under Darwin
{$undef ENABLEWGET}
{$endif}
{$ifdef Windows}
// Do not use wget and family under Windows
{.$undef ENABLEWGET}
{$endif}

{$ifdef libcurlstatic}
{$undef ENABLENATIVE}
{$define USEONLYCURL}
{$endif}

{.$define DISABLE_PPC_CONFIG_PATH}

{$ifdef FPC}

{$WARN 4104 OFF}
{$WARN 4105 OFF}
{$WARN 4106 OFF}
{$WARN 4107 OFF}
{$WARN 4108 OFF}

{$WARN 5024 OFF}
{$WARN 5027 OFF}
{$WARN 5037 OFF}
{$WARN 5057 OFF}
{$WARN 5091 OFF}
{$WARN 5092 OFF}

{$WARN 6058 OFF}

{$WARN CONSTRUCTING_ABSTRACT ERROR}

{$endif FPC}


