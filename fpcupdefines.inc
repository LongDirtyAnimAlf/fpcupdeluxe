{$define FORCEREVISION}
{.$define FORCELAZBUILD}
{.$define DISABLELAZBUILDJOBS}

{.$define crosssimple}

{$ifdef WINDOWS}
{.$define buildnative}
{$endif WINDOWS}

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

