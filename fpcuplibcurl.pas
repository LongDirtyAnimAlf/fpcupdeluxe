{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Pascal packages
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    This version:
    Specially for fpcupdeluxe.
    Enable static/dynamic lib-loading to prevent errors on systems without libcurl.

 **********************************************************************}
 {
   the curl library is governed by its own copyright, see the curl
   website for this. 
 }
{$mode objfpc}
unit fpcuplibcurl;

{$ifdef win32}
{.$define libcurlstatic}
{$endif}

{$ifdef libcurlstatic}
{$ifdef win32}
  {$linklib .\libs\win32\libcurl.a}
  {$linklib .\libs\win32\libadvapi32.a}
  {$linklib .\libs\win32\libws2_32.a}
  //{$linklib .\libs\win32\libmingwex.a} // for _stroll and ___mingw_basename
  {$linklib .\libs\win32\libmsvcrt.a}
  //{$linklib .\libs\win32\libmsvcr100.a} // for __assert
  {$linklib .\libs\win32\libkernel32.a}
  {$linklib .\libs\win32\libcrypt32.a}
  //{$linklib .\libs\win32\libgcc.a} for ___divdi3, ___umoddi3, ___udivdi3
{$endif}
{$endif}

interface

{$IFDEF WINDOWS}
uses
  SysUtils,
  ctypes;

type
  time_t = clong;
  PTime_t = ^time_t;
  off_t  = clong;
{$ELSE}
uses
  unixtype;
{$ENDIF}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{ Automatically converted by H2Pas 1.0.0 from curl.h
  The following command line parameters were used:
    -D -l libcurl -p curl.h }

const
  External_library='libcurl'; {Setup as you need}

Type
  Pcurl_calloc_callback  = ^curl_calloc_callback;
  Pcurl_closepolicy  = ^curl_closepolicy;
  Pcurl_forms  = ^curl_forms;
  Pcurl_ftpauth  = ^curl_ftpauth;
  Pcurl_ftpmethod  = ^curl_ftpmethod;
  Pcurl_ftpssl  = ^curl_ftpssl;
  PCURL_HTTP_VERSION  = ^CURL_HTTP_VERSION;
  Pcurl_httppost  = ^curl_httppost;
  PPcurl_httppost = ^Pcurl_httppost;
  Pcurl_infotype  = ^curl_infotype;
  Pcurl_lock_access  = ^curl_lock_access;
  Pcurl_lock_data  = ^curl_lock_data;
  Pcurl_malloc_callback  = ^curl_malloc_callback;
  PCURL_NETRC_OPTION  = ^CURL_NETRC_OPTION;
  Pcurl_off_t  = ^curl_off_t;
  Pcurl_proxytype  = ^curl_proxytype;
  Pcurl_realloc_callback  = ^curl_realloc_callback;
  Pcurl_slist  = ^curl_slist;
  Pcurl_socket_t  = ^curl_socket_t;
  PCURL_SSL_VERSION  = ^CURL_SSL_VERSION;
  Pcurl_strdup_callback  = ^curl_strdup_callback;
  PCURL_TIMECOND  = ^CURL_TIMECOND;
  Pcurl_version_info_data  = ^curl_version_info_data;
  PCURLcode  = ^CURLcode;
  PCURLFORMcode  = ^CURLFORMcode;
  PCURLformoption  = ^CURLformoption;
  PCURLINFO  = ^CURLINFO;
  Pcurliocmd  = ^curliocmd;
  Pcurlioerr  = ^curlioerr;
  PCURLM  = ^CURLM;
  PCURLMcode  = ^CURLMcode;
  PCURLMoption  = ^CURLMoption;
  PCURLMSG  = ^CURLMSG;
  PCURLoption  = ^CURLoption;
  PCURLSH  = ^CURLSH;
  PCURLSHcode  = ^CURLSHcode;
  PCURLSHoption  = ^CURLSHoption;
  PCURLversion  = ^CURLversion;
  Pfd_set = pointer;

  PCURL = ^CURL;
  CURL = pointer;
  curl_off_t = off_t;
  curl_socket_t = longint;

  curl_httppost = record
    next : Pcurl_httppost;
    name : Pchar;
    namelength : longint;
    contents : Pchar;
    contentslength : longint;
    buffer : Pchar;
    bufferlength : longint;
    contenttype : Pchar;
    contentheader : Pcurl_slist;
    more : Pcurl_httppost;
    flags : longint;
    showfilename : Pchar;
    userp : pointer;
  end;

  curlfiletype = (
      CURLFILETYPE_FILE,
      CURLFILETYPE_DIRECTORY,
      CURLFILETYPE_SYMLINK,
      CURLFILETYPE_DEVICE_BLOCK,
      CURLFILETYPE_DEVICE_CHAR,
      CURLFILETYPE_NAMEDPIPE,
      CURLFILETYPE_SOCKET,
      CURLFILETYPE_DOOR,
      CURLFILETYPE_UNKNOWN);
  curl_fileinfo = record
    filename : ^char;
    filetype : curlfiletype;
    time : time_t;
    perm : dword;
    uid : longint;
    gid : longint;
    size : curl_off_t;
    hardlinks : longint;
    strings : record
        time : ^char;
        perm : ^char;
        user : ^char;
        group : ^char;
        target : ^char;
      end;
    flags : dword;
    b_data : ^char;
    b_size : size_t;
    b_used : size_t;
  end;

  curl_progress_callback = function (clientp:pointer; dltotal:double; dlnow:double; ultotal:double; ulnow:double):longint;cdecl;
  curl_write_callback = function (buffer:Pchar; size:size_t; nitems:size_t; outstream:pointer):size_t;cdecl;
  curl_read_callback = function (buffer:Pchar; size:size_t; nitems:size_t; instream:pointer):size_t;cdecl;
  curl_passwd_callback = function (clientp:pointer; prompt:Pchar; buffer:Pchar; buflen:longint):longint;cdecl;
  curl_chunk_bgn_callback = function (transfer_info:pointer; ptr:pointer; remains:longint):longint;cdecl;
  curl_chunk_end_callback = function (ptr:pointer):longint;cdecl;
  curl_fnmatch_callback = function (ptr:pointer; pattern:Pchar; _string:Pchar):longint;cdecl;
  curl_seek_callback = function (instream:pointer; offset:curl_off_t; origin:longint):longint;cdecl;
  curlsocktype = (
    CURLSOCKTYPE_IPCXN,
    CURLSOCKTYPE_LAST
  );

  curl_sockopt_callback = function (clientp:pointer; curlfd:curl_socket_t; purpose:curlsocktype):longint;cdecl;
  curl_sockaddr = record
    family : longint;
    socktype : longint;
    protocol : longint;
    addrlen : dword;
    addr : array[0..0] of byte; // variable length, needs typecast
  end;
  pcurl_sockaddr = ^curl_sockaddr;

  curl_opensocket_callback = function (clientp:pointer; purpose:curlsocktype; address:Pcurl_sockaddr):curl_socket_t;cdecl;
  curl_closesocket_callback = function (clientp:pointer; item:curl_socket_t):longint;cdecl;

  curlioerr = (CURLIOE_OK, CURLIOE_UNKNOWNCMD, CURLIOE_FAILRESTART, CURLIOE_LAST);
  curliocmd = (CURLIOCMD_NOP, CURLIOCMD_RESTARTREAD, CURLIOCMD_LAST);

  curl_ioctl_callback = function (handle:PCURL; cmd:longint; clientp:pointer):curlioerr;cdecl;
  curl_malloc_callback = function(size: size_t) : pointer; cdecl;
  curl_free_callback = procedure (ptr:pointer); cdecl;
  curl_realloc_callback = function(ptr : pointer; size:size_t) : pointer; cdecl;
  curl_strdup_callback = function(str : pchar) : pchar; cdecl;
  curl_calloc_callback = function(nmemb : size_t; size : size_t) : pointer;

  curl_infotype = (CURLINFO_TEXT := 0,CURLINFO_HEADER_IN,
                   CURLINFO_HEADER_OUT,CURLINFO_DATA_IN,
                   CURLINFO_DATA_OUT,CURLINFO_SSL_DATA_IN,
                   CURLINFO_SSL_DATA_OUT,CURLINFO_END);

  curl_debug_callback = function (handle:PCURL; _type:curl_infotype; data:Pchar; size:size_t; userptr:pointer):longint;cdecl;

  CURLcode = (CURLE_OK,CURLE_UNSUPPORTED_PROTOCOL,
    CURLE_FAILED_INIT,CURLE_URL_MALFORMAT,
    CURLE_URL_MALFORMAT_USER,CURLE_COULDNT_RESOLVE_PROXY,
    CURLE_COULDNT_RESOLVE_HOST,CURLE_COULDNT_CONNECT,
    CURLE_FTP_WEIRD_SERVER_REPLY,CURLE_REMOTE_ACCESS_DENIED,
    CURLE_OBSOLETE10,CURLE_FTP_WEIRD_PASS_REPLY,
    CURLE_OBSOLETE12,CURLE_FTP_WEIRD_PASV_REPLY,
    CURLE_FTP_WEIRD_227_FORMAT,CURLE_FTP_CANT_GET_HOST,
    CURLE_OBSOLETE16, CURLE_FTP_COULDNT_SET_TYPE,
    CURLE_PARTIAL_FILE,CURLE_FTP_COULDNT_RETR_FILE,
    CURLE_OBSOLETE20,CURLE_QUOTE_ERROR,
    CURLE_HTTP_RETURNED_ERROR,CURLE_WRITE_ERROR,
    CURLE_OBSOLETE24,CURLE_UPLOAD_FAILED ,
    CURLE_READ_ERROR,CURLE_OUT_OF_MEMORY,
    CURLE_OPERATION_TIMEOUTED,CURLE_FTP_COULDNT_SET_ASCII,
    CURLE_FTP_PORT_FAILED,CURLE_FTP_COULDNT_USE_REST,
    CURLE_OBSOLETE32,CURLE_RANGE_ERROR,
    CURLE_HTTP_POST_ERROR,CURLE_SSL_CONNECT_ERROR,
    CURLE_BAD_DOWNLOAD_RESUME,CURLE_FILE_COULDNT_READ_FILE,
    CURLE_LDAP_CANNOT_BIND,CURLE_LDAP_SEARCH_FAILED,
    CURLE_OBSOLETE40,CURLE_FUNCTION_NOT_FOUND,
    CURLE_ABORTED_BY_CALLBACK,CURLE_BAD_FUNCTION_ARGUMENT,
    CURLE_OBSOLETE44,CURLE_INTERFACE_FAILED,
    CURLE_OBSOLETE46,CURLE_TOO_MANY_REDIRECTS,
    CURLE_UNKNOWN_TELNET_OPTION,CURLE_TELNET_OPTION_SYNTAX,
    CURLE_OBSOLETE50,CURLE_PEER_FAILED_VERIFICATION,
    CURLE_GOT_NOTHING,CURLE_SSL_ENGINE_NOTFOUND,
    CURLE_SSL_ENGINE_SETFAILED,CURLE_SEND_ERROR,
    CURLE_RECV_ERROR,CURLE_OBSOLETE57,
    CURLE_SSL_CERTPROBLEM,CURLE_SSL_CIPHER,
    CURLE_SSL_CACERT,CURLE_BAD_CONTENT_ENCODING,
    CURLE_LDAP_INVALID_URL,CURLE_FILESIZE_EXCEEDED,
    CURLE_USE_SSL_FAILED,CURLE_SEND_FAIL_REWIND,
    CURLE_SSL_ENGINE_INITFAILED,CURLE_LOGIN_DENIED,
    CURLE_TFTP_NOTFOUND,CURLE_TFTP_PERM,
    CURLE_REMOTE_DISK_FULL,CURLE_TFTP_ILLEGAL,
    CURLE_TFTP_UNKNOWNID,CURLE_REMOTE_FILE_EXISTS,
    CURLE_TFTP_NOSUCHUSER,CURLE_CONV_FAILED,
    CURLE_CONV_REQD,CURLE_SSL_CACERT_BADFILE,
    CURLE_REMOTE_FILE_NOT_FOUND,CURLE_SSH,
    CURLE_SSL_SHUTDOWN_FAILED,CURLE_AGAIN,
    CURLE_SSL_CRL_BADFILE,CURLE_SSL_ISSUER_ERROR,
    CURLE_FTP_PRET_FAILED,CURLE_RTSP_CSEQ_ERROR,
    CURLE_RTSP_SESSION_ERROR,CURLE_FTP_BAD_FILE_LIST,
    CURLE_CHUNK_FAILEDCURL_LAST);
 
  curl_conv_callback = function (buffer:Pchar; length:size_t):CURLcode;cdecl;
  curl_ssl_ctx_callback = function (curl:PCURL; ssl_ctx:pointer; userptr:pointer):CURLcode;cdecl;

  curl_proxytype = (
    CURLPROXY_HTTP := 0,
    CURLPROXY_SOCKS4 := 4,
    CURLPROXY_SOCKS5 := 5,
    CURLPROXY_SOCKS4A := 6,
    CURLPROXY_SOCKS5_HOSTNAME := 7);

  curl_khkey = record
    key : ^char;
    len : size_t;
    keytype : (CURLKHTYPE_UNKNOWN,CURLKHTYPE_RSA1,
    CURLKHTYPE_RSA,CURLKHTYPE_DSS);
   end;
  pcurl_khkey = ^curl_khkey;

  curl_khstat = (CURLKHSTAT_FINE_ADD_TO_FILE,
    CURLKHSTAT_FINE,
    CURLKHSTAT_REJECT,
    CURLKHSTAT_DEFER,
    CURLKHSTAT_LAST
  );

  curl_khmatch = (
    CURLKHMATCH_OK,
    CURLKHMATCH_MISMATCH,
    CURLKHMATCH_MISSING,
    CURLKHMATCH_LAST);
  curl_sshkeycallback = function (easy:PCURL; knownkey:Pcurl_khkey; foundkey:Pcurl_khkey; _para4:curl_khmatch; clientp:pointer):longint;cdecl;

  curl_usessl = (
      CURLUSESSL_NONE,
      CURLUSESSL_TRY,
      CURLUSESSL_CONTROL,
      CURLUSESSL_ALL,
      CURLUSESSL_LAST);

  curl_ftpccc = (CURLFTPSSL_CCC_NONE,CURLFTPSSL_CCC_PASSIVE,
    CURLFTPSSL_CCC_ACTIVE,CURLFTPSSL_CCC_LAST
   );

  curl_ftpauth = (
    CURLFTPAUTH_DEFAULT,
    CURLFTPAUTH_SSL,
    CURLFTPAUTH_TLS,
    CURLFTPAUTH_LAST);

  curl_ftpssl = curl_usessl;

  curl_ftpcreatedir = (
      CURLFTP_CREATE_DIR_NONE,CURLFTP_CREATE_DIR,
      CURLFTP_CREATE_DIR_RETRY,CURLFTP_CREATE_DIR_LAST
      );


  curl_ftpmethod = (CURLFTPMETHOD_DEFAULT,CURLFTPMETHOD_MULTICWD,
    CURLFTPMETHOD_NOCWD,CURLFTPMETHOD_SINGLECWD,
    CURLFTPMETHOD_LAST);

  CURLoption = (
     CURLOPT_FILE := 10000+1,CURLOPT_URL := 10000+2,
     CURLOPT_PORT := 0+3,CURLOPT_PROXY := 10000+4,
     CURLOPT_USERPWD := 10000+5,CURLOPT_PROXYUSERPWD := 10000+6,
     CURLOPT_RANGE := 10000+7,CURLOPT_INFILE := 10000+9,
     CURLOPT_ERRORBUFFER := 10000+10,CURLOPT_WRITEFUNCTION := 20000+11,
     CURLOPT_READFUNCTION := 20000+12,CURLOPT_TIMEOUT := 0+13,
     CURLOPT_INFILESIZE := 0+14,CURLOPT_POSTFIELDS := 10000+15,
     CURLOPT_REFERER := 10000+16,CURLOPT_FTPPORT := 10000+17,
     CURLOPT_USERAGENT := 10000+18,CURLOPT_LOW_SPEED_LIMIT := 0+19,
     CURLOPT_LOW_SPEED_TIME := 0+20,CURLOPT_RESUME_FROM := 0+21,
     CURLOPT_COOKIE := 10000+22,CURLOPT_HTTPHEADER := 10000+23,
     CURLOPT_HTTPPOST := 10000+24,CURLOPT_SSLCERT := 10000+25,
     CURLOPT_SSLCERTPASSWD := 10000+26,CURLOPT_SSLKEYPASSWD := 10000+26,
     CURLOPT_CRLF := 0+27,CURLOPT_QUOTE := 10000+28,
     CURLOPT_WRITEHEADER := 10000+29,CURLOPT_COOKIEFILE := 10000+31,
     CURLOPT_SSLVERSION := 0+32,CURLOPT_TIMECONDITION := 0+33,
     CURLOPT_TIMEVALUE := 0+34,CURLOPT_CUSTOMREQUEST := 10000+36,
     CURLOPT_STDERR := 10000+37,CURLOPT_POSTQUOTE := 10000+39,
     CURLOPT_WRITEINFO := 10000+40,CURLOPT_VERBOSE := 0+41,
     CURLOPT_HEADER := 0+42,CURLOPT_NOPROGRESS := 0+43,
     CURLOPT_NOBODY := 0+44,CURLOPT_FAILONERROR := 0+45,
     CURLOPT_UPLOAD := 0+46,CURLOPT_POST := 0+47,
     CURLOPT_FTPLISTONLY := 0+48,CURLOPT_FTPAPPEND := 0+50,
     CURLOPT_NETRC := 0+51,CURLOPT_FOLLOWLOCATION := 0+52,
     CURLOPT_TRANSFERTEXT := 0+53,CURLOPT_PUT := 0+54,
     CURLOPT_PROGRESSFUNCTION := 20000+56,CURLOPT_PROGRESSDATA := 10000+57,
     CURLOPT_AUTOREFERER := 0+58,CURLOPT_PROXYPORT := 0+59,
     CURLOPT_POSTFIELDSIZE := 0+60,CURLOPT_HTTPPROXYTUNNEL := 0+61,
     CURLOPT_INTERFACE := 10000+62,CURLOPT_KRB4LEVEL := 10000+63,
     CURLOPT_SSL_VERIFYPEER := 0+64,CURLOPT_CAINFO := 10000+65,
     CURLOPT_MAXREDIRS := 0+68,CURLOPT_FILETIME := 0+69,
     CURLOPT_TELNETOPTIONS := 10000+70,CURLOPT_MAXCONNECTS := 0+71,
     CURLOPT_CLOSEPOLICY := 0+72,CURLOPT_FRESH_CONNECT := 0+74,
     CURLOPT_FORBID_REUSE := 0+75,CURLOPT_RANDOM_FILE := 10000+76,
     CURLOPT_EGDSOCKET := 10000+77,CURLOPT_CONNECTTIMEOUT := 0+78,
     CURLOPT_HEADERFUNCTION := 20000+79,CURLOPT_HTTPGET := 0+80,
     CURLOPT_SSL_VERIFYHOST := 0+81,CURLOPT_COOKIEJAR := 10000+82,
     CURLOPT_SSL_CIPHER_LIST := 10000+83,CURLOPT_HTTP_VERSION := 0+84,
     CURLOPT_FTP_USE_EPSV := 0+85,CURLOPT_SSLCERTTYPE := 10000+86,
     CURLOPT_SSLKEY := 10000+87,CURLOPT_SSLKEYTYPE := 10000+88,
     CURLOPT_SSLENGINE := 10000+89,CURLOPT_SSLENGINE_DEFAULT := 0+90,
     CURLOPT_DNS_USE_GLOBAL_CACHE := 0+91,
     CURLOPT_DNS_CACHE_TIMEOUT := 0+92,CURLOPT_PREQUOTE := 10000+93,
     CURLOPT_DEBUGFUNCTION := 20000+94,CURLOPT_DEBUGDATA := 10000+95,
     CURLOPT_COOKIESESSION := 0+96,CURLOPT_CAPATH := 10000+97,
     CURLOPT_BUFFERSIZE := 0+98,CURLOPT_NOSIGNAL := 0+99,
     CURLOPT_SHARE := 10000+100,CURLOPT_PROXYTYPE := 0+101,
     CURLOPT_ENCODING := 10000+102,CURLOPT_PRIVATE := 10000+103,
     CURLOPT_HTTP200ALIASES := 10000+104,CURLOPT_UNRESTRICTED_AUTH := 0+105,
     CURLOPT_FTP_USE_EPRT := 0+106,CURLOPT_HTTPAUTH := 0+107,
     CURLOPT_SSL_CTX_FUNCTION := 20000+108,CURLOPT_SSL_CTX_DATA := 10000+109,
     CURLOPT_FTP_CREATE_MISSING_DIRS := 0+110,
     CURLOPT_PROXYAUTH := 0+111,CURLOPT_FTP_RESPONSE_TIMEOUT := 0+112,
     CURLOPT_IPRESOLVE := 0+113,CURLOPT_MAXFILESIZE := 0+114,
     CURLOPT_INFILESIZE_LARGE := 30000+115,CURLOPT_RESUME_FROM_LARGE := 30000+116,
     CURLOPT_MAXFILESIZE_LARGE := 30000+117,CURLOPT_NETRC_FILE := 10000+118,
     CURLOPT_FTP_SSL := 0+119,CURLOPT_POSTFIELDSIZE_LARGE := 30000+120,
     CURLOPT_TCP_NODELAY := 0+121,CURLOPT_SOURCE_USERPWD := 10000+123,
     CURLOPT_SOURCE_PREQUOTE := 10000+127,CURLOPT_SOURCE_POSTQUOTE := 10000+128,
     CURLOPT_FTPSSLAUTH := 0+129,CURLOPT_IOCTLFUNCTION := 20000+130,
     CURLOPT_IOCTLDATA := 10000+131,CURLOPT_SOURCE_URL := 10000+132,
     CURLOPT_SOURCE_QUOTE := 10000+133,CURLOPT_FTP_ACCOUNT := 10000+134,
     CURLOPT_COOKIELIST := 10000+135,CURLOPT_IGNORE_CONTENT_LENGTH := 0+136,
     CURLOPT_FTP_SKIP_PASV_IP := 0+137,CURLOPT_FTP_FILEMETHOD := 0+138,
     CURLOPT_LOCALPORT := 0+139,CURLOPT_LOCALPORTRANGE := 0+140,
     CURLOPT_CONNECT_ONLY := 0+141,CURLOPT_CONV_FROM_NETWORK_FUNCTION := 20000+142,
     CURLOPT_CONV_TO_NETWORK_FUNCTION := 20000+143,
     CURLOPT_CONV_FROM_UTF8_FUNCTION := 20000+144,
     CURLOPT_MAX_SEND_SPEED_LARGE := 30000+145,
     CURLOPT_MAX_RECV_SPEED_LARGE := 30000+146,
     CURLOPT_FTP_ALTERNATIVE_TO_USER := 10000+147,
     CURLOPT_SOCKOPTFUNCTION := 20000+148,CURLOPT_SOCKOPTDATA := 10000+149,
     CURLOPT_SSL_SESSIONID_CACHE := 0+150,CURLOPT_SSH_AUTH_TYPES := 0+151,
     CURLOPT_SSH_PUBLIC_KEYFILE := 10000+152,CURLOPT_SSH_PRIVATE_KEYFILE := 10000+153,
     CURLOPT_FTP_SSL_CCC := 0+154,CURLOPT_TIMEOUT_MS := 0+155,
     CURLOPT_CONNECTTIMEOUT_MS := 0+156,CURLOPT_HTTP_TRANSFER_DECODING := 0+157,
     CURLOPT_HTTP_CONTENT_DECODING := 0+158,
     CURLOPT_NEW_FILE_PERMS := 0+159,CURLOPT_NEW_DIRECTORY_PERMS := 0+160,
     CURLOPT_POSTREDIR := 0+161,CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 := 10000+162,
     CURLOPT_OPENSOCKETFUNCTION := 20000+163,CURLOPT_OPENSOCKETDATA := 10000+164,
     CURLOPT_COPYPOSTFIELDS := 10000+165,CURLOPT_PROXY_TRANSFER_MODE := 0+166,
     CURLOPT_SEEKFUNCTION := 20000+167,CURLOPT_SEEKDATA := 10000+168,
     CURLOPT_CRLFILE := 10000+169,CURLOPT_ISSUERCERT := 10000+170,
     CURLOPT_ADDRESS_SCOPE := 0+171,CURLOPT_CERTINFO := 0+172,
     CURLOPT_USERNAME := 10000+173,CURLOPT_PASSWORD := 10000+174,
     CURLOPT_PROXYUSERNAME := 10000+175,CURLOPT_PROXYPASSWORD := 10000+176,
     CURLOPT_NOPROXY := 10000+177,CURLOPT_TFTP_BLKSIZE := 0+178,
     CURLOPT_SOCKS5_GSSAPI_SERVICE := 10000+179,
     CURLOPT_SOCKS5_GSSAPI_NEC := 0+180,CURLOPT_PROTOCOLS := 0+181,
     CURLOPT_REDIR_PROTOCOLS := 0+182,CURLOPT_SSH_KNOWNHOSTS := 10000+183,
     CURLOPT_SSH_KEYFUNCTION := 20000+184,CURLOPT_SSH_KEYDATA := 10000+185,
     CURLOPT_MAIL_FROM := 10000+186,CURLOPT_MAIL_RCPT := 10000+187,
     CURLOPT_FTP_USE_PRET := 0+188,CURLOPT_RTSP_REQUEST := 0+189,
     CURLOPT_RTSP_SESSION_ID := 10000+190,CURLOPT_RTSP_STREAM_URI := 10000+191,
     CURLOPT_RTSP_TRANSPORT := 10000+192,CURLOPT_RTSP_CLIENT_CSEQ := 0+193,
     CURLOPT_RTSP_SERVER_CSEQ := 0+194,CURLOPT_INTERLEAVEDATA := 10000+195,
     CURLOPT_INTERLEAVEFUNCTION := 20000+196,CURLOPT_WILDCARDMATCH := 0+197,
     CURLOPT_CHUNK_BGN_FUNCTION := 20000+198,CURLOPT_CHUNK_END_FUNCTION := 20000+199,
     CURLOPT_FNMATCH_FUNCTION := 20000+200,CURLOPT_CHUNK_DATA := 10000+201,
     CURLOPT_FNMATCH_DATA := 10000+202,CURLOPT_RESOLVE := 10000+203,
     CURLOPT_TLSAUTH_USERNAME := 10000+204,CURLOPT_TLSAUTH_PASSWORD := 10000+205,
     CURLOPT_TLSAUTH_TYPE := 10000+206,CURLOPT_TRANSFER_ENCODING := 0+207,
     CURLOPT_CLOSESOCKETFUNCTION := 20000+208,CURLOPT_CLOSESOCKETDATA := 10000+209,
     CURLOPT_GSSAPI_DELEGATION := 0+210,CURLOPT_DNS_SERVERS := 0+211,
     CURLOPT_ACCEPTTIMEOUT_MS := 0+212,CURLOPT_TCP_KEEPALIVE := 0+213,
     CURLOPT_TCP_KEEPIDLE := 0+214,CURLOPT_TCP_KEEPINTVL := 0+215,
     CURLOPT_SSL_OPTIONS := 0+216,CURLOPT_MAIL_AUTH := 0+217,
     CURLOPT_SASL_IR := 0+218,CURLOPT_XFERINFOFUNCTION := 0+219,
     CURLOPT_XOAUTH2_BEARER := 0+220,CURLOPT_DNS_INTERFACE := 0+221,
     CURLOPT_DNS_LOCAL_IP4 := 0+222,CURLOPT_DNS_LOCAL_IP6 := 0+223,
     CURLOPT_LOGIN_OPTIONS := 0+224,CURLOPT_SSL_ENABLE_NPN := 0+225,
     CURLOPT_SSL_ENABLE_ALPN := 0+226,CURLOPT_EXPECT_100_TIMEOUT_MS := 0+227,
     CURLOPT_PROXYHEADER := 0+228,CURLOPT_HEADEROPT := 0+229,
     CURLOPT_PINNEDPUBLICKEY := 0+230,CURLOPT_UNIX_SOCKET_PATH := 0+231,
     CURLOPT_SSL_VERIFYSTATUS := 0+232,CURLOPT_SSL_FALSESTART := 0+233,
     CURLOPT_PATH_AS_IS := 0+234,CURLOPT_PROXY_SERVICE_NAME := 0+235,
     CURLOPT_SERVICE_NAME := 0+236,CURLOPT_PIPEWAIT := 0+237,
     CURLOPT_DEFAULT_PROTOCOL := 0+238,CURLOPT_STREAM_WEIGHT := 0+239,
     CURLOPT_STREAM_DEPENDS := 0+240,CURLOPT_STREAM_DEPENDS_E := 0+241,
     CURLOPT_TFTP_NO_OPTIONS := 0+242,CURLOPT_CONNECT_TO := 0+243,
     CURLOPT_TCP_FASTOPEN := 0+244,CURLOPT_KEEP_SENDING_ON_ERROR := 0+245,
     CURLOPT_PROXY_CAINFO := 0+246,CURLOPT_PROXY_CAPATH := 0+247,
     CURLOPT_PROXY_SSL_VERIFYPEER := 0+248,CURLOPT_PROXY_SSL_VERIFYHOST := 0+249,
     CURLOPT_PROXY_SSLVERSION := 0+250,CURLOPT_PROXY_TLSAUTH_USERNAME := 0+251,
     CURLOPT_PROXY_TLSAUTH_PASSWORD := 0+252,CURLOPT_PROXY_TLSAUTH_TYPE := 0+253,
     CURLOPT_PROXY_SSLCERT := 0+254,CURLOPT_PROXY_SSLCERTTYPE := 0+255,
     CURLOPT_PROXY_SSLKEY := 0+256,CURLOPT_PROXY_SSLKEYTYPE := 0+257,
     CURLOPT_PROXY_KEYPASSWD := 0+258,CURLOPT_PROXY_SSL_CIPHER_LIST := 0+259,
     CURLOPT_PROXY_CRLFILE := 0+260,CURLOPT_PROXY_SSL_OPTIONS := 0+261,
     CURLOPT_PRE_PROXY := 0+262,CURLOPT_PROXY_PINNEDPUBLICKEY := 0+263,
     CURLOPT_LASTENTRY);

  CURL_HTTP_VERSION = (CURL_HTTP_VERSION_NONE,CURL_HTTP_VERSION_1_0,
                       CURL_HTTP_VERSION_1_1,CURL_HTTP_VERSION_2_0,
                       CURL_HTTP_VERSION_2TLS,CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE,
                       CURL_HTTP_VERSION_LAST);

  curl_rtspreq = (CURL_RTSPREQ_NONE,CURL_RTSPREQ_OPTIONS,
      CURL_RTSPREQ_DESCRIBE,CURL_RTSPREQ_ANNOUNCE,
      CURL_RTSPREQ_SETUP,CURL_RTSPREQ_PLAY,
      CURL_RTSPREQ_PAUSE,CURL_RTSPREQ_TEARDOWN,
      CURL_RTSPREQ_GET_PARAMETER,CURL_RTSPREQ_SET_PARAMETER,
      CURL_RTSPREQ_RECORD,CURL_RTSPREQ_RECEIVE,
      CURL_RTSPREQ_LAST);

  CURL_NETRC_OPTION = (CURL_NETRC_IGNORED,CURL_NETRC_OPTIONAL,
                       CURL_NETRC_REQUIRED,CURL_NETRC_LAST);

  CURL_SSL_VERSION = (CURL_SSLVERSION_DEFAULT,CURL_SSLVERSION_TLSv1,
                      CURL_SSLVERSION_SSLv2,CURL_SSLVERSION_SSLv3,
                      CURL_SSLVERSION_LAST);

  CURL_TLSAUTH = (CURL_TLSAUTH_NONE,CURL_TLSAUTH_SRP,CURL_TLSAUTH_LAST);

  CURL_TIMECOND = (CURL_TIMECOND_NONE,CURL_TIMECOND_IFMODSINCE,
                   CURL_TIMECOND_IFUNMODSINCE,CURL_TIMECOND_LASTMOD,
                   CURL_TIMECOND_LAST);

  CURLformoption = (CURLFORM_NOTHING,CURLFORM_COPYNAME,CURLFORM_PTRNAME,
                    CURLFORM_NAMELENGTH,CURLFORM_COPYCONTENTS,
                    CURLFORM_PTRCONTENTS,CURLFORM_CONTENTSLENGTH,
                    CURLFORM_FILECONTENT,CURLFORM_ARRAY,
                    CURLFORM_OBSOLETE,CURLFORM_FILE,CURLFORM_BUFFER,
                    CURLFORM_BUFFERPTR,CURLFORM_BUFFERLENGTH,
                    CURLFORM_CONTENTTYPE,CURLFORM_CONTENTHEADER,
                    CURLFORM_FILENAME,CURLFORM_END,CURLFORM_OBSOLETE2,
                    CURLFORM_STREAM, CURLFORM_LASTENTRY);

  curl_forms = record
    option : CURLformoption;
    value : Pchar;
  end;

  CURLFORMcode = (CURL_FORMADD_OK,CURL_FORMADD_MEMORY,
                  CURL_FORMADD_OPTION_TWICE,CURL_FORMADD_NULL,
                  CURL_FORMADD_UNKNOWN_OPTION,CURL_FORMADD_INCOMPLETE,
                  CURL_FORMADD_ILLEGAL_ARRAY,CURL_FORMADD_DISABLED,
                 CURL_FORMADD_LAST);
  curl_formget_callback = function (arg:pointer; buf:Pchar; len:size_t):size_t;cdecl;

  curl_slist = record
    data : Pchar;
    next : Pcurl_slist;
  end;

  CURLINFO = (CURLINFO_NONE,CURLINFO_EFFECTIVE_URL := $100000+1,
    CURLINFO_RESPONSE_CODE := $200000+2,CURLINFO_TOTAL_TIME := $300000+3,
    CURLINFO_NAMELOOKUP_TIME := $300000+4,CURLINFO_CONNECT_TIME := $300000+5,
    CURLINFO_PRETRANSFER_TIME := $300000+6,CURLINFO_SIZE_UPLOAD := $300000+7,
    CURLINFO_SIZE_DOWNLOAD := $300000+8,CURLINFO_SPEED_DOWNLOAD := $300000+9,
    CURLINFO_SPEED_UPLOAD := $300000+10,CURLINFO_HEADER_SIZE := $200000+11,
    CURLINFO_REQUEST_SIZE := $200000+12,CURLINFO_SSL_VERIFYRESULT := $200000+13,
    CURLINFO_FILETIME := $200000+14,CURLINFO_CONTENT_LENGTH_DOWNLOAD := $300000+15,
    CURLINFO_CONTENT_LENGTH_UPLOAD := $300000+16,
    CURLINFO_STARTTRANSFER_TIME := $300000+17,CURLINFO_CONTENT_TYPE := $100000+18,
    CURLINFO_REDIRECT_TIME := $300000+19,CURLINFO_REDIRECT_COUNT := $200000+20,
    CURLINFO_PRIVATE := $100000+21,CURLINFO_HTTP_CONNECTCODE := $200000+22,
    CURLINFO_HTTPAUTH_AVAIL := $200000+23,CURLINFO_PROXYAUTH_AVAIL := $200000+24,
    CURLINFO_OS_ERRNO := $200000+25,CURLINFO_NUM_CONNECTS := $200000+26,
    CURLINFO_SSL_ENGINES := $400000+27,CURLINFO_COOKIELIST := $400000+28,
    CURLINFO_LASTSOCKET := $200000+29,CURLINFO_FTP_ENTRY_PATH := $100000+30,
    CURLINFO_REDIRECT_URL := $100000+31,CURLINFO_PRIMARY_IP := $100000+32,
    CURLINFO_APPCONNECT_TIME := $300000+33,CURLINFO_CERTINFO := $400000+34,
    CURLINFO_CONDITION_UNMET := $200000+35,CURLINFO_RTSP_SESSION_ID := $100000+36,
    CURLINFO_RTSP_CLIENT_CSEQ := $200000+37,CURLINFO_RTSP_SERVER_CSEQ := $200000+38,
    CURLINFO_RTSP_CSEQ_RECV := $200000+39,CURLINFO_PRIMARY_PORT := $200000+40,
    CURLINFO_LOCAL_IP := $100000+41,CURLINFO_LOCAL_PORT := $200000+42,
    CURLINFO_LASTONE := 42);
 
  curl_closepolicy = (CURLCLOSEPOLICY_NONE,CURLCLOSEPOLICY_OLDEST,
                      CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
                      CURLCLOSEPOLICY_LEAST_TRAFFIC,CURLCLOSEPOLICY_SLOWEST,
                      CURLCLOSEPOLICY_CALLBACK,CURLCLOSEPOLICY_LAST);
 
  curl_lock_data = (CURL_LOCK_DATA_NONE := 0,CURL_LOCK_DATA_SHARE,
                    CURL_LOCK_DATA_COOKIE,CURL_LOCK_DATA_DNS,
                    CURL_LOCK_DATA_SSL_SESSION,CURL_LOCK_DATA_CONNECT,
                    CURL_LOCK_DATA_LAST);
  
  curl_lock_access = (CURL_LOCK_ACCESS_NONE := 0,
                      CURL_LOCK_ACCESS_SHARED := 1,
                      CURL_LOCK_ACCESS_SINGLE := 2,
                      CURL_LOCK_ACCESS_LAST);
 
  curl_lock_function = procedure (handle:PCURL; data:curl_lock_data; locktype:curl_lock_access; userptr:pointer);cdecl;
  curl_unlock_function = procedure (handle:PCURL; data:curl_lock_data; userptr:pointer);cdecl;
 
  CURLSH = pointer;
 
  CURLSHcode = (CURLSHE_OK,CURLSHE_BAD_OPTION,CURLSHE_IN_USE,
                CURLSHE_INVALID,CURLSHE_NOMEM,CURLSHE_LAST);
 
  CURLSHoption = (CURLSHOPT_NONE,CURLSHOPT_SHARE,CURLSHOPT_UNSHARE,
                  CURLSHOPT_LOCKFUNC,CURLSHOPT_UNLOCKFUNC,
                  CURLSHOPT_USERDATA,CURLSHOPT_LAST);

  CURLversion = (CURLVERSION_FIRST,CURLVERSION_SECOND,
                 CURLVERSION_THIRD,CURLVERSION_LAST);

  curl_version_info_data = record
    age : CURLversion;
    version : Pchar;
    version_num : dword;
    host : Pchar;
    features : longint;
    ssl_version : Pchar;
    ssl_version_num : longint;
    libz_version : Pchar;
    protocols : ^Pchar;
    ares : Pchar;
    ares_num : longint;
    libidn : Pchar;
    iconv_ver_num : longint;
    libssh_version : ^char;
  end;
  CURLM = pointer;
 

  CURLMcode = (CURLM_CALL_MULTI_PERFORM := -(1),CURLM_OK,
               CURLM_BAD_HANDLE,CURLM_BAD_EASY_HANDLE,
               CURLM_OUT_OF_MEMORY,CURLM_INTERNAL_ERROR,
               CURLM_BAD_SOCKET,CURLM_UNKNOWN_OPTION, CURLM_LAST);
 
  TCURLMSG = (CURLMSG_NONE,CURLMSG_DONE,CURLMSG_LAST);

  CURLMsg = record
    msg : TCURLMSG;
    easy_handle : PCURL;
    data : record
      case longint of
        0 : ( whatever : pointer );
        1 : ( result : CURLcode );
    end;
  end;
  curl_socket_callback = function (easy:PCURL; s:curl_socket_t; what:longint; userp:pointer; socketp:pointer):longint;cdecl;
  CURLMoption = (
    CURLMOPT_SOCKETFUNCTION := 20000+1,
    CURLMOPT_SOCKETDATA := 10000+2,
    CURLMOPT_PIPELINING := 0+3,
    CURLMOPT_TIMERFUNCTION := 20000+4,
    CURLMOPT_TIMERDATA := 10000+5,
    CURLMOPT_MAXCONNECTS := 0+6,
    CURLMOPT_LASTENTRY);

const
  CURLAUTH_ANY =  not (0);     
  CURLAUTH_BASIC = 1 shl 0;     
  CURLAUTH_ANYSAFE =  not (CURLAUTH_BASIC);     
  CURLAUTH_DIGEST = 1 shl 1;     
  CURLAUTH_GSSNEGOTIATE = 1 shl 2;     
  CURLAUTH_NONE = 0;     
  CURLAUTH_NTLM = 1 shl 3;

  CURL_CHUNK_BGN_FUNC_OK = 0;
  CURL_CHUNK_BGN_FUNC_FAIL = 1;
  CURL_CHUNK_BGN_FUNC_SKIP = 2;
  CURL_CHUNK_END_FUNC_OK = 0;
  CURL_CHUNK_END_FUNC_FAIL = 1;

  CURL_FNMATCHFUNC_MATCH = 0;
  CURL_FNMATCHFUNC_NOMATCH = 1;
  CURL_FNMATCHFUNC_FAIL = 2;

  CURL_SEEKFUNC_OK = 0;
  CURL_SEEKFUNC_FAIL = 1;
  CURL_SEEKFUNC_CANTSEEK = 2;
  CURL_READFUNC_ABORT = $10000000;
  CURL_READFUNC_PAUSE = $10000001;
  CURL_SOCKOPT_OK = 0;
  CURL_SOCKOPT_ERROR = 1;
  CURL_SOCKOPT_ALREADY_CONNECTED = 2;

  CURLE_ALREADY_COMPLETE = 99999;
  CURLE_FTP_BAD_DOWNLOAD_RESUME = CURLE_BAD_DOWNLOAD_RESUME;     
  CURLE_FTP_PARTIAL_FILE = CURLE_PARTIAL_FILE;     
  CURLE_HTTP_NOT_FOUND = CURLE_HTTP_RETURNED_ERROR;     
  CURLE_HTTP_PORT_FAILED = CURLE_INTERFACE_FAILED;     
  CURLE_OPERATION_TIMEDOUT = CURLE_OPERATION_TIMEOUTED;     
  CURLE_FTP_ACCESS_DENIED = CURLE_REMOTE_ACCESS_DENIED;
  CURLE_FTP_COULDNT_SET_BINARY = CURLE_FTP_COULDNT_SET_TYPE;
  CURLE_FTP_QUOTE_ERROR = CURLE_QUOTE_ERROR;
  CURLE_TFTP_DISKFULL = CURLE_REMOTE_DISK_FULL;
  CURLE_TFTP_EXISTS = CURLE_REMOTE_FILE_EXISTS;
  CURLE_HTTP_RANGE_ERROR = CURLE_RANGE_ERROR;
  CURLE_FTP_SSL_FAILED = CURLE_USE_SSL_FAILED;
  CURLE_FTP_COULDNT_STOR_FILE = CURLE_UPLOAD_FAILED;


  CURLFTPSSL_NONE = CURLUSESSL_NONE;
  CURLFTPSSL_TRY = CURLUSESSL_TRY;
  CURLFTPSSL_CONTROL = CURLUSESSL_CONTROL;
  CURLFTPSSL_ALL = CURLUSESSL_ALL;
  CURLFTPSSL_LAST = CURLUSESSL_LAST;

  CURL_ERROR_SIZE = 256;
  CURL_FORMAT_OFF_T = '%ld';     
  CURL_GLOBAL_NOTHING = 0;     
  CURL_GLOBAL_SSL = 1 shl 0;     
  CURL_GLOBAL_WIN32 = 1 shl 1;     
  CURL_GLOBAL_ALL = CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32;     
  CURL_GLOBAL_DEFAULT = CURL_GLOBAL_ALL;     
  CURLINFO_DOUBLE = $300000;     
  CURLINFO_HTTP_CODE = CURLINFO_RESPONSE_CODE;     
  CURLINFO_LONG = $200000;     
  CURLINFO_MASK = $0fffff;     
  CURLINFO_SLIST = $400000;     
  CURLINFO_STRING = $100000;     
  CURLINFO_TYPEMASK = $f00000;     
  CURL_IPRESOLVE_V4 = 1;     
  CURL_IPRESOLVE_V6 = 2;     
  CURL_IPRESOLVE_WHATEVER = 0;     
  CURL_MAX_WRITE_SIZE = 16384;     
  CURLM_CALL_MULTI_SOCKET = CURLM_CALL_MULTI_PERFORM;     
  CURLOPT_CLOSEFUNCTION = -(5);     
  CURLOPT_FTPASCII = CURLOPT_TRANSFERTEXT;     
  CURLOPT_HEADERDATA = CURLOPT_WRITEHEADER;     
  CURLOPT_HTTPREQUEST = -(1);     
  CURLOPT_MUTE = -(2);     
  CURLOPT_PASSWDDATA = -(4);     
  CURLOPT_PASSWDFUNCTION = -(3);     
  CURLOPT_PASV_HOST = -(9);     
  CURLOPT_READDATA = CURLOPT_INFILE;     
  CURLOPT_SOURCE_HOST = -(6);     
  CURLOPT_SOURCE_PATH = -(7);     
  CURLOPT_SOURCE_PORT = -(8);     
  CURLOPTTYPE_FUNCTIONPOINT = 20000;     
  CURLOPTTYPE_LONG = 0;     
  CURLOPTTYPE_OBJECTPOINT = 10000;     
  CURLOPTTYPE_OFF_T = 30000;     
  CURLOPT_WRITEDATA = CURLOPT_FILE;     
  CURL_REDIR_GET_ALL = 0;
  CURL_REDIR_POST_301 = 1;
  CURL_REDIR_POST_302 = 2;
  CURL_POLL_IN = 1;
  CURL_POLL_INOUT = 3;     
  CURL_POLL_NONE = 0;     
  CURL_POLL_OUT = 2;     
  CURL_POLL_REMOVE = 4;     

  CURL_SOCKET_BAD = -(1);      
  CURL_SOCKET_TIMEOUT = CURL_SOCKET_BAD;     
  CURL_VERSION_ASYNCHDNS = 1 shl 7;     
  CURL_VERSION_CONV = 1 shl 12;     
  CURL_VERSION_DEBUG = 1 shl 6;     
  CURL_VERSION_GSSNEGOTIATE = 1 shl 5;     
  CURL_VERSION_IDN = 1 shl 10;     
  CURL_VERSION_IPV6 = 1 shl 0;     
  CURL_VERSION_KERBEROS4 = 1 shl 1;     
  CURL_VERSION_LARGEFILE = 1 shl 9;     
  CURL_VERSION_LIBZ = 1 shl 3;     
  CURLVERSION_NOW = CURLVERSION_THIRD;     
  CURL_VERSION_NTLM = 1 shl 4;     
  CURL_VERSION_SPNEGO = 1 shl 8;     
  CURL_VERSION_SSL = 1 shl 2;     
  CURL_VERSION_SSPI = 1 shl 11;     
  _FILE_OFFSET_BITS = 0;     
  FILESIZEBITS = 0;     
  FUNCTIONPOINT = CURLOPTTYPE_FUNCTIONPOINT;     
  HTTPPOST_BUFFER = 1 shl 4;     
  HTTPPOST_FILENAME = 1 shl 0;     
  HTTPPOST_PTRBUFFER = 1 shl 5;     
  HTTPPOST_PTRCONTENTS = 1 shl 3;     
  HTTPPOST_PTRNAME = 1 shl 2;     
  HTTPPOST_READFILE = 1 shl 1;     
  LIBCURL_COPYRIGHT = '1996 - 2011 Daniel Stenberg, <daniel@haxx.se>.';
  LIBCURL_VERSION = '7.22.0';
  LIBCURL_VERSION_MAJOR = 7;
  LIBCURL_VERSION_MINOR = 22;
  LIBCURL_VERSION_PATCH = 0;
  LIBCURL_VERSION_NUM = $071600;
  LIBCURL_TIMESTAMP = 'Tue Sep 13 16:53:51 UTC 2011';
  CURL_CSELECT_IN = $01;
  CURL_CSELECT_OUT = $02;
  CURL_CSELECT_ERR = $04;

  {$ifdef libcurlstatic}
  function  divdi3(num,den:int64):int64; cdecl;
  function  umoddi3(num,den:uint64):uint64; cdecl;
  function  udivdi3(num,den:uint64):uint64; cdecl;
  function  strtoll(str,endptr:pansichar;base:longint):int64; cdecl;
  function  mingw_basename(str:pansichar):pansichar; cdecl;
  procedure chkstk_ms; cdecl;
  procedure assert(const str1:pansichar; const str2:pansichar; anum:longint); cdecl;
  {$endif}

{$ifndef libcurlstatic}
var
{$endif}

{$ifdef libcurlstatic}function {$endif}curl_strequal{$ifndef libcurlstatic}: function{$endif}(s1:Pchar; s2:Pchar):longint;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_strnequal{$ifndef libcurlstatic}: function{$endif}(s1:Pchar; s2:Pchar; n:size_t):longint;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_formadd{$ifndef libcurlstatic}: function{$endif}(httppost:PPcurl_httppost; last_post:PPcurl_httppost):CURLFORMcode;cdecl varargs;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_formget{$ifndef libcurlstatic}: function{$endif}(form:Pcurl_httppost; arg:pointer; append:curl_formget_callback):longint;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}procedure {$endif}curl_formfree{$ifndef libcurlstatic}: procedure{$endif}(form:Pcurl_httppost);cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_getenv{$ifndef libcurlstatic}: function{$endif}(variable:Pchar):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_version{$ifndef libcurlstatic}: function{$endif}:Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_escape{$ifndef libcurlstatic}: function{$endif}(handle:PCURL; _string:Pchar; length:longint):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_escape{$ifndef libcurlstatic}: function{$endif}(_string:Pchar; length:longint):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_unescape{$ifndef libcurlstatic}: function{$endif}(handle:PCURL; _string:Pchar; length:longint; outlength:Plongint):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_unescape{$ifndef libcurlstatic}: function{$endif}(_string:Pchar; length:longint):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}procedure {$endif}curl_free{$ifndef libcurlstatic}: procedure{$endif}(p:pointer);cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_global_init{$ifndef libcurlstatic}: function{$endif}(flags:longint):CURLcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_global_init_mem{$ifndef libcurlstatic}: function{$endif}(flags:longint; m:curl_malloc_callback; f:curl_free_callback; r:curl_realloc_callback; s:curl_strdup_callback; c:curl_calloc_callback):CURLcode;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}procedure {$endif}curl_global_cleanup{$ifndef libcurlstatic}: procedure{$endif};cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_slist_append {$ifndef libcurlstatic}: function{$endif}(curl_slist : Pcurl_slist; P : PChar) : Pcurl_slist; cdecl; {$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}procedure {$endif}curl_slist_free_all{$ifndef libcurlstatic}: procedure{$endif}(_para1:Pcurl_slist);cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_getdate{$ifndef libcurlstatic}: function{$endif}(p:Pchar; unused:Ptime_t):time_t;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_share_init{$ifndef libcurlstatic}: function{$endif}:PCURLSH;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_share_setopt{$ifndef libcurlstatic}: function{$endif}(_para1:PCURLSH; option:CURLSHoption):CURLSHcode;cdecl varargs;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_share_cleanup{$ifndef libcurlstatic}: function{$endif}(_para1:PCURLSH):CURLSHcode;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_version_info{$ifndef libcurlstatic}: function{$endif}(_para1:CURLversion):Pcurl_version_info_data;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_strerror{$ifndef libcurlstatic}: function{$endif}(_para1:CURLcode):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_share_strerror{$ifndef libcurlstatic}: function{$endif}(_para1:CURLSHcode):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_init{$ifndef libcurlstatic}: function{$endif}:PCURL;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_setopt{$ifndef libcurlstatic}: function{$endif}(curl:PCURL; option:CURLoption):CURLcode;cdecl varargs;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_perform{$ifndef libcurlstatic}: function{$endif}(curl:PCURL):CURLcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}procedure {$endif}curl_easy_cleanup{$ifndef libcurlstatic}: procedure{$endif}(curl:PCURL);cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_getinfo{$ifndef libcurlstatic}: function{$endif}(curl:PCURL; info:CURLINFO):CURLcode;cdecl varargs;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_easy_duphandle{$ifndef libcurlstatic}: function{$endif}(curl:PCURL):PCURL;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}procedure {$endif}curl_easy_reset{$ifndef libcurlstatic}: procedure{$endif}(curl:PCURL);cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_multi_init{$ifndef libcurlstatic}: function{$endif}:PCURLM;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_add_handle{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; curl_handle:PCURL):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_remove_handle{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; curl_handle:PCURL):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_fdset{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; read_fd_set:Pfd_set; write_fd_set:Pfd_set; exc_fd_set:Pfd_set; max_fd:Plongint):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_perform{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; running_handles:Plongint):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_cleanup{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_info_read{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; msgs_in_queue:Plongint):PCURLMsg;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_strerror{$ifndef libcurlstatic}: function{$endif}(_para1:CURLMcode):Pchar;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_multi_socket{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; s:curl_socket_t; running_handles:Plongint):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_socket_all{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; running_handles:Plongint):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_timeout{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; milliseconds:Plongint):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}

{$ifdef libcurlstatic}function {$endif}curl_multi_setopt{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; option:CURLMoption):CURLMcode;cdecl varargs;{$ifdef libcurlstatic}external;{$endif}
{$ifdef libcurlstatic}function {$endif}curl_multi_assign{$ifndef libcurlstatic}: function{$endif}(multi_handle:PCURLM; sockfd:curl_socket_t; sockp:pointer):CURLMcode;cdecl;{$ifdef libcurlstatic}external;{$endif}

function LoadCurlLibrary:boolean;

implementation

{$ifndef libcurlstatic}

uses
  DynLibs;

var
  libcurl: TLibHandle = NilHandle;
{$endif}

function LoadCurlLibrary:boolean;
begin
  {$ifdef libcurlstatic}
  result:=true;
  {$else}
  result:=(libcurl <> NilHandle);

  if result then exit;

  try
    libcurl:= LoadLibrary(External_library+'.'+SharedSuffix);
    {$ifdef MSWINDOWS}
    if libcurl = NilHandle then libcurl:= LoadLibrary(External_library+'-4.'+SharedSuffix);
    {$endif}
    if libcurl = NilHandle then libcurl:= LoadLibrary(External_library+'.'+SharedSuffix+'.4');
    if libcurl = NilHandle then libcurl:= LoadLibrary(External_library+'.'+SharedSuffix+'.3');
    if libcurl = NilHandle then libcurl:= LoadLibrary(External_library+'.'+SharedSuffix+'.0');

    if (libcurl <> NilHandle) then
    try
      pointer(curl_strequal):= GetProcAddress(libcurl, 'curl_strequal');
      pointer(curl_strnequal):= GetProcAddress(libcurl, 'curl_strnequal');

      pointer(curl_formadd):= GetProcAddress(libcurl, 'curl_formadd');

      pointer(curl_formget):= GetProcAddress(libcurl, 'curl_formget');
      pointer(curl_formfree):= GetProcAddress(libcurl, 'curl_formfree');
      pointer(curl_getenv):= GetProcAddress(libcurl, 'curl_getenv');
      pointer(curl_version):= GetProcAddress(libcurl, 'curl_version');
      pointer(curl_easy_escape):= GetProcAddress(libcurl, 'curl_easy_escape');
      pointer(curl_escape):= GetProcAddress(libcurl, 'curl_escape');
      pointer(curl_easy_unescape):= GetProcAddress(libcurl, 'curl_easy_unescape');
      pointer(curl_unescape):= GetProcAddress(libcurl, 'curl_unescape');
      pointer(curl_free):= GetProcAddress(libcurl, 'curl_free');
      pointer(curl_global_init):= GetProcAddress(libcurl, 'curl_global_init');
      pointer(curl_global_init_mem):= GetProcAddress(libcurl, 'curl_global_init_mem');

      pointer(curl_global_cleanup):= GetProcAddress(libcurl, 'curl_global_cleanup');
      pointer(curl_slist_append):= GetProcAddress(libcurl, 'curl_slist_append');
      pointer(curl_slist_free_all):= GetProcAddress(libcurl, 'curl_slist_free_all');
      pointer(curl_getdate):= GetProcAddress(libcurl, 'curl_getdate');

      pointer(curl_share_init):= GetProcAddress(libcurl, 'curl_share_init');
      pointer(curl_share_setopt):= GetProcAddress(libcurl, 'curl_share_setopt');
      pointer(curl_share_cleanup):= GetProcAddress(libcurl, 'curl_share_cleanup');

      pointer(curl_version_info):= GetProcAddress(libcurl, 'curl_version_info');
      pointer(curl_easy_strerror):= GetProcAddress(libcurl, 'curl_easy_strerror');
      pointer(curl_share_strerror):= GetProcAddress(libcurl, 'curl_share_strerror');
      pointer(curl_easy_init):= GetProcAddress(libcurl, 'curl_easy_init');
      pointer(curl_easy_setopt):= GetProcAddress(libcurl, 'curl_easy_setopt');
      pointer(curl_easy_perform):= GetProcAddress(libcurl, 'curl_easy_perform');
      pointer(curl_easy_cleanup):= GetProcAddress(libcurl, 'curl_easy_cleanup');
      pointer(curl_easy_getinfo):= GetProcAddress(libcurl, 'curl_easy_getinfo');
      pointer(curl_easy_duphandle):= GetProcAddress(libcurl, 'curl_easy_duphandle');
      pointer(curl_easy_reset):= GetProcAddress(libcurl, 'curl_easy_reset');

      pointer(curl_multi_init):= GetProcAddress(libcurl, 'curl_multi_init');
      pointer(curl_multi_add_handle):= GetProcAddress(libcurl, 'curl_multi_add_handle');
      pointer(curl_multi_remove_handle):= GetProcAddress(libcurl, 'curl_multi_remove_handle');
      pointer(curl_multi_fdset):= GetProcAddress(libcurl, 'curl_multi_fdset');
      pointer(curl_multi_perform):= GetProcAddress(libcurl, 'curl_multi_perform');
      pointer(curl_multi_cleanup):= GetProcAddress(libcurl, 'curl_multi_cleanup');
      pointer(curl_multi_info_read):= GetProcAddress(libcurl, 'curl_multi_info_read');
      pointer(curl_multi_strerror):= GetProcAddress(libcurl, 'curl_multi_strerror');

      pointer(curl_multi_socket):= GetProcAddress(libcurl, 'curl_multi_socket');
      pointer(curl_multi_socket_all):= GetProcAddress(libcurl, 'curl_multi_socket_all');
      pointer(curl_multi_timeout):= GetProcAddress(libcurl, 'curl_multi_timeout');

      pointer(curl_multi_setopt):= GetProcAddress(libcurl, 'curl_multi_setopt');
      pointer(curl_multi_assign):= GetProcAddress(libcurl, 'curl_multi_assign');

      result:=true;

    except
      UnloadLibrary(libcurl);
      libcurl := NilHandle;
    end;

  except
    UnloadLibrary(libcurl);
    libcurl := NilHandle;
  end;
  {$endif}
end;

{$ifdef libcurlstatic}
function divdi3(num,den:int64):int64; cdecl; [public, alias: '___divdi3'];
begin
 result:=num div den;
end;

function umoddi3(num,den:uint64):uint64; cdecl; [public, alias: '___umoddi3'];
begin
 result:=num mod den;
end;

function udivdi3(num,den:uint64):uint64; cdecl; [public, alias: '___udivdi3'];
begin
 result:=num div den;
end;

function strtoll(str,endptr:pansichar;base:longint):int64; cdecl; [alias: '_strtoll'];
var s:longint;
begin
 result:=0;
 s:=1;
 while (str^<>#0) and (ptruint(str)<ptruint(endptr)) do begin
  case str^ of
   '-':begin
    s:=-s;
   end;
   '0'..'9': result:=(result*base)+(byte(ansichar(str^))-byte(ansichar('0')));
   'a'..'z': result:=(result*base)+((byte(ansichar(str^))-byte(ansichar('a')))+$a);
   'A'..'Z': result:=(result*base)+((byte(ansichar(str^))-byte(ansichar('A')))+$a);
  end;
  inc(str^);
 end;
end;

function malloc(size:longint):pointer; cdecl; external name 'malloc';

function mingw_basename(str:pansichar):pansichar; cdecl; [public, alias: '___mingw_basename'];
var
  l:integer;
  s:string;
begin
  s:=str;
  s:=ExtractFileName(s);
  l:=length(s);
  result:=malloc(l+1);
  move(s,result^,l);
  result[l]:=#0;
end;

procedure chkstk_ms; cdecl; [public, alias: '___chkstk_ms'];
begin
 // not implemented
end;

procedure assert (const str1:pansichar; const str2:pansichar; anum:longint); cdecl; [public, alias: '__assert'];
begin
 // not implemented
end;
{$endif}

{$ifndef libcurlstatic}
finalization
  if (libcurl <> NilHandle) then
  begin
    curl_global_cleanup;
    FreeLibrary(libcurl);
  end;
{$endif}

end.
