export CFLAGS='-O2 -pipe -static'
export CXXFLAGS='-O2 -pipe -static-libstdc++ -static-libgcc'
export LDFLAGS='-static'

.//configure \
    --disable-pthreads --disable-threads --disable-shared --enable-static --prefix=/home/Alfred/curl --disable-ldap \
    --disable-sspi --without-librtmp --enable-ftp --disable-file --disable-dict --without-brotli \
    --disable-telnet --disable-tftp --disable-rtsp --disable-pop3 --disable-imap --with-winssl --with-schannel \
    --disable-smtp --disable-gopher --disable-smb --without-libidn --enable-ares --without-ssl \
    2>&1 | tee curl_configure.log

make    clean
make    all 2>&1 | tee curl_make.log
make    install 2>&1 | tee curl_install.log
