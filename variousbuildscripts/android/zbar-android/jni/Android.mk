#
# Android NDK makefile 
#
# build - <ndk path>/ndk-build ICONV_SRC=<iconv library src> 
# clean -  <ndk path>/ndk-build clean
#
LOCAL_PATH := $(call my-dir)

# libiconv
include $(CLEAR_VARS)

ICONV_SRC := libiconv-1.16

LOCAL_MODULE := libiconv

LOCAL_CFLAGS := \
    -Wno-multichar \
    -DANDROID \
    -D_ANDROID \
    -DLIBDIR="c" \
    -DBUILDING_LIBICONV \
    -DBUILDING_LIBCHARSET \
    -DIN_LIBRARY

LOCAL_SRC_FILES := \
  $(ICONV_SRC)/libcharset/lib/localcharset.c \
  $(ICONV_SRC)/lib/iconv.c \
  $(ICONV_SRC)/lib/relocatable.c

LOCAL_C_INCLUDES += \
  $(LOCAL_PATH)/$(ICONV_SRC)/include \
  $(LOCAL_PATH)/$(ICONV_SRC)/libcharset \
  $(LOCAL_PATH)/$(ICONV_SRC)/lib \
  $(LOCAL_PATH)/$(ICONV_SRC)/libcharset/include \
  $(LOCAL_PATH)/$(ICONV_SRC)/srclib
 
LOCAL_EXPORT_C_INCLUDES       := $(LOCAL_PATH)/$(ICONV_SRC)/include

include $(BUILD_SHARED_LIBRARY)
# include $(BUILD_STATIC_LIBRARY)


LOCAL_LDLIBS := -llog -lcharset

# libzbarjni
include $(CLEAR_VARS)

ZBAR_SRC := zbar

LOCAL_MODULE := zbarjni
LOCAL_SRC_FILES := \
		   java/zbarjni.c \
		   $(ZBAR_SRC)/img_scanner.c \
		   $(ZBAR_SRC)/decoder.c \
		   $(ZBAR_SRC)/image.c \
		   $(ZBAR_SRC)/symbol.c \
		   $(ZBAR_SRC)/convert.c \
		   $(ZBAR_SRC)/config.c \
		   $(ZBAR_SRC)/scanner.c \
		   $(ZBAR_SRC)/error.c \
		   $(ZBAR_SRC)/refcnt.c \
		   $(ZBAR_SRC)/video.c \
		   $(ZBAR_SRC)/video/null.c \
		   $(ZBAR_SRC)/decoder/code128.c \
		   $(ZBAR_SRC)/decoder/code39.c \
		   $(ZBAR_SRC)/decoder/code93.c \
		   $(ZBAR_SRC)/decoder/codabar.c \
		   $(ZBAR_SRC)/decoder/databar.c \
		   $(ZBAR_SRC)/decoder/ean.c \
		   $(ZBAR_SRC)/decoder/i25.c \
		   $(ZBAR_SRC)/decoder/qr_finder.c \
		   $(ZBAR_SRC)/qrcode/bch15_5.c \
		   $(ZBAR_SRC)/qrcode/binarize.c \
		   $(ZBAR_SRC)/qrcode/isaac.c \
		   $(ZBAR_SRC)/qrcode/qrdec.c \
		   $(ZBAR_SRC)/qrcode/qrdectxt.c \
		   $(ZBAR_SRC)/qrcode/rs.c \
		   $(ZBAR_SRC)/qrcode/util.c

LOCAL_C_INCLUDES := \
		  $(LOCAL_PATH)/$(ZBAR_SRC) \
		  $(LOCAL_PATH)/$(ZBAR_SRC)/include \
		  $(LOCAL_PATH)/$(ICONV_SRC)/include

LOCAL_SHARED_LIBRARIES := libiconv

include $(BUILD_SHARED_LIBRARY)