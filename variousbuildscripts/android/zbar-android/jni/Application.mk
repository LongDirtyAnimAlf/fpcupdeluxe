APP_ABI := armeabi-v7a arm64-v8a x86 x86_64
APP_PLATFORM := android-21
APP_CPPFLAGS += -fexceptions -frtti
APP_CFLAGS += -Wno-parentheses-equality -Wno-shift-op-parentheses -Wno-logical-op-parentheses -Wno-bitwise-op-parentheses -Wno-initializer-overrides -Wno-empty-body
APP_STL := c++_shared