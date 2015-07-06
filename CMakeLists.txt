project (gpsfish CXX)
set (CMAKE_VERBOSE_MAKEFILE on)

# Tweak build mode
if(NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()

if(NOT CMAKE_BUILD_TYPE MATCHES "^[Dd][Ee][Bb][Uu][Gg]")
        add_definitions(-DQT_NO_DEBUG_OUTPUT)
        set(CMAKE_BUILD_RELEASE TRUE)
else()
        set(CMAKE_BUILD_RELEASE FALSE)
endif()

# Installation directory
if(APPLE AND CMAKE_INSTALL_PREFIX MATCHES "/usr/local")
        set(CMAKE_INSTALL_PREFIX "/Applications")
endif()

message(STATUS "Building ${PROJECT_NAME} in ${CMAKE_BUILD_TYPE} mode")
cmake_minimum_required(VERSION 3.2)
set (CMAKE_VERBOSE_MAKEFILE on)

#ENABLE_TESTING()

# Basic build options
if(CMAKE_COMPILER_IS_GNUCXX OR MINGW OR APPLE)
  #if(CMAKE_BUILD_RELEASE)
  #        add_definitions(-Werror)
  #endif()
  set (CMAKE_CXX_FLAGS "-Wall -D_REENTRANT")
  set (CMAKE_CXX_FLAGS_DEBUG "-O0 -g -DDEBUG")
  set (CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG -funroll-loops -funswitch-loops -fpeel-loops --param large-function-growth=48000 --param inline-unit-growth=8000 --param max-inline-insns-single=8000 -fomit-frame-pointer")
  # GPSFISH
  add_definitions(-std=c++11)
endif()

# Compiler specific build options
if(MINGW)
  set (CMAKE_CXX_FLAGS "-Wall -D_REENTRANT -D_MT -m64 -mthreads -mwin32 -msse2 -static-libgcc -static-libstdc++ -DWIN32_LEAN_AND_MEAN -mstackrealign -fno-strict-aliasing")
  #set (CMAKE_CXX_FLAGS "-Wall -D_REENTRANT -D_MT -m64 -mthreads -mwin32 -msse2 -static -DWIN32_LEAN_AND_MEAN -mstackrealign -fno-strict-aliasing")
  add_definitions(-march=core2)
  add_definitions(-D_WIN32_WINNT=0x0600)

  find_library(Winsock2_LIBRARY ws2_32)
  add_definitions(-DGPSFISH_HOME="${CMAKE_CURRENT_SOURCE_DIR}/src")
elseif (APPLE)
  set (CMAKE_CXX_COMPILER "/usr/local/bin/g++-4.7")
  set (CMAKE_CXX_FLAGS "-Wall -D_REENTRANT -m64 -fno-common")
  add_definitions(-march=core2)
  add_definitions(-DGPSFISH_HOME="/Applications/${PROJECT_NAME}.app")
endif()

include_directories ("${PROJECT_SOURCE_DIR}/src")

#
# Third-party libraries
#

#if (MINGW)
#  ## bzip2
#  find_library(Bz2_LIBRARY bz2 PATH "/usr/i686-w64-mingw32/lib" NO_DEFAULT_PATH)
#  ## psapi
#  find_library(Psapi_LIBRARY psapi PATH "/usr/i686-w64-mingw32/lib" NO_DEFAULT_PATH)
#else()
#  find_library(Bz2_LIBRARY bz2)
#  find_library(Iconv_LIBRARY iconv)
#endif()

## gpsfish
# TODO:  src/mainone.cc
file(GLOB_RECURSE GPSFISH_HEADERS  src/*.h)
set(GPSFISH_SRCS src/benchmark.cpp
  src/book.cpp
  src/evaluate.cpp
  src/main.cpp
  src/misc.cpp
  src/move.cpp
  src/movegen.cpp
  src/movepick.cpp
  src/osl_checkmate.cc
  src/osl_eval.cc
  src/osl_position.cc
  src/osl_types.cc
  src/position.cpp
  src/search.cpp
  src/tables.cc
  src/thread.cpp
  src/timeman.cpp
  src/tt.cpp
  src/uci.cpp
  src/ucioption.cpp
  src/usi.cc
  src/whitelist.cc)
set(GPSFISH_ALL_SRCS ${GPSFISH_HEADERS} ${GPSFISH_SRCS})
add_executable(gpsfish MACOSX_BUNDLE ${GPSFISH_ALL_SRCS})
if (MINGW)
  #target_link_libraries (gpsfish ${Psapi_LIBRARY})
  target_link_libraries (gpsfish ${Winsock2_LIBRARY})
else()
  target_link_libraries (gpsfish ${Iconv_LIBRARY})
endif()

set (GPSFISH_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

##
## Installer
##
set(PROJECT_VERSION_MAJOR "20150705")
set(PROJECT_VERSION_MINOR "1")
set(PROJECT_VERSION_PATCH "0")
set(PROJECT_VERSION_COUNT 0)
set(PROJECT_VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")
set(PROJECT_VENDOR "TeamGPS")
set(PROJECT_COPYRIGHT_YEAR "2015")

set(ICONS_DIR "${PROJECT_SOURCE_DIR}/icons")
if(APPLE)
  set(BIN_INSTALL_DIR ".")
  set(DOC_INSTALL_DIR "${PROJECT_NAME}.app")
  set(DATA_INSTALL_DIR "${PROJECT_NAME}.app/data")
else()
  string(TOLOWER ${PROJECT_NAME} PROJECT_NAME_LOWERCASE)
  set(BIN_INSTALL_DIR "smp")
  set(DOC_INSTALL_DIR "smp")
  set(DATA_INSTALL_DIR "./")
  set(DOC_INSTALL_DIR "./")
endif()

if(APPLE)
  set(MACOSX_BUNDLE_INFO_STRING "${PROJECT_NAME} ${PROJECT_VERSION}")
  set(MACOSX_BUNDLE_BUNDLE_VERSION "${PROJECT_NAME} ${PROJECT_VERSION}")
  set(MACOSX_BUNDLE_LONG_VERSION_STRING "${PROJECT_NAME} ${PROJECT_VERSION}")
  set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${PROJECT_VERSION}")
  set(MACOSX_BUNDLE_COPYRIGHT "${PROJECT_COPYRIGHT_YEAR} ${PROJECT_VENDOR}")
  #set(MACOSX_BUNDLE_ICON_FILE "audio-input-microphone.icns")
  set(MACOSX_BUNDLE_GUI_IDENTIFIER "gps.tanaka.ecc.u-tokyo.ac.jp")
  set(MACOSX_BUNDLE_BUNDLE_NAME "${PROJECT_NAME}")
  set(MACOSX_BUNDLE_RESOURCES "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.app/Contents/Resources")
  #set(MACOSX_BUNDLE_ICON "${ICONS_DIR}/${MACOSX_BUNDLE_ICON_FILE}")
  execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${MACOSX_BUNDLE_RESOURCES})
  execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MACOSX_BUNDLE_ICON} ${MACOSX_BUNDLE_RESOURCES})
endif()

if(APPLE)
        set(CMAKE_INSTALL_PREFIX "/Applications")
endif()

message(STATUS "${PROJECT_NAME} will be installed to ${CMAKE_INSTALL_PREFIX}")


# CPack

set(CPACK_GENERATOR "TBZ2")
set (CPACK_PACKAGE_VENDOR "${PROJECT_VENDOR}")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "GPSFish by TeamGPS")
if (MINGW)
  set (CPACK_PACKAGE_DESCRIPTION_FILE "${GPSFISH_SOURCE_DIR}/sample/binary-dist/gpsfish_setup/README.ja.txt")
else()
  set (CPACK_PACKAGE_DESCRIPTION_FILE "${GPSFISH_SOURCE_DIR}/sample/binary-dist/gpsfish_setup/README.mac.txt")
endif()
set (CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
set (CPACK_RESOURCE_FILE_LICENSE "${GPSFISH_SOURCE_DIR}/sample/binary-dist/GPL-3")
#set (CPACK_PACKAGE_EXECUTABLES gpsshogi;gpsshogi) # do not create start menu
set (CPACK_PACKAGE_INSTALL_DIRECTORY "gpsfish")

if (MINGW)
  # Specific to NSIS (Windows)
  set (CPACK_GENERATOR "NSIS")
  #set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY HKCU "/Environment/OSL_HOME" ${CPACK_NSIS_INSTALL_ROOT})
  set (CPACK_NSIS_PACKAGE_NAME "GPSFish")
  set (CPACK_NSIS_DISPLAY_NAME "GPSFish")
  set (CPACK_NSIS_URL_INFO_ABOUT "http://gps.tanaka.ecc.u-tokyo.ac.jp/gpsshogi/")
  set (CPACK_NSIS_HELP_LINK "http://gps.tanaka.ecc.u-tokyo.ac.jp/gpsshogi/")
  ## registry key for OSL_HOME environment variable
  set (CPACK_NSIS_EXTRA_INSTALL_COMMANDS   "WriteRegStr HKCU \\\"Environment\\\" \\\"GPSFISH64_HOME\\\" \\\"$INSTDIR\\\"")
  set (CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "DeleteRegValue HKCU \\\"Environment\\\" \\\"GPSFISH64_HOME\\\"")
elseif (APPLE)
  set(CMAKE_INSTALL_PREFIX "/Applications")
  set(CPACK_GENERATOR "DragNDrop")
  set(CPACK_DMG_FORMAT "UDBZ")
  set(CPACK_DMG_VOLUME_NAME "${PROJECT_NAME}")
  set(CPACK_SYSTEM_NAME "OSX")
  set(CPACK_PACKAGE_FILE_NAME "${PROJECT_NAME}-${PROJECT_VERSION}")
  #set(CPACK_PACKAGE_ICON "${ICONS_DIR}/DMG.icns")
  #set(CPACK_DMG_DS_STORE "${ICONS_DIR}/DMGDSStore")
  #set(CPACK_DMG_BACKGROUND_IMAGE "${ICONS_DIR}/DMGBackground.png")
elseif(UNIX)
  set(CPACK_SYSTEM_NAME "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
endif()

## Binary
install (TARGETS gpsfish DESTINATION ${BIN_INSTALL_DIR})
## Data files
set (GPSFISH_DATAFILES
  "${CMAKE_CURRENT_SOURCE_DIR}/src/book.txt"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/eval.txt"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/progress.txt")
install (FILES ${GPSFISH_DATAFILES} DESTINATION "${DATA_INSTALL_DIR}")
## README
if (MINGW)
  install (FILES "${CMAKE_CURRENT_SOURCE_DIR}/sample/binary-dist/gpsfish_setup/README.txt"
                 "${CMAKE_CURRENT_SOURCE_DIR}/sample/binary-dist/gpsfish_setup/README.ja.txt"
           DESTINATION "./")
else()
  install (FILES "${CMAKE_CURRENT_SOURCE_DIR}/sample/binary-dist/gpsfish_setup/README.mac.txt"
           DESTINATION "./")
endif()
## licenses for data derived from GPS
#install (FILES
#  "${GPS_SOURCE_DIR}/AUTHORS"
#  "${GPS_SOURCE_DIR}/COPYING"
#  "${GPS_SOURCE_DIR}/LICENSE"
#  DESTINATION "data")
## licenses for binaries
if(MINGW)
  ### Mingw
  install (FILES "/usr/share/doc/mingw-w64/copyright" DESTINATION "${DOC_INSTALL_DIR}" RENAME "copyright.mingw")
endif()

##
## gpsfish
##
if(MINGW)
  install (PROGRAMS "${GPSFISH_SOURCE_DIR}/build-Release/gpsfish.exe"
           DESTINATION "${BIN_INSTALL_DIR}")
  install (PROGRAMS "/usr/x86_64-w64-mingw32/lib/libwinpthread-1.dll" 
           DESTINATION "${BIN_INSTALL_DIR}")
endif()

### GPL
install (FILES "${CMAKE_CURRENT_SOURCE_DIR}/sample/binary-dist/GPL-3" DESTINATION "${DOC_INSTALL_DIR}")
install (FILES "${GPSFISH_SOURCE_DIR}/AUTHORS.gpsfish"                DESTINATION "${DOC_INSTALL_DIR}")
install (FILES "${GPSFISH_SOURCE_DIR}/AUTHORS.stockfish" DESTINATION "${DOC_INSTALL_DIR}")
install (FILES "${GPSFISH_SOURCE_DIR}/Copying.txt"       DESTINATION "${DOC_INSTALL_DIR}" RENAME "Copying.txt.stockfish")
install (FILES "${GPSFISH_SOURCE_DIR}/Readme.txt"        DESTINATION "${DOC_INSTALL_DIR}" RENAME "Readme.txt.stockfish")

if(APPLE)
  set(CMAKE_INSTALL_SYSTEM_RUNTIME_DESTINATION "${BIN_INSTALL_DIR}")
  include(InstallRequiredSystemLibraries)

  set(EXECUTABLE "${PROJECT_NAME}.app")
elseif(WIN32)
  set(EXECUTABLE "${PROJECT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
else()
  set(EXECUTABLE "${BIN_INSTALL_DIR}/${PROJECT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
endif()

include (CPack)

