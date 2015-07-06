#ifndef _OSL_CONFIG_H
#define _OSL_CONFIG_H

#ifndef _WIN32
#  include <stdint.h>
#endif

// for helgrind or drd
// #define OSL_USE_RACE_DETECTOR

#ifdef OSL_USE_RACE_DETECTOR
#  ifndef OSL_NO_SSE
#    define OSL_NO_SSE 1
#  endif
#endif

#if 0
#ifdef _MSC_VER
#pragma warning( disable : 4099 )
#pragma warning( disable : 4146 )
#pragma warning( disable : 4244 )
#pragma warning( disable : 4267 )
#pragma warning( disable : 4661 )
#pragma warning( disable : 4800 )
#pragma warning( disable : 4805 )
#pragma warning( disable : 4906 )
#pragma warning( disable : 4996 )
#define OSL_NO_SSE 1
#endif
#endif

#ifndef OSL_NO_SSE
#if (defined __x86_64__) || (defined __i386__) || defined(__INTEL_COMPILER)
#  ifndef OSL_USE_SSE
#  define OSL_USE_SSE 1
#  endif
#else
#  warning "QuadInt without SSE"
#endif
#endif

#ifdef __SSE4_2__
#define OSL_USE_SSE41 1
#endif

#endif /* _OSL_CONFIG_H */
