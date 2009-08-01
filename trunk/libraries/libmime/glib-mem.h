/*
* C++ Interface: glib-mem
*
* Description: Compatibility macros for older versions of glib
*
*
* Author: Hong Jen Yee (PCMan) <pcman.tw (AT) gmail.com>, (C) 2006
*
* Copyright: See COPYING file that comes with this distribution
*
*/

#ifndef _GLIB_MEM_H_
#define _GLIB_MEM_H_

#include <glib.h>

#if ! GLIB_CHECK_VERSION(2, 10, 0)
/* older versions of glib don't provde g_slice API */
#define g_slice_alloc(size)         g_malloc(size)
#define g_slice_alloc0(size)        g_malloc0(size)
#define g_slice_new(type)           g_new(type, 1)
#define g_slice_new0(type)          g_new0(type, 1)
#define g_slice_free(type, mem)     g_free(mem)
#define g_slice_free1(size, mem)    g_free(mem)
#endif

#endif

