/*
 *      mime-action.h
 *
 *      Copyright 2007 PCMan <pcman.tw@gmail.com>
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *      MA 02110-1301, USA.
 */

#ifndef _MIME_ACTION_H_INCLUDED_
#define _MIME_ACTION_H_INCLUDED_

#include <glib.h>

G_BEGIN_DECLS

/*
 *  Get a list of applications supporting this mime-type
 * The returned string array was newly allocated, and should be
 * freed with g_strfreev() when no longer used.
 */
char** mime_type_get_actions( const char* type );

/*
 * Add an applications used to open this mime-type
 * desktop_id is the name of *.desktop file.
 *
 * custom_desktop: used to store name of the newly created user-custom desktop file, can be NULL.
 */
void mime_type_add_action( const char* type, const char* desktop_id, char** custom_desktop );

/*
 * Check if an applications currently set to open this mime-type
 * desktop_id is the name of *.desktop file.
 */
gboolean mime_type_has_action( const char* type, const char* desktop_id );

/*
 *  Get default applications used to open this mime-type
 * The returned string was newly allocated, and should be
 * freed when no longer used.
 * If NULL is returned, that means default app is not set for this mime-type.
 */
char* mime_type_get_default_action( const char* type );

/*
 *  Set default applications used to open this mime-type
 *  desktop_id is the name of *.desktop file.
 */
void mime_type_set_default_action( const char* type, const char* desktop_id );

/* Locate the file path of desktop file by desktop_id */
char* mime_type_locate_desktop_file( const char* dir, const char* desktop_id );

G_END_DECLS

#endif
