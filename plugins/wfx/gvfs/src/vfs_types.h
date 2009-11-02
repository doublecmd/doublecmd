/*  Tux Commander VFS: Virtual File System types and definitions
 *   - prototypes functions and types
 *  draft version 3
 *
 *  Copyright (C) 2003 Radek Cervinka <radek.cervinka@centrum.cz>
 *  Copyright (C) 2008 Tomas Bzatek <tbzatek@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef __VFS_TYPES_H__
#define __VFS_TYPES_H__


#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>

typedef int TVFSResult;

/*  Compatible with gio/GAskPasswordFlags  */
typedef enum {
  VFS_ASK_PASSWORD_NEED_PASSWORD       = 1<<0,
  VFS_ASK_PASSWORD_NEED_USERNAME       = 1<<1,
  VFS_ASK_PASSWORD_NEED_DOMAIN         = 1<<2,
  VFS_ASK_PASSWORD_SAVING_SUPPORTED    = 1<<3,
  VFS_ASK_PASSWORD_ANONYMOUS_SUPPORTED = 1<<4,
  VFS_ASK_PASSWORD_SAVE_INTERNAL       = 1<<14,
  VFS_ASK_PASSWORD_ARCHIVE_MODE        = 1<<15
} TVFSAskPasswordFlags;

/*  Compatible with gio/GPasswordSave  */
typedef enum {
  VFS_PASSWORD_SAVE_NEVER,
  VFS_PASSWORD_SAVE_FOR_SESSION,
  VFS_PASSWORD_SAVE_PERMANENTLY
} TVFSPasswordSave;

/*  Return index of the choice selected or negative number when dialog has been cancelled  */
typedef void (* TVFSAskQuestionCallback)
                (const char *message,
                 const char **choices,
                 int        *choice,
                 int         cancel_choice,
                 void       *user_data);

//  Error codes (TVFSResult)
enum {
  cVFS_OK = 0,
  cVFS_Failed = 1,   //  also No such file
  cVFS_Cancelled = 2,
  cVFS_Not_Supported = 3,
  cVFS_No_More_Files = 4,
  cVFS_ReadErr = 5,
  cVFS_WriteErr = 6,  //  also Readonly FileSystem
  cVFS_LoginFailed = 7,
  cVFS_PermissionDenied = 8,
  cVFS_NoSpaceLeft = 9,
  cVFS_mallocFailed = 10,
  cVFS_BadPassword = 11,
  cVFS_MissingVolume = 12,
  cVFS_CorruptedArchive = 13
};

#endif /* __VFS_TYPES_H__ */
