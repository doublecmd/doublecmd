/*
 *      mime-type.c
 *
 *      Copyright 2007 Houng Jen Yee (PCMan) <pcman.tw@gmail.com>
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

/* Currently this library is NOT MT-safe */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "mime-type.h"
#include "mime-cache.h"

#include <string.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "glib-mem.h"

/*
 * FIXME:
 * Currently, mmap cannot be used because of the limitation of mmap.
 * When a file is mapped for mime-type sniffing (checking file magic),
 * they could be deleted during the check and hence result in Bus error.
 * (Refer to the man page of mmap for detail)
 * So here I undef HAVE_MMAP to disable the implementation using mmap.
 */
#undef HAVE_MMAP

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif

/* max extent used to checking text files */
#define TEXT_MAX_EXTENT 512

const char xdg_mime_type_unknown[] = "application/octet-stream";
const char xdg_mime_type_directory[] = "inode/directory";
const char xdg_mime_type_executable[] = "application/x-executable";
const char xdg_mime_type_plain_text[] = "text/plain";

static MimeCache** caches = NULL;
static guint n_caches = 0;
guint32 mime_cache_max_extent = 0;

/* allocated buffer used for mime magic checking to
     prevent frequent memory allocation */
static char* mime_magic_buf = NULL;
/* for MT safety, the buffer should be locked */
G_LOCK_DEFINE_STATIC(mime_magic_buf);

/* load all mime.cache files on the system,
 * including /usr/share/mime/mime.cache,
 * /usr/local/share/mime/mime.cache,
 * and $HOME/.local/share/mime/mime.cache. */
static void mime_cache_load_all();

/* free all mime.cache files on the system */
static void mime_cache_free_all();

static gboolean mime_type_is_data_plain_text( const char* data, int len );

/*
 * Get mime-type of the specified file (quick, but less accurate):
 * Mime-type of the file is determined by cheking the filename only.
 * If statbuf != NULL, it will be used to determine if the file is a directory.
*/
const char* mime_type_get_by_filename( const char* filename, struct stat* statbuf )
{
    const char* type = NULL, *suffix_pos = NULL, *prev_suffix_pos = (const char*)-1;
    int i;
    MimeCache* cache;

    if( G_UNLIKELY( statbuf && S_ISDIR( statbuf->st_mode ) ) )
        return XDG_MIME_TYPE_DIRECTORY;

    for( i = 0; ! type && i < n_caches; ++i )
    {
        cache = caches[i];
        type = mime_cache_lookup_literal( cache, filename );
        if( G_LIKELY( ! type ) )
        {
            const char* _type = mime_cache_lookup_suffix( cache, filename, &suffix_pos );
            if( _type && suffix_pos < prev_suffix_pos )
            {
                type = _type;
                prev_suffix_pos = suffix_pos;
            }
        }
    }

    if( G_UNLIKELY( ! type ) )  /* glob matching */
    {
        int max_glob_len = 0, glob_len = 0;
        for( i = 0; ! type && i < n_caches; ++i )
        {
            cache = caches[i];
            const char* matched_type;
            matched_type = mime_cache_lookup_glob( cache, filename, &glob_len );
            /* according to the mime.cache 1.0 spec, we should use the longest glob matched. */
            if( matched_type && glob_len > max_glob_len )
            {
                type = matched_type;
                max_glob_len = glob_len;
            }
        }
    }


    return type && *type ? type : XDG_MIME_TYPE_UNKNOWN;
}

/*
 * Get mime-type info of the specified file (slow, but more accurate):
 * To determine the mime-type of the file, mime_type_get_by_filename() is
 * tried first.  If the mime-type couldn't be determined, the content of
 * the file will be checked, which is much more time-consuming.
 * If statbuf is not NULL, it will be used to determine if the file is a directory,
 * or if the file is an executable file; otherwise, the function will call stat()
 * to gather this info itself. So if you already have stat info of the file,
 * pass it to the function to prevent checking the file stat again.
 * If you have basename of the file, pass it to the function can improve the
 * efifciency, too. Otherwise, the function will try to get the basename of
 * the specified file again.
*/
const char* mime_type_get_by_file( const char* filepath, struct stat* statbuf, const char* basename )
{
    const char* type;
    struct stat _statbuf;

    if( statbuf == NULL || G_UNLIKELY( S_ISLNK(statbuf->st_mode) ) )
    {
        statbuf = &_statbuf;
        if( stat ( filepath, statbuf ) == -1 )
            return XDG_MIME_TYPE_UNKNOWN;
    }

    if( S_ISDIR( statbuf->st_mode ) )
        return XDG_MIME_TYPE_DIRECTORY;

    if( basename == NULL )
    {
        basename = g_utf8_strrchr( filepath, -1, '/' );
        if( G_LIKELY( basename ) )
            ++basename;
        else
            basename = filepath;
    }

    if( G_LIKELY(basename) )
    {
        type = mime_type_get_by_filename( basename, statbuf );
        if( G_LIKELY( strcmp( type, XDG_MIME_TYPE_UNKNOWN ) ) )
            return type;
        type = NULL;
    }

    if( G_LIKELY(statbuf->st_size > 0) )
    {
        int fd = -1;
        char* data;

        /* Open the file and map it into memory */
        fd = open ( filepath, O_RDONLY, 0 );
        if ( fd != -1 )
        {
            int len = mime_cache_max_extent < statbuf->st_size ?  mime_cache_max_extent : statbuf->st_size;
#ifdef HAVE_MMAP
            data = (char*) mmap( NULL, len, PROT_READ, MAP_SHARED, fd, 0 );
#else
            /*
             * FIXME: Can g_alloca() be used here? It's very fast, but is it safe?
             * Actually, we can allocate a block of memory with the size of mime_cache_max_extent,
             * then we don't need to  do dynamic allocation/free every time, but multi-threading
             * will be a nightmare, so...
             */
            /* try to lock the common buffer */
            if( G_LIKELY( G_TRYLOCK( mime_magic_buf ) ) )
                data = mime_magic_buf;
            else /* the buffer is in use, allocate new one */
                data = g_malloc( len );

            len = read( fd, data, len );

            if( G_UNLIKELY( len == -1 ) )
            {
                if( G_LIKELY( data == mime_magic_buf ) )
                    G_UNLOCK( mime_magic_buf );
                else
                    g_free( data );
                data = (void*)-1;
            }
#endif
            if( data != (void*)-1 )
            {
                int i;
                for( i = 0; ! type && i < n_caches; ++i )
                    type = mime_cache_lookup_magic( caches[i], data, len );

                /* Check for executable file */
                if( ! type && g_file_test( filepath, G_FILE_TEST_IS_EXECUTABLE ) )
                    type = XDG_MIME_TYPE_EXECUTABLE;

                /* fallback: check for plain text */
                if( ! type )
                {
                    if( mime_type_is_data_plain_text( data, len > TEXT_MAX_EXTENT ? TEXT_MAX_EXTENT : len ) )
                        type = XDG_MIME_TYPE_PLAIN_TEXT;
                }

#ifdef HAVE_MMAP
                munmap ( (char*)data, len );
#else
                if( G_LIKELY( data == mime_magic_buf ) )
                    G_UNLOCK( mime_magic_buf );  /* unlock the common buffer */
                else /* we use our own buffer */
                    g_free( data );
#endif
            }
            close( fd );
        }
    }
    else
    {
        /* empty file can be viewed as text file */
        type = XDG_MIME_TYPE_PLAIN_TEXT;
    }
    return type && *type ? type : XDG_MIME_TYPE_UNKNOWN;
}

/* Get the name of mime-type */
/*const char* mime_type_get_type( MimeInfo* info )
{
    return info->type_name;
}
*/

static char* parse_xml_desc( const char* buf, size_t len, const char* locale )
{
    const char *buf_end = buf + len;
    const char *comment = NULL, *comment_end, *eng_comment;
    size_t eng_comment_len = 0, comment_len = 0;
    char target[64];
    static const char end_comment_tag[]="</comment>";

    eng_comment = g_strstr_len( buf, len, "<comment>" );    /* default English comment */
    if( G_UNLIKELY( ! eng_comment ) ) /* This xml file is invalid */
        return NULL;
    len -= 9;
    eng_comment += 9;
    comment_end = g_strstr_len( eng_comment, len, end_comment_tag ); /* find </comment> */
    if( G_UNLIKELY( ! comment_end ) )
        return NULL;
    eng_comment_len = comment_end - eng_comment;

    if( G_LIKELY( locale ) )
    {
        int target_len = g_snprintf( target, 64, "<comment xml:lang=\"%s\">", locale);
        buf = comment_end + 10;
        len = (buf_end - buf);
        if( G_LIKELY( ( comment = g_strstr_len( buf, len, target ) ) ) )
        {
            len -= target_len;
            comment += target_len;
            comment_end = g_strstr_len( comment, len, end_comment_tag );    /* find </comment> */
            if( G_LIKELY( comment_end ) )
                comment_len = (comment_end - comment);
            else
                comment = NULL;
        }
    }
    if( G_LIKELY( comment ) )
        return g_strndup( comment, comment_len );
    return g_strndup( eng_comment, eng_comment_len );
}

static char* _mime_type_get_desc( const char* type, const char* data_dir, const char* locale )
{
    int fd;
    struct stat statbuf;
    char *buffer, *_locale, *desc;
    char file_path[ 256 ];

    /* FIXME: This path shouldn't be hard-coded. */
    g_snprintf( file_path, 256, "%s/mime/%s.xml", data_dir, type );

    fd = open ( file_path, O_RDONLY, 0 );
    if ( G_UNLIKELY( fd == -1 ) )
        return NULL;
    if( G_UNLIKELY( fstat ( fd, &statbuf ) == -1 ) )
    {
        close( fd );
        return NULL;
    }
#ifdef HAVE_MMAP
    buffer = (char*)mmap( NULL, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0 );
#else
    buffer = (char*)g_malloc( statbuf.st_size );
    if( read( fd, buffer, statbuf.st_size ) == -1 )
    {
        g_free( buffer );
        buffer = (char*)-1;
    }
#endif
    close( fd );
    if ( G_UNLIKELY( buffer == (void*)-1 ) )
        return NULL;

    _locale = NULL;
    if( ! locale )
    {
        const char* const * langs = g_get_language_names();
        char* dot = strchr( langs[0], '.' );
        if( dot )
            locale = _locale = g_strndup( langs[0], (size_t)(dot - langs[0]) );
        else
            locale = langs[0];
    }
    desc = parse_xml_desc( buffer, statbuf.st_size, locale );
    g_free( _locale );

#ifdef HAVE_MMAP
    munmap ( buffer, statbuf.st_size );
#else
    g_free( buffer );
#endif
    return desc;
}

/*
 * Get human-readable description of the mime-type
 * If locale is NULL, current locale will be used.
 * The returned string should be freed when no longer used.
*/
char* mime_type_get_desc( const char* type, const char* locale )
{
    char* desc;
    const gchar* const * dir;

    dir = g_get_system_data_dirs();
    for( ; *dir; ++dir )
    {
        desc = _mime_type_get_desc( type, *dir, locale );
        if( G_LIKELY(desc) )
            return desc;
    }

    /*
     * FIXME: According to specs on freedesktop.org, user_data_dir has
     * higher priority than system_data_dirs, but in most cases, there was
     * no file, or very few files in user_data_dir, so checking it first will
     * result in many unnecessary open() system calls, yealding bad performance.
     * Since the spec really sucks, we don't follow it here.
     */
    desc = _mime_type_get_desc( type, g_get_user_data_dir(), locale );
    return desc;
}

void mime_type_finalize()
{
/*
    if( G_LIKELY( table ) )
    {
        g_hash_table_destroy( table );
        table = NULL;
    }
*/
    mime_cache_free_all();
}

#if 0
void test_parents(const char* type)
{
    int i;
    const char** parents = NULL;
    const char** p;

    for( i = 0; i < n_caches; ++i )
    {
        parents = mime_cache_lookup_parents( caches[i], type );
        if( parents )
            break;
    }
    if( parents )
        for( p = parents; *p; ++p )
        {
            g_debug( "%s is parent of %s", *p, type );
        }
    else
        g_debug( "no parent found" );
}

void test_alias( const char* type )
{
    int i;
    const char* alias = NULL;
    for( i = 0; i < n_caches; ++i )
    {
        alias = mime_cache_lookup_alias( caches[i], type );
        if( alias )
            break;
    }
    g_debug("test:\nalias of %s is %s", type, alias );

}
#endif

void mime_type_init()
{
    mime_cache_load_all();
//    table = g_hash_table_new_full( g_str_hash, g_str_equal, g_free, (GDestroyNotify)mime_type_unref );
}

/* load all mime.cache files on the system,
 * including /usr/share/mime/mime.cache,
 * /usr/local/share/mime/mime.cache,
 * and $HOME/.local/share/mime/mime.cache. */
void mime_cache_load_all()
{
    const char* const * dirs;
    int i;
    const char filename[] = "/mime/mime.cache";
    char* path;

    dirs = g_get_system_data_dirs();
    n_caches = g_strv_length( (char**)dirs ) + 1;
    caches = (MimeCache**)g_slice_alloc( n_caches * sizeof(MimeCache*) );

    path = g_build_filename( g_get_user_data_dir(), filename, NULL );
    caches[0] = mime_cache_new( path );
    g_free( path );
    if( caches[0]->magic_max_extent > mime_cache_max_extent )
        mime_cache_max_extent = caches[0]->magic_max_extent;

    for( i = 1; i < n_caches; ++i )
    {
        path = g_build_filename( dirs[i - 1], filename, NULL );
        caches[ i ] = mime_cache_new( path );
        g_free( path );
        if( caches[i]->magic_max_extent > mime_cache_max_extent )
            mime_cache_max_extent = caches[i]->magic_max_extent;
    }
    mime_magic_buf = g_malloc( mime_cache_max_extent );
    return ;
}

/* free all mime.cache files on the system */
void mime_cache_free_all()
{
    mime_cache_foreach( (GFunc)mime_cache_free, NULL );
    g_slice_free1( n_caches * sizeof(MimeCache*), caches );
    n_caches = 0;
    caches = NULL;
    mime_cache_max_extent = 0;

    g_free( mime_magic_buf );
    mime_magic_buf = NULL;
}

/* Iterate through all mime caches */
void mime_cache_foreach( GFunc func, gpointer user_data )
{
    int i;
    for( i = 0; i < n_caches; ++i )
        func( caches[i], user_data );
}

gboolean mime_cache_reload( MimeCache* cache )
{
    int i;
    gboolean ret = mime_cache_load( cache, cache->file_path );
    /* recalculate max magic extent */
    for( i = 0; i < n_caches; ++i )
    {
        if( caches[i]->magic_max_extent > mime_cache_max_extent )
            mime_cache_max_extent = caches[i]->magic_max_extent;
    }

    G_LOCK( mime_magic_buf );

    mime_magic_buf = g_realloc( mime_magic_buf, mime_cache_max_extent );

    G_UNLOCK( mime_magic_buf );

    return ret;
}

gboolean mime_type_is_data_plain_text( const char* data, int len )
{
    int i;
    if ( G_LIKELY( len >= 0 && data ) )
    {
        for ( i = 0; i < len; ++i )
        {
            if ( data[ i ] == '\0' )
                return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

gboolean mime_type_is_text_file( const char *file_path, const char* mime_type )
{
    int file;
    int rlen;
    gboolean ret = FALSE;

    if( mime_type )
    {
        if( mime_type_is_subclass( mime_type, XDG_MIME_TYPE_PLAIN_TEXT ) )
            return TRUE;
        if( ! g_str_has_prefix( mime_type, "text/" ) && ! g_str_has_prefix( mime_type, "application/" ) )
            return FALSE;
    }

    if( !file_path )
        return FALSE;

    file = open ( file_path, O_RDONLY );
    if ( file != -1 )
    {
        struct stat statbuf;
        if( fstat( file, &statbuf ) != -1 )
        {
            if( S_ISREG( statbuf.st_mode ) )
            {
#ifdef HAVE_MMAP
                char* data;
                rlen = statbuf.st_size < TEXT_MAX_EXTENT ? statbuf.st_size : TEXT_MAX_EXTENT;
                data = (char*) mmap( NULL, rlen, PROT_READ, MAP_SHARED, file, 0 );
                ret = mime_type_is_data_plain_text( data, rlen );
                munmap ( (char*)data, rlen );
#else
                unsigned char data[ TEXT_MAX_EXTENT ];
                rlen = read ( file, data, sizeof( data ) );
                ret = mime_type_is_data_plain_text( (char*) data, rlen );
#endif
            }
        }
        close ( file );
    }
    return ret;
}

gboolean mime_type_is_executable_file( const char *file_path, const char* mime_type )
{
    if ( !mime_type )
    {
        mime_type = mime_type_get_by_file( file_path, NULL, NULL );
    }

    /*
    * Only executable types can be executale.
    * Since some common types, such as application/x-shellscript,
    * are not in mime database, we have to add them ourselves.
    */
    if ( mime_type != XDG_MIME_TYPE_UNKNOWN &&
            (mime_type_is_subclass( mime_type, XDG_MIME_TYPE_EXECUTABLE ) ||
            mime_type_is_subclass( mime_type, "application/x-shellscript" ) ) )
    {
        if ( file_path )
        {
            if ( ! g_file_test( file_path, G_FILE_TEST_IS_EXECUTABLE ) )
                return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

/* Check if the specified mime_type is the subclass of the specified parent type */
gboolean mime_type_is_subclass( const char* type, const char* parent )
{
    int i;
    const char** parents = NULL;
    const char** p;

    /* special case, the type specified is identical to the parent type. */
    if( G_UNLIKELY( 0 == strcmp(type, parent) ) )
        return TRUE;

    for( i = 0; i < n_caches; ++i )
    {
        parents = mime_cache_lookup_parents( caches[i], type );
        if( parents )
        {
            for( p = parents; *p; ++p )
            {
                if( 0 == strcmp( parent, *p ) )
                    return TRUE;
            }
        }
    }
    return FALSE;
}

/*
 * Get all parent type of this mime_type
 * The returned string array should be freed with g_strfreev().
 */
char** mime_type_get_parents( const char* type )
{
    int i;
    const char** parents = NULL;
    const char** p;
    GArray* ret = g_array_sized_new( TRUE, TRUE, sizeof(char*), 5 );

    for( i = 0; i < n_caches; ++i )
    {
        parents = mime_cache_lookup_parents( caches[i], type );
        if( parents )
        {
            for( p = parents; *p; ++p )
            {
                char* parent = g_strdup( *p );
                g_array_append_val( ret, parent );
            }
        }
    }
    return (char**)g_array_free( ret, (0 == ret->len) );
}

/*
 * Get all alias types of this mime_type
 * The returned string array should be freed with g_strfreev().
 */
char** mime_type_get_alias( const char* type )
{
    int i;
    const char** alias = NULL;
    const char** p;
    GArray* ret = g_array_sized_new( TRUE, TRUE, sizeof(char*), 5 );

    for( i = 0; i < n_caches; ++i )
    {
        alias = (const char **) mime_cache_lookup_alias( caches[i], type );
        if( alias )
        {
            for( p = alias; *p; ++p )
            {
                char* type = g_strdup( *p );
                g_array_append_val( ret, type );
            }
        }
    }
    return (char**)g_array_free( ret, (0 == ret->len) );
}

/*
 * Get mime caches
 */
MimeCache** mime_type_get_caches( int* n )
{
    *n = n_caches;
    return caches;
}
