// Based on code in vfs_app_desktop.c of PCManFM.

#include <glib.h>
#include "mime-type.h"

const char desktop_entry_name[] = "Desktop Entry";

typedef struct _VFSAppDesktop VFSAppDesktop;

struct _VFSAppDesktop
{
    char* disp_name;
    char* comment;
    char* exec;
    char* icon_name;
    gboolean terminal;
    gboolean hidden;
};

/*
* If file_name is not a full path, this function searches default paths
* for the desktop file.
*/
VFSAppDesktop mime_get_desktop_entry( const char* file_name )
{
    GKeyFile* file;
    gboolean load;
    char* relative_path;
    char* ret = NULL;
    VFSAppDesktop app;

    file = g_key_file_new();

    if( g_path_is_absolute( file_name ) )
    {
        load = g_key_file_load_from_file( file, file_name,
                                          G_KEY_FILE_NONE, NULL );
    }
    else
    {
        relative_path = g_build_filename( "applications",
                                          file_name, NULL );
        load = g_key_file_load_from_data_dirs( file, relative_path, NULL,
                                               G_KEY_FILE_NONE, NULL );
        g_free( relative_path );
    }

    if( load )
    {
        app.disp_name = g_key_file_get_locale_string ( file,
                                                       desktop_entry_name,
                                                       "Name", NULL, NULL);

        app.comment = g_key_file_get_locale_string ( file, desktop_entry_name,
                                                     "Comment", NULL, NULL);
        app.exec = g_key_file_get_string ( file, desktop_entry_name,
                                           "Exec", NULL);
        app.icon_name = g_key_file_get_string ( file, desktop_entry_name,
                                                "Icon", NULL);
        app.terminal = g_key_file_get_boolean( file, desktop_entry_name,
                                               "Terminal", NULL );
        app.hidden = g_key_file_get_boolean( file, desktop_entry_name,
                                             "NoDisplay", NULL );
    }

    g_key_file_free( file );

    return app;
}

/*
* Parse Exec command line of app desktop file, and translate
* it into a real command which can be passed to g_spawn_command_line_async().
* file_list is a null-terminated file list containing full
* paths of the files passed to app.
* returned char* should be freed when no longer needed.
*/
char* translate_app_exec_to_command_line( const VFSAppDesktop* app,
                                          GList* file_list )
{
    char* file;
    GList* l;
    gchar *tmp;
    const char * pexec;
    GString* cmd = g_string_new("");
    gboolean add_files = FALSE;

    for( pexec = app->exec; *pexec; ++pexec )
    {
        if( *pexec == '%' )
        {
            ++pexec;
            switch( *pexec )
            {
            case 'U':
                for( l = file_list; l; l = l->next )
                {
                    tmp = g_filename_to_uri( (char*)l->data, NULL, NULL );
                    file = g_shell_quote( tmp );
                    g_free( tmp );
                    g_string_append( cmd, file );
                    g_string_append_c( cmd, ' ' );
                    g_free( file );
                }
                add_files = TRUE;
                break;
            case 'u':
                if( file_list && file_list->data )
                {
                    file = (char*)file_list->data;
                    tmp = g_filename_to_uri( file, NULL, NULL );
                    file = g_shell_quote( tmp );
                    g_free( tmp );
                    g_string_append( cmd, file );
                    g_free( file );
                    add_files = TRUE;
                }
                break;
            case 'F':
            case 'N':
                for( l = file_list; l; l = l->next )
                {
                    file = (char*)l->data;
                    tmp = g_shell_quote( file );
                    g_string_append( cmd, tmp );
                    g_string_append_c( cmd, ' ' );
                    g_free( tmp );
                }
                add_files = TRUE;
                break;
            case 'f':
            case 'n':
                if( file_list && file_list->data )
                {
                    file = (char*)file_list->data;
                    tmp = g_shell_quote( file );
                    g_string_append( cmd, tmp );
                    g_free( tmp );
                    add_files = TRUE;
                }
                break;
            case 'D':
                for( l = file_list; l; l = l->next )
                {
                    tmp = g_path_get_dirname( (char*)l->data );
                    file = g_shell_quote( tmp );
                    g_free( tmp );
                    g_string_append( cmd, file );
                    g_string_append_c( cmd, ' ' );
                    g_free( file );
                }
                add_files = TRUE;
                break;
            case 'd':
                if( file_list && file_list->data )
                {
                    tmp = g_path_get_dirname( (char*)file_list->data );
                    file = g_shell_quote( tmp );
                    g_free( tmp );
                    g_string_append( cmd, file );
                    g_free( tmp );
                    add_files = TRUE;
                }
                break;
            case 'c':
                g_string_append( cmd, app->disp_name );
                break;
            case 'i':
                /* Add icon name */
                if( app->icon_name )
                {
                    g_string_append( cmd, "--icon " );
                    g_string_append( cmd, app->icon_name );
                }
                break;
            case 'k':
                /* Location of the desktop file */
                break;
            case 'v':
                /* Device name */
                break;
            case '%':
                g_string_append_c ( cmd, '%' );
                break;
            case '\0':
                goto _finish;
                break;
            }
        }
        else  /* not % escaped part */
        {
            g_string_append_c ( cmd, *pexec );
        }
    }
_finish:
    if( ! add_files )
    {
        g_string_append_c ( cmd, ' ' );
        for( l = file_list; l; l = l->next )
        {
            file = (char*)l->data;
            tmp = g_shell_quote( file );
            g_string_append( cmd, tmp );
            g_string_append_c( cmd, ' ' );
            g_free( tmp );
        }
    }

    return g_string_free( cmd, FALSE );
}
