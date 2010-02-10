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
