#!/bin/bash

echo "$SSH_PRIVATE_KEY" > ssh_key && chmod 0600 ssh_key
sftp -o StrictHostKeyChecking=no -i ssh_key $REMOTE_USER@$REMOTE_HOST <<END
cd /home/project-web/doublecmd/htdocs/snapshots
lcd /var/tmp/doublecmd-release
rm *.dmg
put *.dmg
put *.php
quit
END
