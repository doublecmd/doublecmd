#!/bin/bash

echo $OSTYPE

echo "$SSH_PRIVATE_KEY" > ssh_key

echo "cd /home/project-web/doublecmd/htdocs/snapshots" > upload_snapshot.txt
echo "lcd doublecmd-release" >> upload_snapshot.txt

if [[ "$OSTYPE" == "msys" ]]; then

  icacls.exe ssh_key //inheritance:r

  echo "rm *.7z" >> upload_snapshot.txt
  echo "put *.7z" >> upload_snapshot.txt
  echo "put *.txt" >> upload_snapshot.txt

else

  chmod 0600 ssh_key

  echo "rm *.dmg" >> upload_snapshot.txt
  echo "put *.dmg" >> upload_snapshot.txt
  echo "put *.php" >> upload_snapshot.txt

fi

echo "quit" >> upload_snapshot.txt

sftp -o StrictHostKeyChecking=no -i ssh_key -b upload_snapshot.txt $REMOTE_USER@$REMOTE_HOST

rm -f ssh_key upload_snapshot.txt
