#!/bin/bash

# Example usage in a alias file:
# darcswatch: |/opt/darcswatch/mailfilter.sh /opt/darcswatch/real/mails/ /opt/darcswatch/real/keyring /opt/darcswatch/darcswatch /opt/darcswatch/real/
# 
# Note that $HOME has to be set, otherwise gpg will complain.

DIR=$1
KEYRING=$2
DARCSWATCH=$3
CONFIG=$4


test -n "$DIR" || { echo "Missing directory parameter"; exit 1; }
test -d "$DIR" || { echo "Not a directory: $DIRMissing"; exit 1; }
test -n "$KEYRING" || { echo "Missing keyring parameter"; exit 1; }
test -n "$DARCSWATCH" || { echo "Missing darcswatch parameter"; exit 1; }
test -n "$CONFIG" || { echo "Missing config parameter"; exit 1; }


FILE=$(tempfile --prefix patch)
mimedecode > "$FILE"

echo "Verifying patch"
if gpg --no-default-keyring --no-options --keyring "$KEYRING" --trust-model always --verify "$FILE" 
then
	echo "Patch ok, adding to patch directory"
	MD5SUM=$(perl -n -e 'print if (/^-----BEGIN PGP SIGNED MESSAGE-----/.../^-----END PGP SIGNATURE-----/)' < "$FILE" |md5sum - |cut -c-32)
	perl -n -e 'print if (/^-----BEGIN PGP SIGNED MESSAGE-----/.../^-----END PGP SIGNATURE-----/)' < "$FILE" > "$DIR/patch_$MD5SUM"
	echo "Updateing darcswatch web view"
	$DARCSWATCH $CONFIG 
else
	echo "Verification failed:" 
	echo "Did you sign your patches?" 
	rm "$FILE"
	exit 1
fi
