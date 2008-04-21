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

function grep_dpatch { perl -n -e 'print if (/^New patches:/.../^--=_/)'; }
function grep_gpg {
	perl -n -e 'print if (/^-----BEGIN PGP SIGNED MESSAGE-----/.../^-----END PGP SIGNATURE-----/)';
	}
function md5 { md5sum - |cut -c-32 ; }
function update {
	echo "Updateing darcswatch web view"
	$DARCSWATCH $CONFIG new ;
	}

FILE=$(tempfile --prefix patch)
mimedecode > "$FILE"

echo "Looking for a patch"

if fgrep -q 'Content-Type: text/x-darcs-patch;' "$FILE" ||
   fgrep -q 'Content-Type: text/x-patch;' "$FILE"
then

	echo "Patch ok, adding to patch directory"

	if fgrep -q -- '-----BEGIN PGP SIGNED MESSAGE-----' "$FILE"
	then

		MD5SUM=$(grep_gpg < "$FILE" | md5)
		grep_gpg < "$FILE" > "$DIR/patch_$MD5SUM"

	else
		MD5SUM=$(grep_dpatch < "$FILE" | md5)
		grep_dpatch < "$FILE" > "$DIR/patch_$MD5SUM"
	fi

	update
	rm "$FILE"
	exit 0


else
	echo "No patch contained, it seems"
	rm "$FILE"
fi
