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

FILE=$(tempfile --prefix patch)
mimedecode > "$FILE"

if fgrep -q '^List-Post: <mailto:darcs-devel@darcs.net>' "$FILE" ||
   fgrep -q '^List-Post: <mailto:xmonad@haskell.org>' "$FILE"
then
	echo "Looking for a patch"
	
	if fgrep -q 'Content-Type: text/x-darcs-patch;' "$FILE"
	then

		echo "Looking for gpg frame"
		if fgrep -q -- '-----BEGIN PGP SIGNED MESSAGE-----' "$FILE"
		then

			echo "Patch ok, adding to patch directory"
			MD5SUM=$(grep_gpg < "$FILE" | md5)
			grep_gpg < "$FILE" > "$DIR/patch_$MD5SUM"
			echo "Updateing darcswatch web view"
			$DARCSWATCH $CONFIG 

			rm "$FILE"
			exit 0

		else
			echo "Patch ok, adding to patch directory"
			MD5SUM=$(perl -n -e 'print if (/^New patches:/.../^--=_/)' < "$FILE" |md5sum - |cut -c-32)
			perl -n -e 'print if (/^New patches:/.../^--=_/)' < "$FILE" > "$DIR/patch_$MD5SUM"
			echo "Updateing darcswatch web view"
			$DARCSWATCH $CONFIG 

			rm "$FILE"
			exit 0

		fi

	else
		echo "No patch contained, it seems"
	fi
else
	echo "Verifying patch"
	if gpg --no-default-keyring --no-options --keyring "$KEYRING" --trust-model always --verify "$FILE" 
	then
		echo "Patch ok, adding to patch directory"
		MD5SUM=$(grep_gpg < "$FILE" | md5)
		grep_gpg < "$FILE" > "$DIR/patch_$MD5SUM"
		echo "Updateing darcswatch web view"
		$DARCSWATCH $CONFIG 

		rm "$FILE"
		exit 0
	else
		echo "Verification failed:" 
		echo "Did you sign your patches?" 
		rm "$FILE"
		exit 1
	fi
fi
