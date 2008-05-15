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

echo "Looking for a state"

STATE=none
if formail -z -x "Subject:" < "$FILE" | fgrep -q '[OBSOLETE]'
then
	STATE=obsolete
fi
if formail -z -x "Subject:" < "$FILE" | fgrep -q '[REJECTED]'
then
	STATE=rejected
fi
# More polite style, for xmonad :-)
if fgrep -q -i 'DarcsWatch: rejected' < "$FILE"
then
	STATE=rejected
fi
if fgrep -q -i 'DarcsWatch: obsolete' < "$FILE"
then
	STATE=obsolete
fi
echo "Found state $STATE"

echo "Looking for the sender"
FROM=$(formail -z -x "From:" < "$FILE")


MSGID="$(formail -z -x "Message-ID:" < "$FILE")"

echo "Looking for a patch"

if fgrep -q 'Content-Type: text/x-darcs-patch;' "$FILE" ||
   fgrep -q 'Content-Type: text/x-patch;' "$FILE" ||
   fgrep -q 'New patches:' "$FILE" 
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

	if [ -n "$MSGID" ]
	then
		if test -e "$DIR/mid-mapping" && fgrep -q "$MSGID" "$DIR/mid-mapping"
		then
			echo "Seen this mail before"
		else
			echo "$MSGID $MD5SUM" >> "$DIR/mid-mapping"
		fi
	fi

	if [ "STATE" = "none" ]
	then
		STATE=add
	fi

else
	echo "No patch contained, it seems"
fi

if [ -n "$MSGID" -a "$STATE" != "none" ]
then
	if [ -z "$MD5SUM" ]
	then
		# No referred bundle yet
		REPLY=$(formail -z -x "In-Reply-To:" < "$FILE") 	
		if [ -n "$REPLY" ] && test -e "$DIR/mid-mapping"
		then
			MD5SUM=$(fgrep "$REPLY" "$DIR/mid-mapping" | cut -f2 -d\ )
		fi
	fi

	if [ -n "$MD5SUM" ]
	then
		# Got something to mark
		if test -e "$DIR/states" && fgrep -q "$MSGID" "$DIR/states"
		then
			echo "Seen this mail before"
		else
			echo "Marking patch $MD5SUM as $STATE"
			echo "$MD5SUM $STATE $MSGID $FROM" >> "$DIR/states"
		fi
	fi
fi

if [ -n "$MD5SUM" ]
then
	update
fi

rm "$FILE"
exit 0
