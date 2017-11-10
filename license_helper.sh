#!/usr/bin/env bash

set -e -u

if ! git diff --exit-code >/dev/null
then
	echo "This script will modify your files, therefore I will refuse \
to run until you stage your changes"
	exit 1
fi

# If I need to generalise this
REACH="Reach us at https:\\/\\/github.com\\/ongy\\/waymonad"
PROJECT="waymonad A wayland compositor in the spirit of xmonad"
YEAR=`date "+%Y"`
NAME=`git config user.name`

FILES=`find src -type f`

for file in ${FILES}
do
	TFILE="${file}.license_tmp"
	# For now we skip everything that contains a license header already.
	# Maybe I'll change this later (probably not in bash)
	if grep -q "Copyright (C)" "${file}"
	then
		continue
	fi

	cp "${file}" "${TFILE}"

	sed \
		-e "s/{{YEAR}}/${YEAR}/" \
		-e "s/{{NAME}}/${NAME}/" \
		-e "s/{{PROJECT}}/${PROJECT}/" \
		-e "s/{{REACH}}/${REACH}/" \
		<"./.license_template" \
		>"${file}"

	cat "${TFILE}" >> "${file}"
	unlink "${TFILE}"
done
