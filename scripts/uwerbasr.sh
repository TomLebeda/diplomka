#!/bin/bash

# parametr pro skript (jméno modelu):
# uwebasr.sh generic/cs/E77685B6-D4F2-4030-9C54-399ED62BBF32
# uwebasr.sh generic/cs/no-lm/E77685B6-D4F2-4030-9C54-399ED62BBF32

set -o nounset
set -o errexit
set -o pipefail

LANG=${1:?Please, pass LANG as \$1}

URL="https://uwebasr.zcu.cz/api/v2/lindat/${LANG}"
CONV_URL="https://uwebasr.zcu.cz/utils/v2/convert-speechcloud-json"

shift
x=${1:?Please, specify one or more input files}

for INPUT_FILE in "$@"; do
	JSON_FILE=${INPUT_FILE%.*}.json
	TXT_FILE=${INPUT_FILE%.*}.txt
	STXT_FILE=${INPUT_FILE%.*}.s.txt
	VTT_FILE=${INPUT_FILE%.*}.vtt
	SVTT_FILE=${INPUT_FILE%.*}.s.vtt

	echo "=== Recognizing to raw JSON: $JSON_FILE"
	ffmpeg -hide_banner -loglevel error -i "$INPUT_FILE" -ar 16000 -ac 1 -q:a 1 -f mp3 - |
		curl --http1.1 --data-binary @- "${URL}?format=speechcloud_json" >"$JSON_FILE"
	echo "=== Converting to plaintext: $TXT_FILE"
	curl --data-binary "@${JSON_FILE}" "${CONV_URL}?format=plaintext" >"$TXT_FILE"
	echo "=== Converting to sentext: $STXT_FILE"
	curl --data-binary "@${JSON_FILE}" "${CONV_URL}?format=plaintext&sp=0.3&pau=2.0" >"$STXT_FILE"
	echo "=== Converting to WebVTT: $VTT_FILE"
	curl --data-binary "@${JSON_FILE}" "${CONV_URL}?format=webvtt" >"$VTT_FILE"
	echo "=== Converting to SentVTT: $SVTT_FILE"
	curl --data-binary "@${JSON_FILE}" "${CONV_URL}?format=sentvtt&sp=0.3&pau=2.0" >"$SVTT_FILE"
done
