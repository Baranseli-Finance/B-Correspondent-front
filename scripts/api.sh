#!/bin/sh

url=$1
file=$2
api=$3

oops() {
    echo "$0:" "$@" >&2
    exit 1
}

abort()
{
    echo >&2 '
***************
*** ABORTED ***
***************
'
    echo "An error occurred. Exiting..." >&2
    exit 1
}

trap 'abort' 0

set -e

echo >&2 '
************
*** START *** 
************
'

# if [[ -z "${API_LOGIN_ENV}" ]]; then
#   oops "no API_LOGIN_ENV is set. cannot proceed"
# else
#   login="${API_LOGIN_ENV}"
# fi

# if [[ -z "${API_PASS_ENV}" ]]; then
#   oops "no API_PASS_ENV is set. cannot proceed"
# else
#   pass="${API_PASS_ENV}"
# fi

credentials="$login:$pass"

generate() { 
#   node api-downloader.mjs $url $file $credentials
  openapi-generator-cli \
  generate -i $file -g javascript -o src/core/BCorrespondent/Api/Foreign/$api \
  --additional-properties=usePromises=true,emitModelMethods=true
   find src/ -type f -name "*.js" -exec js-beautify -r {} \;
}

generate

trap : 0

echo >&2 '
************
*** DONE *** 
************
'