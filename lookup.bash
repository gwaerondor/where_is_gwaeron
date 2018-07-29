#!/bin/bash
set -u

create_query ()
{
    LATITUDE="${1}"
    LONGITUDE="${2}"
    QUERY="lat=${LATITUDE}&lon=${LONGITUDE}&accept-language=en&zoom=18"
}

send_request ()
{
    URI="https://nominatim.openstreetmap.org/reverse?format=json&${QUERY}"
    RESPONSE=$(curl -s "${URI}")
}

print_response()
{
    echo "${RESPONSE}"
}

main ()
{
    create_query ${@}
    send_request
    print_response
}

main "${@}"
