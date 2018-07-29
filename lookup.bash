#!/bin/bash
set -u

create_query ()
{
    LATITUDE="${1}"
    LONGITUDE="${2}"
    QUERY="lat=${LATITUDE}&lon=${LONGITUDE}&accept-language=en"
}

send_request ()
{
    URI="https://nominatim.openstreetmap.org/reverse?format=json&${QUERY}"
    curl "${URI}"
}

main ()
{
    create_query ${@}
    send_request
}

main "${@}"
