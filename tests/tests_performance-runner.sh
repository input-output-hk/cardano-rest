#!/bin/bash

HOST=http://localhost:8100/api/
REPORT_DIR=./target/gatling

rm -fr ${REPORT_DIR}

mvn gatling:test -Dhost=${HOST}

for dir in "${REPORT_DIR}"/*/
do
    mv "${dir}" "$(echo "${dir}" | cut -d- -f1)"
done
