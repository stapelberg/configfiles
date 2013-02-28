#!/bin/bash
printf -v ARGS "%q " "$@"
exec ssh midna notmuch ${ARGS}
