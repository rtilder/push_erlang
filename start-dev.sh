#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    +P 134217727 \
    -sname push_erlang_dev \
    -s push_erlang \
    -s reloader
