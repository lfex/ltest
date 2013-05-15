lfeunit: eunit for LFE
======================

Currently, when the Erlang eunit header file (`.hrl`) is `include-lib`ed in
LFE, only a few macros make it over. Robert Virding is looking into this, but
until the fix is ready, it would be a fun exercise to implement a subset of
`eunit`'s functionality for LFE. Thus this project ;-)
