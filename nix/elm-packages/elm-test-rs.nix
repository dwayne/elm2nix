{ elm-test-rs }:

#
# elm-test-rs requires 0.19.1 or 0.19.2 to run the checks.
#
# Why? The registry.dat file it uses is in the 0.19.1 format.
#
# The simplest solution for now is to just disable those checks.
#
elm-test-rs.overrideAttrs { doCheck = false; }
