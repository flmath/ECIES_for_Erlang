#!/bin/bash
cd $(dirname "$0")/..

rm -rf isolated
echo "removed isolated"
rm -rf _build
echo "removed build"
rm -rf ebin/*
echo "removed ebin"
rm -rf rebar.lock
echo "removed rebar.lock"
rm -rf logs/*
rm -rf ctlogs/*
echo "removed test logs"
rm -rf test/*beam
rm -rf src/*beam
echo "removed beam files"
rm -rf test/*~
rm -rf src/*~
echo "removed tmp files"
rm -rf erl_crash.dump
rm -rf rebar3.crashdump
echo "removed crash dump files"
rm -rf certs/*
echo "removed certs"

