#!/bin/sh

raco pkg remove vm
cd $(dirname $0)
raco pkg install
