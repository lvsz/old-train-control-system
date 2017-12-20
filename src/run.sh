#!/bin/sh

cd "${0%/*}/infrabel"
racket server.rkt &
cd ".."
racket main.rkt

