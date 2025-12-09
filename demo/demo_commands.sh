#!/bin/bash

# commands to run in the demo, for easy copy/paste
dune exec -- satie ../demo/demo.rules
dune exec -- satie ../demo/demo.rules -smt2 > demo.smt
dune exec -- satie ../demo/demo.rules -synth demo.mid
