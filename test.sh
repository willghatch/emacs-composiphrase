#!/usr/bin/env bash

emacs -batch -q -l composiphrase.el -l test-composiphrase.el -f ert-run-tests-batch-and-exit
