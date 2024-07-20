# !/bin/bash
# Translates generated dot-file into ps-file and 
# then just opens it.

dot -Tps sample.dot -o out.ps
evince out.ps &
