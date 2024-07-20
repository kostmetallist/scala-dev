# !/bin/bash

dot -Tps dotfile.dot -o out.ps
evince out.ps &
