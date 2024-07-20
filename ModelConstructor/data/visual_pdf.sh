# !/bin/bash

dot -Tpdf sample.dot -o out.pdf
evince out.pdf &
