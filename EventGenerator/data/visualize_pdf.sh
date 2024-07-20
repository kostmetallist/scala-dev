# !/bin/bash
 
dot -Tpdf dotfile.dot -o out.pdf
evince out.pdf &
