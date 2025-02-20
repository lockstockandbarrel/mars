#!/bin/bash
export FMAN_COLORS='?'
./a.exe --color abs >x.out
export FMAN_COLORS="bg='<G>',fg='<y><bo>'" 
export LINES
(
exec 2>&1
./a.exe --color abs
)|tee fman.log
FMAN_COLORS=\
'BG="<E>",FG="<w>",PRG="<c>",HEAD="<y></bo>",HEAD_="</bo>",FIXED="<w>",OUTPUT="<y>",OUTPUT_="</bo>",/'
FMAN_COLORS=\
'BG="<W>",FG="<e>",PRG="<b>",HEAD="<Y></bo>",HEAD_="</bo>",FIXED="<g><bo>",OUTPUT="</bo><g>",OUTPUT_="</bo>",/'
./a.exe --color abs
exit
