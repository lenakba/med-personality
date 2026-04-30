* Encoding: UTF-8.
% correlations

CORRELATIONS
  /VARIABLES=big5.cold big5.helpful big5.rude big5.considerate
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=big5.thorough big5.unorganized big5.planner big5.uncareful
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=big5.chatty big5.quiet big5.outgoing big5.shy
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=big5.original big5.wild_imagination big5.speculate big5.not_artistic
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=big5.depressed big5.worried_a_lot big5.relaxed_rev big5.nervous
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.

% Reliability

RELIABILITY
  /VARIABLES=big5.helpful big5.rude big5.considerate big5.cold
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

RELIABILITY
  /VARIABLES=big5.thorough big5.unorganized big5.planner big5.uncareful
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

RELIABILITY
  /VARIABLES=big5.chatty big5.quiet big5.outgoing big5.shy
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

RELIABILITY
  /VARIABLES=big5.original big5.wild_imagination big5.speculate big5.not_artistic
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

RELIABILITY
  /VARIABLES=big5.depressed big5.worried_a_lot big5.relaxed_rev big5.nervous
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

% EFA

FACTOR
  /VARIABLES  big5.chatty big5.cold big5.thorough big5.depressed big5.original big5.quiet big5.helpful 
    big5.unorganized big5.relaxed big5.wild_imagination big5.outgoing big5.rude big5.planner 
    big5.worried_a_lot big5.speculate big5.shy big5.considerate big5.uncareful big5.nervous 
    big5.not_artistic
  /MISSING LISTWISE 
  /ANALYSIS  big5.chatty big5.cold big5.thorough big5.depressed big5.original big5.quiet big5.helpful 
    big5.unorganized big5.relaxed big5.wild_imagination big5.outgoing big5.rude big5.planner 
    big5.worried_a_lot big5.speculate big5.shy big5.considerate big5.uncareful big5.nervous 
    big5.not_artistic
  /PRINT INITIAL EXTRACTION ROTATION FSCORE
  /PRINT INITIAL KMO EXTRACTION
  /FORMAT SORT
  /PLOT ROTATION
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA KAISER  ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /SAVE REG(ALL)
  /METHOD=CORRELATION.







