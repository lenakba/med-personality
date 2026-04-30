* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
RECODE mbti_extrovert ('E'='1') ('I'='0').
EXECUTE.

RECODE mbti_intuitive ('N'='1') ('S'='0').
EXECUTE.

RECODE mbti_feeling ('F'='1') ('T'='0').
EXECUTE.

RECODE mbti_judging ('J'='1') ('P'='0').
EXECUTE.

RECODE mbti_assertive ('T'='1') ('A'='0').
EXECUTE.

CORRELATIONS
  /VARIABLES=mbti_extrovert big5_extrovert_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=mbti_intuitive big5_openness_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=mbti_judging big5_conscientiousness_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=mbti_feeling big5_conscientiousness_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=mbti_feeling big5_agreeableness_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=mbti_assertive big5_neuroticism_tot
  /PRINT=TWOTAIL NOSIG FULL
  /CI CILEVEL(95)
  /MISSING=PAIRWISE.

MEANS TABLES=big5_extrovert_tot BY mbti_extrovert
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=big5_openness_tot BY mbti_intuitive
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=big5_conscientiousness_tot BY mbti_judging
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES= big5_agreeableness_tot BY mbti_feeling
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=big5_conscientiousness_tot BY mbti_feeling
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES= big5_neuroticism_tot BY mbti_assertive
  /CELLS=MEAN COUNT STDDEV.
