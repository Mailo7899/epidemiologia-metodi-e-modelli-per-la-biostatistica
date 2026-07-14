/* Mock dataset matching the schema the analysis reads:
   low race smoke ptl ht ui age lwt  (Hosmer-Lemeshow low-birth-weight variables).
   The upstream progetto.sas reads an external dataset `pp`; this sample
   reproduces its column shape and types so the author's PROC steps run unmodified. */
data pp;
  input low age lwt race smoke ptl ht ui;
  datalines;
0 19 182 2 0 0 0 1
0 33 155 3 0 0 0 0
0 20 105 1 1 0 0 0
0 21 108 1 1 0 0 1
0 18 107 1 1 0 0 1
0 21 124 3 0 0 0 0
0 22 118 1 0 0 0 0
0 17 103 3 0 0 0 0
0 29 123 1 1 0 0 0
0 26 113 1 1 0 0 0
0 19 95 3 0 0 0 0
0 19 150 3 0 0 0 0
0 22 95 3 0 0 1 0
0 30 107 3 0 1 0 1
0 18 100 1 1 0 0 0
0 18 100 1 1 0 0 0
0 15 98 2 0 0 0 0
0 25 118 1 1 0 0 0
0 20 120 3 0 0 0 1
0 28 120 1 1 0 0 0
1 23 130 2 0 0 0 0
1 17 120 3 1 0 0 0
1 26 154 3 0 1 1 0
1 20 105 3 0 0 0 0
1 24 132 3 0 0 1 0
1 21 165 1 1 0 1 0
1 22 130 1 1 0 0 0
1 25 92 1 1 0 0 0
1 16 130 3 0 0 0 0
1 25 130 3 0 0 0 1
1 30 95 1 1 0 0 0
1 27 130 2 0 0 0 1
1 26 96 3 0 0 0 0
1 33 141 1 0 0 0 0
1 24 115 1 0 0 0 0
1 19 91 1 1 2 0 1
1 34 133 1 0 0 1 0
1 23 128 3 0 0 0 0
1 24 138 1 0 0 0 0
1 21 124 3 0 0 0 0
;
run;

/* Logistic regression of low birth weight — the author's competing predictive
   models from the "modelli predittivi" section of progetto.sas. Rsquare
   requests the generalized R-square (Nagelkerke). */
proc logistic data=pp descending;
  model low=RACE HT ptl /Rsquare;
run;
proc logistic data=pp descending;
  model low=lwt RACE ptl ht /Rsquare;
run;
