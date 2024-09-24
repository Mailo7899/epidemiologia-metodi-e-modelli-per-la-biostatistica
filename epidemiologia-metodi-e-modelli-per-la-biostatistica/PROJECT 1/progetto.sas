/*progetto Millone Rossi */
/*var categoriale dopo variabile mettere dollaro $*/

*------------------------stat descrittive -------------------------------------
fare gli effect plot per categoria ->le medie dei pesi per le categorie dei diversi fattori 
proc freq ;
 proc freq data=pp;
   tables low race smoke ptl ht ui /chisq;
 run;
 proc freq data=pp;
   tables race*smoke race*ht race*ui   race*ptl  smoke*ptl smoke*ui  ht*smoke  ptl*ui  ht*ptl ht*ui          low*ptl low*ht  low*ui low*race  low*smoke low*race/chisq;
 run;
 proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   var  age lwt  ;
 run;
  proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class race smoke ptl ht ui;
   var low age lwt;
 run;

   proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class race ;
   var low age lwt;
 run;

   proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class smoke ;
   var low age lwt;
 run;

   proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class ptl ;
   var low age lwt;
 run;

   proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class  ht ;
   var low age lwt;
 run;

   proc means data=pp MEAN STD MIN P25 P50 P75 MAX MAXDEC=3;
   class ui;
   var low age lwt;
 run;

/*box plot entro categorie fatooriale*/
 proc sgplot data=pp;
  vbox low / category=race datalabel;
run;

ods graphics on;
proc corr data=pp plots=matrix(histogram);
/* plots=matrix(histogram): scatterplot con gli istogrammi sulla diagonale*/
var  age lwt   /*incident front back*/  ;
run;
ods graphics off;
*------------------------stat descrittive -------------------------------------;






/*-----------------------------------------------------studio assunti modello lineare----------------------------------------------------------------------- */
ods graphics on;
proc glm data=pp;
class  low race smoke ptl ht ui ;
model low=race smoke ptl ht ui age lwt ;
output out=resdat r=resid p=pred student=stand
rstudent=rstu cookd=cksd h=lev;
run;/*MI CERA DT MA NON RIESCO A VISUALIZZARLO.OMUNQUE RIESCO A FARE TEST COME QUELLO DELLA NORMALIT‡ */
ods graphics off;
/*std normalit‡ */
proc univariate data=resdat normal plot;var resid; run;/* TEST KOOMOGORV E SHAPIRO CON QQPLOT ->RISPETTIAMO LA NORMALIT‡ */
/*proc gchart data=resdat;
vbar resid / midpoints=(-0.6, -0.4, -0.1, 0, 0.1, 0.4, 0.6);
run;quit;*/
proc capability data=resdat noprint;
ppplot resid /normal square;
run;


/*metodo kernel per verificare normalita dai residui ,bwm=1 finestrra in cui luii va a interpolare dati */
proc kde data=resdat out=den bwm=1;var resid;
proc sort data=den;by resid;
proc gplot data=den;
plot density*resid;
run;/*grafico distribuzione con valori nella curva */

/*eteroschedasticita
*/

title Ågverifica presenza eteroschedasticitaÅh;
proc sort data=resdat;by pred;
proc gplot data=resdat;/*presenza di etero data la distribuzione non casuale e i residui non attorno a zero */
plot rstu*pred / vref=0;
run;quit;
/*CE PRESENZA ETEROWC HEDASTICIT‡ */


/*OUTLIER */

/*residui studentizzati per i residui predetti  e cook*/
title Ågverifica osservazioni influentiÅh;
proc freq data=resdat;tables lev;
run;quit;

/*residui studentizzati */
title Ågverifica presenza outliersÅh;
proc freq data=resdat;tables rstu;
run;quit;

goption reset=all;
proc reg data=pp lineprinter;
model low=race smoke ptl ht ui age lwt;
paint rstudent.<-3.52 or rstudent.>3.52/symbol='*';
plot rstudent.*predicted.;
quit;
run;
proc reg data=pp;
model low=race smoke ptl ht ui age lwt/influence;/*--------------------------------------------------------------------------*-/
/*id subject;*/
run;quit;

/*distanze cook */
proc freq data=resdat;tables cksd;
run;quit;


/*collinearita */
/*proc arima serve per serie storiche ma in modo naive si puo usare per questo */
proc arima data=resdat;identify var=resid;
title "Ågverifica autocorrelazione dei residuiÅh";
run;quit;

proc reg data=pp;
model low=age lwt /vif collin tol;
run;



data andrea ;set pp;
log_age=log(age);
log_lwt=log(lwt);
n+1;
run;

ods graphics on;
proc glm data=andrea;
class  low race smoke ptl ht ui ;
model low=race smoke ptl ht ui log_age log_lwt ;
output out=resdat2 r=resid2 p=pred2 student=stand2
rstudent=rstu2 cookd=cksd2 h=lev2;
run;/*MI CERA DT MA NON RIESCO A VISUALIZZARLO.OMUNQUE RIESCO A FARE TEST COME QUELLO DELLA NORMALIT‡ */
ods graphics off;

proc univariate data=resdat2 normal plot;var resid2; run;/* TEST KOOMOGORV E SHAPIRO CON QQPLOT ->RISPETTIAMO LA NORMALIT‡ */
/*proc gchart data=resdat;
vbar resid / midpoints=(-0.6, -0.4, -0.1, 0, 0.1, 0.4, 0.6);
run;quit;*/
proc capability data=resdat2 noprint;
ppplot resid2 /normal square;
run;


/*metodo kernel per verificare normalita dai residui ,bwm=1 finestrra in cui luii va a interpolare dati */
proc kde data=resdat2 out=den2 bwm=1;var resid2;
proc sort data=den2;by resid2;
proc gplot data=den2;
plot density*resid2;
run;/*grafico distribuzione con valori nella curva */

proc reg data=andrea;
model low=race smoke ptl ht ui log_age log_lwt/influence;/*--------------------------------------------------------------------------*/
id n;
run;quit;

proc reg data=andrea;
model low=log_age log_lwt /vif collin tol;
run;








*--------------------------------------------modelli predittivi -------------------------------------------
. La capacit‡ predittiva viene misura sotto tre
aspetti: i) misura dellíerrore di predizione, ii) la discriminazione
(capacit‡ del modello di classificare correttamente i pazienti tra
coloro che hanno líevento e coloro che non hanno líevento), iii)
la calibrazione (grado di corrispondenza tra la probabilit‡ stimata
dal modello e quella osservata fornisce indicazioni sulla validit‡
interna del modello).;


proc logistic data=pp descending;
class race(param=ref ref="0") smoke(param=ref ref="0") ptl(param=ref ref="0") ht(param=ref ref="0") ui(param=ref ref="0") ;
model low=age lwt race smoke ptl ht ui ;
output out=prova p=predetti;
run;


proc logistic data=andrea descending;
class race(param=ref ref="0") smoke(param=ref ref="0") ptl(param=ref ref="0") ht(param=ref ref="0") ui(param=ref ref="0") ;
model low=log_age log_lwt race smoke ptl ht ui ;
output out=prova2 p=predetti2;
run;




/*---------------------------------------------------------------------------------------------------------------------------------------------------*/
data pp2;set pp;
n+1;
run;
Proc sort data=pp2  ;
by low;
run;



Proc sort data=andrea  ;
by low;
run;

/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

/*decerscente */
/*estrarre campione casuale da dt
samprate=tasso estrazione
seed =seme 
outall= fornisce dt con variabile seleceted che indica se soggetto e nel trainng o validation 
*/
Proc surveyselect Data=pp2 Samprate=.6667 Out=samu Seed=7548 Outall;
Strata low;/*la proporzione di casi nei due camiponi deve essee uguale (di trainng e validation)*/
Run; 
/*low=age lwt race smoke ptl ht ui bwt*/
/*iniziamo a cr4eare statistica c */
Data train;set samu;if selected=1;run;
Data valid;set samu;if selected=0;run;
proc logistic data=train descending;/*modelliamo risposta uguale a 1 con decending */
model low=  RACE HT  ptl  /Rsquare; Score data=valid/*applico stesso modello njel campione valid */ out=sco_validate1 (rename=(p_1=p_11)) fitstat/*statstiche errore predeizione */; run;
proc logistic data=train descending;*altro modello ;
model low= lwt RACE  ptl ht ; Score data=valid out=sco_validate2 (rename=(p_1=p_12)) fitstat; run;
/*proc logistic data=train descending;* 3 modelli diversi ;
model low= lwt RACE  ptl ht ui ; Score data=valid out=sco_validate3 (rename=(p_1=p_13)) fitstat; run;
proc logistic data=train descending;* 3 modelli diversi ;
model low= lwt RACE smoke ptl ht ui ; Score data=valid out=sco_validate4 (rename=(p_1=p_14)) fitstat; run;
proc logistic data=train descending;* 3 modelli diversi ;*/
model low=age lwt RACE smoke ptl ht ui ; Score data=valid out=sco_validate5 (rename=(p_1=p_15)) fitstat; run;



proc sort data=sco_validate1;by n;
proc sort data=sco_validate2;by n;
p/*roc sort data=sco_validate3;by n;
proc sort data=sco_validate4;by n;*/
proc sort data=sco_validate5;by n;
run;
data bbb;merge sco_validate1 sco_validate2;by n;run;*fondo 3 dataset in un unico dataset ;
/*data ccc;merge sco_validate3 bbb;by n;run;
data DDD;merge sco_validate4 CCC;by n;run;*/
data EEE;merge sco_validate5 bbb;by n;run;
* r sclalto max errore di predizione;
* vedo risultati grafici e test con curve roc (roccontrast  e il 
test di delong della capacita discriminatoria dei tre modelli );
ods graphics on;
proc logistic data=EEE;
model low=p_11 p_12/* p_13 p_14*/ p_15/nofit;
roc "RACE HT  ptl " p_11;
roc "lwt RACE  ptl ht" p_12;
/*roc "lwt RACE  ptl ht ui " p_13;
roc "lwt RACE smoke ptl ht ui" p_14;*/
roc "age lwt RACE smoke ptl ht ui" p_15;
roccontrast "contrasti"/estimate=allpairs;run;


/*per i modelli neasted il test associazione deve essere fatto per i novi predittori 
vedere se nuovo predittori con test di delong risulta significativo */




proc logistic data=andrea descending;/*modelliamo risposta uguale a 1 con decending */
model low=log_age log_lwt smoke ptl  /Rsquare; Score data=valid/*applico stesso modello njel campione valid */ out=sco_validate11 (rename=(p_1=p_11)) fitstat/*statstiche errore predeizione */; run;
proc logistic data=andrea descending;*altro modello ;
model low=log_age log_lwt  smoke ptl ht ; Score data=valid out=sco_validate22 (rename=(p_1=p_12)) fitstat; run;
proc logistic data=andrea descending;* 3 modelli diversi ;
model low=log_age log_lwt smoke ptl ht ui ; Score data=valid out=sco_validate33 (rename=(p_1=p_13)) fitstat; run;
proc sort data=sco_validate11;by n;
proc sort data=sco_validate22;by n;
proc sort data=sco_validate33;by n;
run;
data bbbbb;merge sco_validate11 sco_validate22;by n;run;*fondo 3 dataset in un unico dataset ;
data ccccc;merge sco_validate33 bbb;by n;run;
* r sclalto max errore di predizione;
* vedo risultati grafici e test con curve roc (roccontrast  e il 
test di delong della capacita discriminatoria dei tre modelli );
ods graphics on;
proc logistic data=ccccc;
model low=p_11 p_12  p_13/nofit;
roc "black" p_11;
roc "black+married" p_12;
roc "black+married+smoke" p_13;
roccontrast "contrasti"/estimate=allpairs;run;









/*MACRO PER NRI-----------------------------------------------------------*/
%add_predictive(data=EEE,y=low,p_old=p_11,p_new=p_12, nripoints=.17 .33);
%macro hoslem (data=, pred=, y=, ngro=10,print=T,out=hl);
/*---
Macro computes the Hosmer-Lemeshow Chi Square Statistic
for Calibration
Parameters (* = required)
-------------------------
data* input dataset
pred* Predicted Probabilities
y* Outcome 0/1 variable
ngro # of groups for the calibration test (default 10)
print Prints output (set to F to Suppress)
out output dataset (default HL)
Author: Kevin Kennedy
---*/ 
%let print = %upcase(&print);
proc format;
value pval 0-.0001='<.0001';
run;
data first;set &data;where &y^=. and &pred^=.;run;
proc rank groups=&ngro out=ranks data=first;ranks phat_grp;var &pred;run;
proc sort data=ranks;by phat_grp;run;
proc sql;
create table ranks2 as select *, count(*) as num_dec label='Sample Size', sum(&pred) as sum_pred
label='Sum Probabilities', sum(&y) as sum_y label='Number of Events'
from ranks
group by phat_grp;
create table ranks3 as select distinct(phat_grp),num_dec,sum_pred,sum_y,
((sum_y-sum_pred)**2/(sum_pred*(1-sum_pred/num_dec))) as chi_part label 'Chi-Square Term'
from ranks2 ;
select sum(chi_part) into :chi_sq
from ranks3;
quit;
data &out;
chi_sq=&chi_sq; label chi_sq='Hosmer Lemeshow Chi Square';
df=&ngro-2; label df ='Degree of Freedom';
p_value=1-cdf('chisquared',chi_sq,df);label p_value= 'P-Value';
format p_value pval.;
run;
%if &print=T %then %do;
title 'Hosmer Lemeshow Details';
proc print data=ranks3 noobs label;run;
Options formdlim='-';
title 'Hosmer Lemeshow Calibration Test';
proc print data=&out noobs label;run;
%end;
Options formdlim='';
%mend;
%macro add_predictive(data=, y=, p_old=, p_new= , nripoints=%str(),hoslemgrp=10) ;
/*---
this macro attempts to quantify the added predictive ability of a covariate(s) in logistic regression based off of the statistics in:
M. J. Pencina ET AL. Statistics in Medicine (2008 27(2):157-72). Statistics returned with be: C-statistics (for 9.2 users),
IDI (INTEGRATED DISCRIMINATION IMPROVEMENT), NRI (net reclassification index) for both Category-Free and User defined
groups, and Hosmer Lemeshow GOF test with associated pvalues and z scores.
Parameters (* = required)
-------------------------
data* Specifies the SAS dataset
y* Response variable (Outcome must be 0/1)
p_old* Predicted Probability of an Event using Initial Model
p_new* Predicted Probability of an Event using New Model
nripoints Groups for User defined classification (Optional),
Example 3 groups: (<.06, .06-.2, >.2) then nripoints=.06 .2
hoslemgrp # of groups for the Hosmer Lemeshow test (default 10)
Author: Kevin Kennedy and Michael Pencina
Date: May 26, 2010
---*/
options nonotes nodate nonumber;
ods select none;
%local start end ;
%let start=%sysfunc(datetime());
proc format;
value pval 0-.0001='<.0001';
run;
/******Step 1: C-Statistics******/
/********************************/
%if %sysevalf(&sysver >= 9.2) %then %do;
%put ********Running AUC Analysis************;
proc logistic data=&data descending; model &y=&p_old &p_new; roc 'first' &p_old; roc 'second' &p_new;
roccontrast reference('first')/estimate e;
ods output ROCAssociation=rocass ROCContrastEstimate=rocdiff; run;
proc sql noprint;
select estimate, StdErr, lowercl, uppercl, (ProbChiSq*100) as pval
into :rocdiff, :rocdiff_stderr, :rocdiff_low, :rocdiff_up, :rocp
from rocdiff
where find(contrast,'second'); quit;
data _null_; set rocass;
if ROCModel='first' then do;
call symputx('c_old',Area); end;
if ROCModel='second' then do;
call symputx('c_new',Area);
end; run;
data cstat;
cstat_old=&c_old; label cstat_old='Model1 AUC';
cstat_new=&c_new; label cstat_new='Model2 AUC';
cstat_diff=&rocdiff; label cstat_diff='Difference in AUC';
cstat_stderr=&rocdiff_stderr; label cstat_stderr='Standard Error of Difference in AUC';
cstat_low=&rocdiff_low; label cstat_low='Difference in AUC Lower 95% CI';
cstat_up=&rocdiff_up; label cstat_up='Difference in ACU Upper 95% CI';
cstat_ci='('!!trim(left(cstat_low))!!','!!trim(left(cstat_up))!!')';
label cstat_ci='95% CI for Difference in AUC';
cstat_pval=&rocp/100; label cstat_pval='P-value for AUC Difference';
format cstat_pval pval.;
run;
%end;
%if %sysevalf(&sysver < 9.2) %then %do;
options notes ;
%put *********************;
%put NOTE: You are running a Pre 9.2 version of SAS;
%put NOTE: Go to SAS website to get example of ROC Macro for AUC Comps;
%put NOTE: http://support.sas.com/kb/25/017.html;
%put *********************;
%put;
options nonotes ;
%end;
/******************************/
/*****End step 1***************/
/******************************/
/******Step 2: IDI***************/
%put ********Running IDI Analysis************;
proc sql noprint;
create table idinri as select &y,&p_old, &p_new, (&p_new-&p_old) as pdiff
from &data
where &p_old^=. and &p_new^=.
order by &y;
quit;
proc sql noprint; /*define mean probabilities for old and new model and event and nonevent*/
select count(*),avg(&p_old), avg(&p_new),stderr(pdiff) into
:num_event, :p_event_old, :p_event_new,:eventstderr
from idinri
where &y=1 ;
select count(*),avg(&p_old), avg(&p_new),stderr(pdiff) into
:num_nonevent, :p_nonevent_old, :p_nonevent_new ,:noneventstderr
from idinri
where &y=0;quit;
data fin(drop=slope_noadd slope_add);
pen=&p_event_new; label pen='Mean Probability for Events: Model2';
peo=&p_event_old; label peo='Mean Probability for Events: Model1';
pnen=&p_nonevent_new; label pnen='Mean Probability for NonEvents: Model2';
pneo=&p_nonevent_old; label pneo='Mean Probability for NonEvents: Model1';
idi=(&p_event_new-&p_nonevent_new)-(&p_event_old-&p_nonevent_old);
label idi='Integrated Discrimination Improvement';
idi_stderr=sqrt((&eventstderr**2)+(&noneventstderr**2));
label idi_stderr='IDI Standard Error';
idi_lowci=round(idi-1.96*idi_stderr,.0001);
idi_upci=round(idi+1.96*idi_stderr,.0001);
idi_ci='('!!trim(left(idi_lowci))!!','!!trim(left(idi_upci))!!')';
label idi_ci='IDI 95% CI';
z_idi=abs(idi/(sqrt((&eventstderr**2)+(&noneventstderr**2))));
label z_idi='Z-value for IDI';
pvalue_idi=2*(1-PROBNORM(abs(z_idi))); label pvalue_idi='P-value for IDI';
change_event=&p_event_new-&p_event_old;
label change_event='Probability change for Events';
change_nonevent=&p_nonevent_new-&p_nonevent_old;
label change_nonevent='Probability change for Nonevents';
slope_noadd=&p_event_old-&p_nonevent_old;
slope_add=&p_event_new-&p_nonevent_new;
relative_idi=slope_add/slope_noadd-1; label relative_idi='Relative IDI';
format pvalue_idi pval.; run;
/************step 3 NRI analysis*******/
%put ********Running NRI Analysis************;
data nri_inf;
set idinri;
if &y=1 then do;
down_event=(pdiff<0);up_event=(pdiff>0);down_nonevent=0;up_nonevent=0;
end;
if &y=0 then do;
down_nonevent=(pdiff<0);up_nonevent=(pdiff>0);down_event=0;up_event=0;
end;
run;
proc sql;
select sum(up_nonevent), sum(down_nonevent), sum(up_event),sum(down_event)
into :num_nonevent_up_user, :num_nonevent_down_user, :num_event_up_user, :num_event_down_user
from nri_inf
quit;
/* Category-Free Groups */
data nri1;
group="Category-Free NRI";
p_up_event=&num_event_up_user/&num_event;
p_down_event=&num_event_down_user/&num_event;
p_up_nonevent=&num_nonevent_up_user/&num_nonevent;
p_down_nonevent=&num_nonevent_down_user/&num_nonevent; 
nri=(p_up_event-p_down_event)-(p_up_nonevent-p_down_nonevent);
nri_stderr=sqrt(((&num_event_up_user+&num_event_down_user)/&num_event**2-(&num_event_up_user-
&num_event_down_user)**2/&num_event**3)+
((&num_nonevent_down_user+&num_nonevent_up_user)/&num_nonevent**2-
(&num_nonevent_down_user-&num_nonevent_up_user)**2/&num_nonevent**3));
low_nrici=round(nri-1.96*nri_stderr,.0001);
up_nrici=round(nri+1.96*nri_stderr,.0001);
nri_ci='('!!trim(left(low_nrici))!!','!!trim(left(up_nrici))!!')';
z_nri=nri/sqrt(((p_up_event+p_down_event)/&num_event)
+((p_up_nonevent+p_down_nonevent)/&num_nonevent)) ;
pvalue_nri=2*(1-PROBNORM(abs(z_nri)));
event_correct_reclass=p_up_event-p_down_event;
nonevent_correct_reclass=p_down_nonevent-p_up_nonevent;
z_event=event_correct_reclass/sqrt((p_up_event+p_down_event)/&num_event);
pvalue_event=2*(1-probnorm(abs(z_event)));
z_nonevent=nonevent_correct_reclass/sqrt((p_up_nonevent+p_down_nonevent)/&num_nonevent);
pvalue_nonevent=2*(1-probnorm(abs(z_nonevent)));
format pvalue_nri pvalue_event pvalue_nonevent pval. event_correct_reclass
nonevent_correct_reclass percent.;
label nri='Net Reclassification Improvement'
nri_stderr='NRI Standard Error'
low_nrici='NRI lower 95% CI'
up_nrici='NRI upper 95% CI'
nri_ci='NRI 95% CI'
z_nri='Z-Value for NRI'
pvalue_nri='NRI P-Value'
pvalue_event='Event P-Value'
pvalue_nonevent='Non-Event P-Value'
event_correct_reclass='% of Events correctly reclassified'
nonevent_correct_reclass='% of Nonevents correctly reclassified';
run; 
/*User Defined NRI*/
%if &nripoints^=%str() %then %do;
/*words macro*/
%macro words(list,delim=%str( ));
%local count;
%let count=0;
%do %while(%qscan(%bquote(&list),&count+1,%str(&delim)) ne %str());
%let count=%eval(&count+1);
%end; &count
%mend words;
%let numgroups=%eval(%words(&nripoints)+1); /*figure out how many ordinal groups*/
proc format ; value group
1 = "0 to %scan(&nripoints,1,%str( ))"
%do i=2 %to %eval(&numgroups-1);
%let j=%eval(&i-1);
&i="%scan(&nripoints,&j,%str( )) to %scan(&nripoints,&i,%str( ))"
%end;
%let j=%eval(&numgroups-1);
&numgroups="%scan(&nripoints,&j,%str( )) to 1"; run;
data idinri;
set idinri;
/*define first ordinal group for pre and post*/
if 0<=&p_old<=%scan(&nripoints,1,%str( )) then group_pre=1;
if 0<=&p_new<=%scan(&nripoints,1,%str( )) then group_post=1;
%let i=1; %do %until(&i>%eval(&numgroups-1)); 
if %scan(&nripoints,&i,%str( ))<&p_old then do;
group_pre=&i+1;
end;
if %scan(&nripoints,&i,%str( ))<&p_new then do;
group_post=&i+1;
end;
%let i=%eval(&i+1);
%end;
if &y=0 then do;
up_nonevent=(group_post>group_pre);
down_nonevent=(group_post<group_pre);
down_event=0; up_event=0;
end;
if &y=1 then do;
up_event=(group_post>group_pre);
down_event=(group_post<group_pre);
down_nonevent=0; up_nonevent=0;
end;
format group_pre group_post group.;
run;
proc sql;
select sum(up_nonevent), sum(down_nonevent), sum(up_event),sum(down_event),avg(&y)
into :num_nonevent_up_user, :num_nonevent_down_user, :num_event_up_user,
:num_event_down_user, :eventrate
from idinri quit;
data nri2;
group='User Category NRI';
p_up_event=&num_event_up_user/&num_event;
p_down_event=&num_event_down_user/&num_event;
p_up_nonevent=&num_nonevent_up_user/&num_nonevent;
p_down_nonevent=&num_nonevent_down_user/&num_nonevent;
nri=(p_up_event-p_down_event)-(p_up_nonevent-p_down_nonevent);
nri_stderr=sqrt(((&num_event_up_user+&num_event_down_user)/&num_event**2-
(&num_event_up_user-&num_event_down_user)**2/&num_event**3)+
((&num_nonevent_down_user+&num_nonevent_up_user)/&num_nonevent**2-
(&num_nonevent_down_user-&num_nonevent_up_user)**2/&num_nonevent**3));
low_nrici=round(nri-1.96*nri_stderr,.0001);
up_nrici=round(nri+1.96*nri_stderr,.0001);
nri_ci='('!!trim(left(low_nrici))!!','!!trim(left(up_nrici))!!')';
z_nri=nri/sqrt(((p_up_event+p_down_event)/&num_event)
+((p_up_nonevent+p_down_nonevent)/&num_nonevent)) ;
pvalue_nri=2*(1-PROBNORM(abs(z_nri)));
event_correct_reclass=p_up_event-p_down_event;
nonevent_correct_reclass=p_down_nonevent-p_up_nonevent;
z_event=event_correct_reclass/sqrt((p_up_event+p_down_event)/&num_event);
pvalue_event=2*(1-probnorm(abs(z_event)));
z_nonevent=nonevent_correct_reclass/sqrt((p_up_nonevent+p_down_nonevent)/&num_nonevent);
pvalue_nonevent=2*(1-probnorm(abs(z_nonevent)));
format pvalue_nri pval.;run;
data nri1;set nri1 nri2;run;%end;
/**************/
/*step 4 gof */
/**************/
%hoslem(data=idinri,pred=&p_old,y=&y,ngro=&hoslemgrp,out=m1,print=F);
%hoslem(data=idinri,pred=&p_new,y=&y,ngro=&hoslemgrp,out=m2,print=F);
data hoslem(drop=cnt);
retain model;
set m1 m2; cnt+1;
if cnt=1 then model='Model1';
else model='Model2';
run;
ods select all;
/*output for cstat*/
%if %sysevalf(&sysver >= 9.2) %then %do;
proc print data=cstat label noobs;
title1 "Evaluating added predictive ability of model2";
title2 'AUC Analysis';run;
%END;
/*output for IDI*/
proc print data=fin label noobs;
title1 "Evaluating added predictive ability of model2";
title2 'IDI Analysis';
var idi idi_stderr z_idi pvalue_idi idi_ci
pen peo pnen pneo change_event change_nonevent relative_idi;
run;

/*output for NRI*/
proc print data=nri1 label noobs;
title1 "Evaluating added predictive ability of model2";
title2 'NRI Analysis';
var group nri nri_stderr z_nri pvalue_nri nri_ci event_correct_reclass pvalue_event nonevent_correct_reclass pvalue_nonevent; run;
%if &nripoints^=%str() %then %do;
proc freq data=idinri;
where &y=0;
title 'NRI Table for Non-Events';
tables group_pre*group_post/nopercent nocol; run;
proc freq data=idinri; where &y=1;
title 'NRI Table for Events';
tables group_pre*group_post/nopercent nocol; run; %end;
/*print HL gof*/
proc print data=hoslem noobs label;
title "Hosmer Lemeshow Test with %sysevalf(&hoslemgrp-2) df"; run;
proc datasets library=work nolist;
delete fin idinri nri1 nri2 nri_inf stderr; quit;
options notes;
%put NOTE: Macro %nrstr(%%)add_predictive completed.;
%let end=%sysfunc(datetime());
%let runtime=%sysfunc(round(%sysevalf(&end-&start)));
%put NOTE: Macro Real Run Time=&runtime seconds;
title;
%mend;
*-------------------------------VEDERE OUTPUT IN SLIDE 13 PAG 65-66;
* ID FA INTERGRABLE DEI CASI E INTEGREALE DEI NON CASI IN TERMINI DI PROBABILIT‡ PREDETTA CON INTEGRALE 
NO  GRANDE SVILUPPO ;

* ALLA FNE TEST HOMEN E LEMNESHOW;





*--------------------------------------------test multipli singoli o in sequenza stepwise-------------------------------------;











































































































































































































