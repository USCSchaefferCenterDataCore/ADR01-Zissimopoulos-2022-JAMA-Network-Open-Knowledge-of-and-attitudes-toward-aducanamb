******************************************************
* Recode response and socio-demographic variables 
* Describe knowledge of & attitude to aducanumab
* Model 1) increasing chance of screening
*       2) receiving aducanumab if have AD
*       3) correct answers to T/F questions
******************************************************


use "../uas405.dta", clear


****** Recode Response Variables

recode ad001 (1 2 =1 "1.At least some") (3=0 "0.Nothing"), gen(know) label(know)

recode ad002 (1 =1 "1.Very concerned") (2 =2 "2.Somewhat concerned")  (3 4 =3 "0. A little/not concerned") , gen(concern) label(concern)

recode ad010a (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(benefit) label(benefit)

recode ad010b (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(brkthr) label(brkthr)

recode ad010c (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(costly) label(costly)

recode ad010d (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(expsv) label(expsv)

recode ad010e (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(sdeff) label(sdeff)

recode ad010f (1 2 = 1 "1.Agree") (3 =2 "2.Neither agree nor disagree") (4 5 = 3 "4.Disagree"), gen(scrn) label(scrn)
recode ad010f (1 2 = 1 "1. Agree") (3 4 5 = 0 "0. Not agree"), gen(scrnagr) label(scrnagr)

recode ad011 (1 = 1 "1. Receive") (2 3 = 0 "0. Not receive"), gen(rcv) label(rcv)


****** Recode Socio-demographic Variables 
gen nhw = 0
replace nhw =1 if race==1 & hisplatino==0
tab nhw race, m
tab nhw hisplatino, m

recode education (2 3 4 5 6 7 8 = 1 "1.lt hs") (9 10 11 12 =2 "2.HS/some college") (13 14 15 16 = 3 "3.BA+"), gen(educ3) label(educ3)
tab educ3 education, m

recode age (54/59.999 = 1 "1.<60") (60/64.999 = 2 "2.60-64") (65/70 =3 "3.65-70") (71/75 = 4 "4.71-75") (76/80 = 5 "5.76-80") (81/97=6 "6.81+"), gen(agegrp5) label(agegrp5)
replace agegrp5=99 if missing(agegrp5)
tab agegrp5, m


****** Describe knowledge & attitude
* How concerned about AD
tab concern, m

* Self-assessed knowledge
tab know, m

* # of correct answers to 6 T/F questions
gen noanswerall6 = (missing(ad003) & missing(ad004) & missing(ad005) & missing(ad006) & missing(ad007) & missing(ad009))
tab noanswerall6, m

gen dkall6 = (ad003==3 & ad004==3 & ad005==3 & ad006==3 & ad007==3 & ad009==3)
tab dkall6, m

gen corrsum6 = corr003 + corr004 + corr005 + corr006 + corr007 + corr009
sum corrsum6 if noanswerall6 != 1, d
sum corrsum6 if noanswerall6 != 1 & dkall6 !=1, d

* Benefits society
tab benefit, m

* Breakthrough
tab brkthr, m

* Costly to Medicare
tab costly, m 

* Expensive for patients
tab expsv, m


cd "../Results"

****** Model increasing chance of seeking screening
logit scrnagr i.gender i.nhw i.agegrp5 i.educ3
outreg2 using "results.xls", cttop(scrn) ci alpha(0.001, 0.01, 0.05) br eform replace 
coefplot, eform drop(_cons) xscale(log) xline(1)


****** Model chance of receiving Aduhelm if have AD
logit rcv i.gender i.nhw i.agegrp5 i.educ3
outreg2 using "results.xls", cttop(rcv) ci alpha(0.001, 0.01, 0.05) br eform append 
coefplot, eform drop(_cons) xscale(log) xline(1)


****** Model correct answers to 6 T/F questions
** # of correct answers
* OLS
reg corrsum6 i.gender i.nhw i.agegrp5 i.educ3 
outreg2 using "results.xls", cttop(ols) ci alpha(0.001, 0.01, 0.05) br append

* Poisson
poisson corrsum6 i.gender i.nhw i.agegrp5 i.educ3 
outreg2 using "results.xls", cttop(poisson) ci alpha(0.001, 0.01, 0.05) br append

** Any correct anwers
gen somecorr6 = .
replace somecorr6 = 1 if corrsum6 >= 1 & !missing(corrsum6) & noanswerall6 != 1
replace somecorr6 = 0 if (corrsum6 == 0 | dkall6 == 1) & noanswerall6 != 1

* LPM
reg somecorr6 i.gender i.nhw i.agegrp5 i.educ3 
outreg2 using "results.xls", cttop(lpm) ci alpha(0.001, 0.01, 0.05) br append

* Logit
logit somecorr6 i.gender i.nhw i.agegrp5 i.educ3 
outreg2 using "results.xls", cttop(logit) ci alpha(0.001, 0.01, 0.05) br append





