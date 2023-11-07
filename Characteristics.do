use "../uas405.dta", clear

* Collapse some categories

recode ad001 (1 2 =1 "1.at least some") (3=0 "0.nothing"), gen(know) label(know)

recode ad010a (4 5 = 4 "4.disagree"), gen(benefit) label(benefit)

recode ad010b (4 5 =4 "4.disagree"), gen(brkthr) label(brkthr)

recode ad010c (4 5 =4 "4.disagree"), gen(costly) label(costly)

recode ad010d (4 5 =4 "4.disagree"), gen(expsv) label(expsv)

recode ad010e (4 5 =4 "4.disagree"), gen(sdeff) label(sdeff)

recode ad010f (4 5 =4 "4.disagree"), gen(scrn) label(scrn)

* characteristics by response category
tab gender know, col m
tab gender know, chi2
sum age, d
table know, c(mean age p25 age p50 age p75 age)
ttest age, by(know)
twoway (kdensity age if know==1) || (kdensity age if know==0), title("ad001 Age Distribution") xtitle("Age") legend(label(1 "At least some") label(2 "Nothing") )
graph save age_ad001
tab race know, col m
tab race know, chi2

tab gender ad002, col m
tab gender ad002, chi2
table ad002, c(mean age p25 age p50 age p75 age)
reg age i.ad002
ttest age if inlist(ad002, 1, 2), by(ad002)
ttest age if inlist(ad002, 1, 3), by(ad002)
ttest age if inlist(ad002, 1, 4), by(ad002)
ttest age if inlist(ad002, 3, 2), by(ad002)
ttest age if inlist(ad002, 3, 4), by(ad002)
ttest age if inlist(ad002, 4, 2), by(ad002)
twoway (kdensity age if ad002==1) || (kdensity age if ad002==2) || (kdensity age if ad002==3) || (kdensity age if ad002==4), title("ad002 Age Distribution") xtitle("Age") legend(label(1 "Very concerned") label(2 "Somewhat concerned") label(3 "Just a little concerned") label(4 "Not at all concerned") )
graph save age_ad002
tab race ad002, col m
tab race ad002, chi2

tab gender ad003, col m
tab gender ad003, chi2
table ad003, c(mean age p25 age p50 age p75 age)
reg age i.ad003
twoway (kdensity age if ad003==1) || (kdensity age if ad003==2) || (kdensity age if ad003==3) , title("ad003 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad003
tab race ad003, col m
tab race ad003, chi2

tab gender ad004, col m
tab gender ad004, chi2
table ad004, c(mean age p25 age p50 age p75 age)
reg age i.ad004
twoway (kdensity age if ad004==1) || (kdensity age if ad004==2) || (kdensity age if ad004==3) , title("ad004 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad004
tab race ad004, col m
tab race ad004, chi2

tab gender ad005, col m
tab gender ad005, chi2
table ad005, c(mean age p25 age p50 age p75 age)
reg age i.ad005
twoway (kdensity age if ad005==1) || (kdensity age if ad005==2) || (kdensity age if ad005==3) , title("ad005 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad005
tab race ad005, col m
tab race ad005, chi2

tab gender ad006, col m
tab gender ad006, chi2
table ad006, c(mean age p25 age p50 age p75 age)
reg age i.ad006
twoway (kdensity age if ad006==1) || (kdensity age if ad006==2) || (kdensity age if ad006==3) , title("ad006 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad006
tab race ad006, col m
tab race ad006, chi2

tab gender ad007, col m
tab gender ad007, chi2
table ad007, c(mean age p25 age p50 age p75 age)
reg age i.ad007
twoway (kdensity age if ad007==1) || (kdensity age if ad007==2) || (kdensity age if ad007==3) , title("ad007 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad007
tab race ad007, col m
tab race ad007, chi2

tab gender ad008, col m
tab gender ad008, chi2
table ad008, c(mean age p25 age p50 age p75 age)
reg age i.ad008
twoway (kdensity age if ad008==1) || (kdensity age if ad008==2) || (kdensity age if ad008==3) , title("ad008 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad008
tab race ad008, col m
tab race ad008, chi2

tab gender ad009, col m
tab gender ad009, chi2
table ad009, c(mean age p25 age p50 age p75 age)
reg age i.ad009
twoway (kdensity age if ad009==1) || (kdensity age if ad009==2) || (kdensity age if ad009==3) , title("ad009 Age Distribution") xtitle("Age") legend(label(1 "True") label(2 "False") label(3 "Don't Know") )
graph save age_ad009
tab race ad009, col m
tab race ad009, chi2

tab gender benefit, col m
tab gender benefit, chi2
table benefit, c(mean age p25 age p50 age p75 age)
reg age i.benefit
twoway (kdensity age if benefit==1) || (kdensity age if benefit==2) || (kdensity age if benefit==3) || (kdensity age if benefit==4), title("ad010a Age Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010a
tab race benefit, col m
tab race benefit, chi2

tab gender brkthr, col m
tab gender brkthr, chi2
table brkthr, c(mean age p25 age p50 age p75 age)
reg age i.brkthr
twoway (kdensity age if brkthr==1) || (kdensity age if brkthr==2) || (kdensity age if brkthr==3) || (kdensity age if brkthr==4), title("ad010b Age Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010b
tab race brkthr, col m
tab race brkthr, chi2

tab gender costly, col m
tab gender costly, chi2
table costly, c(mean age p25 age p50 age p75 age)
reg age i.costly
twoway (kdensity age if costly==1) || (kdensity age if costly==2) || (kdensity age if costly==3) || (kdensity age if costly==4), title("ad010c Age Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010c
tab race costly, col m
tab race costly, chi2

tab gender expsv, col m
tab gender expsv, chi2
table expsv, c(mean age p25 age p50 age p75 age)
reg age i.expsv
twoway (kdensity age if expsv==1) || (kdensity age if expsv==2) || (kdensity age if expsv==3) || (kdensity age if expsv==4), title("ad010d Age Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010d
tab race expsv, col m
tab race expsv, chi2

tab gender sdeff, col m
tab gender sdeff, chi2
table sdeff, c(mean age p25 age p50 age p75 age)
reg age i.sdeff
twoway (kdensity age if sdeff==1) || (kdensity age if sdeff==2) || (kdensity age if sdeff==3) || (kdensity age if sdeff==4), title("ad010e Age Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010e
tab race sdeff, col m
tab race sdeff, chi2

tab gender scrn, col m
tab gender scrn, chi2
table scrn, c(mean age p25 age p50 age p75 age)
reg age i.scrn
twoway (kdensity age if scrn==1) || (kdensity age if scrn==2) || (kdensity age if scrn==3) || (kdensity age if scrn==4), title("ad010fAge Distribution") xtitle("Age") legend(label(1 "Strongly agree") label(2 "Somewhat agree") label(3 "Neutral") label(4 "Disagree") )
graph save age_ad010f
tab race scrn, col m
tab race scrn, chi2

tab gender ad011, col m
tab gender ad011, chi2
table ad011, c(mean age p25 age p50 age p75 age)
reg age i.ad011
twoway (kdensity age if ad011==1) || (kdensity age if ad011==2) || (kdensity age if ad011==3), title("ad011 Age Distribution") xtitle("Age") legend(label(1 "Yes") label(2 "No") label(3 "Uncertain") )
graph save age_ad011
tab race ad011, col m
tab race ad011, chi2

tab gender if inlist(ad011, 2,3)
sum age if  inlist(ad011, 2,3),d
tab race if inlist(ad011, 2,3)

tab gender ad012s2 col m
tab gender ad012s2, chi2
table ad012s2, c(mean age p25 age p50 age p75 age)
reg age i.ad012s2
tab race ad012s2, col m
tab race ad012s2, chi2

twoway (kdensity age if ad012s1==1) || (kdensity age if ad012s2==1) || (kdensity age if ad012s3==1) || (kdensity age if ad012s9==1), title("ad012 Age Distribution") xtitle("Age") legend(label(1 "Ineffective") label(2 "Side effects") label(3 "Costs") label(4 "Other") )
graph save age_ad012

** group minorities
gen nhw = 0
replace nhw =1 if race==1 & hisplatino==0

tab nhw, m
foreach var in know ad002 ad003 ad004 ad005 ad006 ad007 ad008 ad009 benefit brkthr costly expsv sdeff scrn ad011 {
	tab nhw `var', col m
	tab nhw `var', chi2
}

foreach i in 1 2 3 9 {
	tab nhw ad012s`i', col m
	tab nhw ad012s`i', chi2
}


** more recent v. 
di date("07292021","MDY")
//22490
count if start_date >=22490 
//80
di date("07222021","MDY")
//22483
count if start_date >=22483 
//208


gen recent = 1 if start_date >= 22483  & !missing(start_date)
replace recent = 0 if missing(recent)
tab recent

foreach var in know ad002 ad003 ad004 ad005 ad006 ad007 ad008 ad009 benefit brkthr costly expsv sdeff scrn ad011 {
	tab recent `var', col m
	tab recent `var', chi2
}
foreach i in 1 2 3 9 {
	tab recent ad012s`i', col m
	tab recent ad012s`i', chi2
}


**  collapse a little and not at all into one category in ad002

recode ad002 (3 4 =3 "3. A little/no") , gen(concern) label(concern)
tab ad002 concern, m
foreach var in gender nhw recent {
	tab `var' concern, col m
	tab `var' concern, chi2
}


**** Model correct responses
gen noanswerall = (missing(ad003) & missing(ad004) & missing(ad005) & missing(ad006) & missing(ad007) & missing(ad008) & missing(ad009))
tab noanswerall, m

gen dkall = (ad003==3 & ad004==3 & ad005==3 & ad006==3 & ad007==3 & ad008==3 & ad009==3)
tab dkall, m

gen corr003 = (ad003==2)
tab corr003, m

gen corr004 = (ad004==2)
tab corr004, m

gen corr005 = (ad005==1)
tab corr005, m

gen corr006 = (ad006==1)
tab corr006, m

gen corr007 = (ad007==1)
tab corr007, m

gen corr008 = (ad008==1)
tab corr008, m

gen corr009 = (ad009==2)
tab corr009, m

gen corrsum = corr003 + corr004 + corr005 + corr006 + corr007 + corr008 + corr009
sum corrsum if noanswerall != 1, d
histogram corrsum if noanswerall!=1, frequency addlabels

sum corrsum if noanswerall != 1 & dkall !=1, d
histogram corrsum if noanswerall != 1 & dkall !=1, frequency addlabels

gen corrcat =.
replace corrcat = 0 if corrsum ==0 & dkall != 1 & noanswerall!=1
replace corrcat = 1 if inrange(corrsum, 1,3)
replace corrcat = 2 if inrange(corrsum, 4,7)
replace corrcat = 3 if dkall == 1
tab corrcat, m 

cd "/Users/chenyi/Desktop/Coursework/Research/Price/Spring 2021 RA/UAS/Reg"

* OLS: sample excl. dk to all
reg corrsum gender nhw age if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(ols) ci alpha(0.001, 0.01, 0.05) br replace

gen agesq = age*age
reg corrsum gender nhw age agesq if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(ols_agesq) ci alpha(0.001, 0.01, 0.05) br append

* LPM: all answered
gen somecorr = .
replace somecorr = 1 if corrsum >=1 & !missing(corrsum) & noanswerall!=1
replace somecorr = 0 if (corrsum ==0 | dkall==1) & noanswerall!=1
tab somecorr, m
tab somecorr dkall, m

reg somecorr gender nhw age if noanswerall!=1
outreg2 using "results.xls", cttop(lpm) ci alpha(0.001, 0.01, 0.05) br append
reg somecorr gender nhw age agesq if noanswerall!=1
outreg2 using "results.xls", cttop(lpm_agesq) ci alpha(0.001, 0.01, 0.05) br append

* add knowledge and concern covariates
recode ad001 (1 2 =1 "1.at least some") (3=0 "0.nothing"), gen(know) label(know)
recode ad002 (1 2 = 1 "1. very/somewhat concered") (3 4 =0 "0.a little or not concerned") , gen(concern) label(concern)

reg corrsum gender nhw age agesq know if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(ols_know) ci alpha(0.001, 0.01, 0.05) br append

reg corrsum gender nhw age agesq know concern if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(ols_concern) ci alpha(0.001, 0.01, 0.05) br append

reg somecorr gender nhw age agesq know if noanswerall!=1
outreg2 using "results.xls", cttop(lpm_know) ci alpha(0.001, 0.01, 0.05) br append

reg somecorr gender nhw age agesq know concern if noanswerall!=1
outreg2 using "results.xls", cttop(lpm_concern) ci alpha(0.001, 0.01, 0.05) br append

** LPM Y = 4+ correct response
gen corr4more = .
replace corr4more = 1 if corrsum >=4 & !missing(corrsum) & noanswerall!=1
replace corr4more = 0 if (corrsum <4 | dkall==1) & noanswerall!=1
tab corr4more, m
tab corr4more dkall, m

//excl. dk to all N=521
reg corr4more gender nhw age if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_pt_base) ci alpha(0.001, 0.01, 0.05) br append

gen agesq = age*age
reg corr4more gender nhw age agesq if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_pt_agesq) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw age agesq know if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_pt_know) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw age agesq know concern if dkall != 1 & noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_pt_concern) ci alpha(0.001, 0.01, 0.05) br append

//incl. dk to all N=1034
reg corr4more gender nhw age if noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_fl_base) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw age agesq if noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_fl_agesq) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw age agesq know if noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_fl_know) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw age agesq know concern if noanswerall!=1
outreg2 using "results.xls", cttop(lpm4_fl_concern) ci alpha(0.001, 0.01, 0.05) br append


** Model chance of seeking screening
recode ad010f (1 2 = 1 "1. agree") (3 4 5 = 0 "0. Not agree"), gen(scrnagr) label(scrnagr)
tab ad010f scrnagr, m

reg scrnagr gender nhw age agesq if !missing(scrnagr)
outreg2 using "results_scrn.xls", cttop(demo) ci alpha(0.001, 0.01, 0.05) br replace

reg scrnagr gender nhw age agesq somecorr if !missing(scrnagr)
outreg2 using "results_scrn.xls", cttop(somecorr) ci alpha(0.001, 0.01, 0.05) br append

reg scrnagr gender nhw age agesq somecorr know if !missing(scrnagr)
outreg2 using "results_scrn.xls", cttop(know) ci alpha(0.001, 0.01, 0.05) br append

reg scrnagr gender nhw age agesq somecorr know concern if !missing(scrnagr)
outreg2 using "results_scrn.xls", cttop(concern) ci alpha(0.001, 0.01, 0.05) br append


***** Use age categories in all reg
recode age (54/64.999 = 1 "1.<65" ) (65/74.999 =2 "2.65-74") (75/97 = 3 "3.>=75"), gen(agegrp) label(agegrp)
replace agegrp = 99 if missing(agegrp)

** LPM - any correct
reg somecorr gender nhw i.agegrp if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm) ci alpha(0.001, 0.01, 0.05) br replace 

reg somecorr gender nhw i.agegrp know if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm_know) ci alpha(0.001, 0.01, 0.05) br append

reg somecorr gender nhw i.agegrp know concern if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm_concern) ci alpha(0.001, 0.01, 0.05) br append

** LPM - at least 4 correct
* excl. dk to all
reg corr4more gender nhw i.agegrp if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_pt_base) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw i.agegrp know if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_pt_know) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw i.agegrp know concern if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_pt_concern) ci alpha(0.001, 0.01, 0.05) br append

* incl. dk to all 
reg corr4more gender nhw i.agegrp if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_fl_base) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw i.agegrp know if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_fl_know) ci alpha(0.001, 0.01, 0.05) br append

reg corr4more gender nhw i.agegrp know concern if noanswerall!=1
outreg2 using "agegrp.xls", cttop(lpm4_fl_concern) ci alpha(0.001, 0.01, 0.05) br append

** Check if results hold using logit
logit somecorr gender nhw i.agegrp if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit) ci alpha(0.001, 0.01, 0.05) br append eform

logit somecorr gender nhw i.agegrp know if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit_know) ci alpha(0.001, 0.01, 0.05) br append eform

logit somecorr gender nhw i.agegrp know concern if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit_concern) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_pt_base) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp know if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_pt_know) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp know concern if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_pt_concern) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_fl_base) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp know if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_fl_know) ci alpha(0.001, 0.01, 0.05) br append eform

logit corr4more gender nhw i.agegrp know concern if noanswerall!=1
outreg2 using "agegrp.xls", cttop(logit4_fl_concern) ci alpha(0.001, 0.01, 0.05) br append eform

***** Change OLS to Poisson
poisson corrsum gender nhw i.agegrp if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(poisson) ci alpha(0.001, 0.01, 0.05) br append eform

poisson corrsum gender nhw i.agegrp know if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(poisson_know) ci alpha(0.001, 0.01, 0.05) br append eform

poisson corrsum gender nhw i.agegrp know concern if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(poisson) ci alpha(0.001, 0.01, 0.05) br append eform

** why inconsistent with MJ's?
table agegrp, c(min corrsum max corrsum mean corrsum sd corrsum)
table agegrp, c(p10 corrsum p25 corrsum p50 corrsum p75 corrsum p90 corrsum)

 ttest corrsum if agegrp==1 | agegrp==2, by(agegrp) // p=0.0575 two tails
 ttest corrsum if agegrp==1 | agegrp==3, by(agegrp) // p=0.5098
 ttest corrsum if agegrp==2 | agegrp==3, by(agegrp) //p=0.4243
 
histogram corrsum if agegrp==1 & dkall != 1 & noanswerall!=1, frequency addlabels xtitle("# of Correct Response (Under 65)") discrete
histogram corrsum if agegrp==2 & dkall != 1 & noanswerall!=1, frequency addlabels xtitle("# of Correct Response (65-74)") discrete
histogram corrsum if agegrp==3 & dkall != 1 & noanswerall!=1, frequency addlabels xtitle("# of Correct Response (75+)") discrete

tab agegrp if dkall != 1 & noanswerall!=1, m

***** Add characteristics by agegrp 
recode ad001 (1 2 =1 "1.at least some") (3=0 "0.nothing"), gen(know2) label(know2)

* characteristics by response category
tab agegrp know2, chi2
tab agegrp ad002, chi2
tab agegrp ad003, chi2
tab agegrp ad004, chi2
tab agegrp ad005, chi2
tab agegrp ad006, chi2
tab agegrp ad007, chi2
tab agegrp ad008, chi2
tab agegrp ad009, chi2
foreach var in benefit brkthr costly expsv sdeff scrn ad011 {
	tab agegrp `var', chi2
}
foreach i in 1 2 3 9 {
	tab agegrp ad012s`i', chi2
}


***** Tables/Figures
recode education (2 3 4 5 6 7 8 = 1 "1.lt hs") (9 10 11 12 =2 "2.HS/some college") (13 14 15 16 = 3 "3.BA+"), gen(educ3) label(educ3)
tab educ3 education, m

recode age (54/59.999 = 1 "1.<60") (60/64.999 = 2 "2.60-64") (65/70 =3 "3.65-70") (71/75 = 4 "4.71-75") (76/80 = 5 "5.76-80") (81/97=6 "6.81+"), gen(agegrp5) label(agegrp5)
replace agegrp5=99 if missing(agegrp5)

cd "/Users/chenyi/Desktop/Coursework/Research/Price/Fall 2021 RA/UAS/Reg"

poisson corrsum i.gender i.nhw i.agegrp5 i.educ3 if dkall != 1 & noanswerall!=1
outreg2 using "agegrp.xls", cttop(poisson) ci alpha(0.001, 0.01, 0.05) br eform replace 

recode ad010f (1 2 = 1 "1. agree") (3 4 5 = 0 "0. Not agree"), gen(scrnagr) label(scrnagr)

logit scrnagr i.gender i.nhw i.agegrp5 i.educ3
outreg2 using "agegrp.xls", cttop(logit) ci alpha(0.001, 0.01, 0.05) br eform append 


** change T/F to 6, instead of 7
gen noanswerall6 = (missing(ad003) & missing(ad004) & missing(ad005) & missing(ad006) & missing(ad007) & missing(ad009)) //never see all 6 questions
tab noanswerall6, m

gen dkall6 = (ad003==3 & ad004==3 & ad005==3 & ad006==3 & ad007==3 & ad009==3)
tab dkall6, m

//gen dkall = (ad003==3 & ad004==3 & ad005==3 & ad006==3 & ad007==3 & ad008==3 & ad009==3)

gen corrsum6 = corr003 + corr004 + corr005 + corr006 + corr007 + corr009
sum corrsum6 if noanswerall6 != 1, d
sum corrsum6 if noanswerall6 != 1 & dkall6 !=1, d

gen corrcat6 =.
replace corrcat6 = 0 if corrsum6 ==0 & dkall6 != 1 & noanswerall6!=1
replace corrcat6 = 1 if inrange(corrsum6, 1,3)
replace corrcat6 = 2 if inrange(corrsum6, 4,6)
replace corrcat6 = 3 if dkall6 == 1
tab corrcat6, m 

poisson corrsum6 i.gender i.nhw i.agegrp5 i.educ3 if dkall6 != 1 & noanswerall6!=1
outreg2 using "agegrp.xls", cttop(poisson6) ci alpha(0.001, 0.01, 0.05) br eform append 


** add chance of receiving Aduhelm if have AD
recode ad011 (1 = 1 "1. receive") (2 3 = 0 "0. Not receive"), gen(rcv) label(rcv)
logit rcv i.gender i.nhw i.agegrp5 i.educ3
outreg2 using "agegrp.xls", cttop(rcv) ci alpha(0.001, 0.01, 0.05) br eform append 


logit scrnagr i.gender i.nhw i.agegrp5 i.educ3
coefplot, eform drop(_cons) xscale(log) xline(1)

