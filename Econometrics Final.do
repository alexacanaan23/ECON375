*Alexa Canaan & Ryan Curran
*ECON 375
*Project Description Code

*Load the data 2009
*contains data from American Housing Survey
clear
use "\\econ\ECONDATA\Econ375Scrimgeour\studentfolders\Alexa Canaan\Final Project\2009\30941-0004-Data.dta"
destring CONTROL REGION, replace
generate year = 2009
destring HHRACE, replace ignore(B)
destring HHSEX , replace ignore(B)
destring HHNATVTY, replace ignore(B)
destring HHCITSHP, replace ignore(B)
destring HHSPAN, replace ignore(B)

*Load the data 2007
append using "\\econ\ECONDATA\Econ375Scrimgeour\studentfolders\Alexa Canaan\Final Project\2007\23563-0004-Data.dta", force
replace year = 2007 if year == . 
 
*drop blank variables
drop if VALUE ==. | VALUE == -6 | VALUE == -7 | VALUE == -8 | VALUE == -9
drop if ZINC ==. | ZINC == -6 | ZINC == -7 | ZINC == -8 | ZINC == -9
drop if ZINC2 ==. | ZINC2 == -6 | ZINC2 == -7 | ZINC2 == -8 | ZINC2 == -9
drop if UNITSF ==. | UNITSF == -6 | UNITSF == -7 | UNITSF == -8 | UNITSF == -9

*create dummy variables 
*0 is male
recode HHSEX (2 = 0), gen(FEMALE)
label variable FEMALE "Female"
summarize FEMALE

*0 is no hispanic
recode HHSPAN (2 = 0), gen(SPAN)
label variable SPAN "Spanish"
summarize SPAN

*generating Race dummies

recode HHRACE (2/21 = 0) (1 = 1), gen(WHITE)
label var WHITE "White"

recode HHRACE (1 = 0) (2 = 1) (3/21 = 0), gen(BLACK)
label var BLACK "Black"

recode HHRACE (1 2 3 = 0) (4 = 1) (5/21 = 0), gen(ASIAN)
label var ASIAN "Asian"

recode HHRACE (1/5 13 15/21 = 0) (6/12 14 = 1), gen(MD)
label var MD "Multiracial" 

recode HHRACE (1 2 4 6/12 14 = 0) (3 5 13 15/21 = 1), gen(OTHER)
label var OTHER "Other Race"

*generate citizenship variables
gen CITIZEN1 = HHCITSHP if HHCITSHP >= 1 & HHCITSHP <= 5
label var CITIZEN1 "Citizenship Dummy"
recode CITIZEN1 (1 2 3 4 = 0) (5 = 1), gen(CITIZEN)
label var CITIZEN "Citizen"

*generate edu attainment number of years of school completed
recode HHGRAD (31 = 0) (32 = 4) (33 = 6) (34 = 8) (35 = 9) (36 = 10) (37 = 11) (38 = 12) (39 = 13) (40 41 = 14) (42 43 = 15) (44 = 17) (45 = 19) (46 = 21) (47 = 25), gen(GRAD)
*year dummy
recode year (2007 = 0) (2009 = 1), gen(Year2009)
label variable Year2009 "Year 2009"
summarize Year2009
recast int Year2009

*INTRODUCTORY FIGURES-----------------------------------------------------------
drop if VALUE < 0
drop if ZINC2 < 0
*summary statistics
capture ssc install estout
estpost summarize FEMALE CITIZEN GRAD ZINC2 HHAGE, detail
esttab . using sumstat.rtf, ///
   cells("mean(fmt(%5.2f) label(Mean)) sd(label(SD)) min(label(Min)) max(label(Max))  count(fmt(%9.0f) label(N))") ///
   noobs label nonum replace mlabels(none) title(Summary Statistics)
   
capture ssc install estout
estpost summarize WHITE BLACK SPAN ASIAN MD OTHER, detail
esttab . using sumRACEobs.rtf, ///
   cells("mean(fmt(%5.2f) label(Mean)) sd(label(SD)) min(label(Min)) max(label(Max))  count(fmt(%9.0f) label(N))") ///
   noobs label nonum replace mlabels(none) title(Summary Statistics)
   
drop if VALUE > 1110000
graph box VALUE, over(year) title(Housing Value Before and After the Great Recession)



*REGRESSIONS--------------------------------------------------------------------

. ssc install estout, replace
eststo A : reg VALUE Year2009
eststo B : reg VALUE Year2009 GRAD ZINC2 HHAGE
eststo C : reg VALUE Year2009 FEMALE CITIZEN GRAD ZINC2 HHAGE
eststo D : reg VALUE Year2009 SPAN BLACK MD ASIAN OTHER FEMALE CITIZEN
eststo E : reg VALUE Year2009 SPAN BLACK MD ASIAN OTHER FEMALE CITIZEN GRAD ZINC2 HHAGE
esttab A B C D E using OLSreg.doc, se r2

*panel data set
format year %ty
xtset CONTROL year , delta(2)
xtdes

. ssc install estout, replace
eststo A : xtreg VALUE GRAD ZINC2 HHAGE, fe
eststo B : xtreg VALUE FEMALE CITIZEN GRAD ZINC2 HHAGE, fe
eststo C : xtreg VALUE SPAN BLACK MD ASIAN OTHER FEMALE CITIZEN, fe
eststo D : xtreg VALUE SPAN BLACK ASIAN MD OTHER FEMALE CITIZEN GRAD ZINC2 HHAGE, fe
esttab A B C D using FEreg1.doc, se r2

. ssc install estout, replace
eststo A : xtreg VALUE GRAD ZINC2 HHAGE, re
eststo B : xtreg VALUE FEMALE CITIZEN GRAD ZINC2 HHAGE, re
eststo C : xtreg VALUE SPAN BLACK MD ASIAN OTHER FEMALE CITIZEN, re
eststo D : xtreg VALUE SPAN BLACK ASIAN MD OTHER FEMALE CITIZEN GRAD ZINC2 HHAGE, re
esttab A B C D using REreg1.doc, se r2
   
 
*-------------------------------------------------------------------------------
*Extra code

/*

xtset CONTROL year , delta(2)

*Black vs. White
*gen D11 = (BWD == 1) * (l.BWD == 1)
*gen D12 = (BWD == 0) * (l.BWD == 0)
*gen D13 = (BWD == 1) * (l.BWD == 0)
*gen D14 = (BWD == 0) * (l.BWD == 1)
*xtreg VALUE D11 D13 D14
*xtreg VALUE D11 D13 D14, fe

*gen D1 = (BLACK == 1) * (l.BLACK == 1)
*gen D12 = (BWD == 0) * (l.BWD == 0)
*gen D3 = (BLACK == 1) * (l.BLACK == 0)
*gen D4 = (BLACK == 0) * (l.BLACK == 1)
*xtreg VALUE D1 D3 D4
*xtreg VALUE D1 D3 D4, fe

*Spanish vs. White
*gen D21 = (SPAN == 1) * (l.SPAN == 1)
*gen D12 = (BWD == 0) * (l.BWD == 0)
*gen D23 = (SPAN == 1) * (l.SPAN == 0)
*gen D24 = (SPAN == 0) * (l.SPAN == 1)
*xtreg VALUE D21 D23 D24
*xtreg VALUE D21 D23 D24, fe

*Asian vs. White
*gen D31 = (ASIAN == 1) * (l.ASIAN == 1)
*gen D12 = (BWD == 0) * (l.BWD == 0)
*gen D33 = (ASIAN == 1) * (l.ASIAN == 0)
*gen D34 = (ASIAN == 0) * (l.ASIAN == 1)
*xtreg VALUE D31 D33 D34
*xtreg VALUE D31 D33 D34, fe

*Multiracial vs. White
*gen D41 = (MD == 1) * (l.MD == 1)
*gen D12 = (BWD == 0) * (l.BWD == 0)
*gen D43 = (MD == 1) * (l.MD == 0)
*gen D44 = (MD == 0) * (l.MD == 1)
*xtreg VALUE D41 D43 D44
*xtreg VALUE D41 D43 D44, fe

*generate BW = HHRACE if HHRACE == 1 | HHRACE == 2
*label var BW "Black White Dummy"
*recode BW (1 = 0) (2 = 1), gen(BWD)
*label var BWD "Black White Dummy"
*recast int BWD

*recode HHRACE (1 2 = 0) (3 = 1) (4/21 = 0), gen(INDALAS)
*label var INDALAS "American Indian, Alaskan Native"

*generate INDALASDUMMY = HHRACE if HHRACE == 1 | HHRACE == 3
*recode INDALASDUMMY (1 = 0) (3 = 1), gen(IAD)
*label var IAD "Native American Dummy"

*generate ASIAWHITEDUMMY = HHRACE if HHRACE == 1 | HHRACE == 4
*recode ASIAWHITEDUMMY (1 = 0) (4 = 1), gen(AWD)
*label var AWD "Asian White Dummy"
*recast int AWD

*recode HHRACE (1 2 3 4 = 0) (5 = 1) (6/21 = 0), gen(HAWI)
*label var HAWI "Hawaiian, Pacific Islander"
