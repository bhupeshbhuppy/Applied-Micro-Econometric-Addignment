keep if gender==1
keep if age>=15 & age<=60
keep if wgnw != 1
keep if wgnw!=2
keep if STATEID !=07
	
gen exp= (age - ED6 -5)
replace exp=0 if exp<=0
gen exp_sq = exp*exp

drop if exp==.
drop if exp_sq==.

******religion dummy
gen relg=ID11
gen dhindu= (relg==1)
gen dmuslim= (relg==2)
gen dchrstn= (relg==3)
gen dsikh= (relg==4)
gen djain= (relg==6)
gen dorelg= (relg==5 | relg>=7 |  relg==.)

****caste dummy
gen duch= (ID13==1)
gen dobc= (ID13==3)
gen dsc= (ID13==4)
gen dst= (ID13==5)
gen dosgr= (ID13==2 | ID13==6 | ID13==.)

*******Creating dummy  from categorical*****
tab region, g (region)
tab educd, g ( educd )
tab STATEID , g ( STATEID )
replace newoccup1=7 if newoccup1==.
tab newoccup1 , g ( newoccup1 )
replace newoccup2=7 if newoccup2==.
tab newoccup2 , g ( newoccup2 )
tab newindus1 , g ( newindus1 ) 
tab newindus2 , g ( newindus2) 


gen log_wages = log(wrkwg)
gen log_hr = log(wrkhr)
gen log_hrlywage = log(wrkwg/wrkhr)
drop if wgnw==.
drop if log_hrlywage==. & wgnw>=4


global xlist  exp exp_sq STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 educd2 educd3 educd4 educd5 educd6 region2 region3 dosgr dst dsc dobc duch dorelg djain dsikh dchrstn dmuslim dhindu newindus22 newindus23 newindus24 newindus25 newindus26 newindus27 newindus12 newindus13 newindus14 newindus15 newindus16 newindus17 newoccup12 newoccup13 newoccup14 newoccup15 newoccup16 newoccup17 newoccup22 newoccup23 newoccup24 newoccup25 newoccup26 newoccup27


summ log_hrlywage log_wages log_hr exp exp_sq STATEID* educd* region* dosgr dst dsc dobc duch dorelg djain dsikh dchrstn dmuslim dhindu newindus2* newindus1* newoccup1* newoccup2*
*outreg2 using asss.doc, replace sum(log) keep(log_hrlywage log_wages log_hr exp exp_sq STATEID* educd* region* dosgr dst dsc dobc duch dorelg djain dsikh dchrstn dmuslim dhindu newindus2* newindus1* newoccup1* newoccup2*)

******************************************************************Model 1 ***********************************************************************

regress log_hrlywage $xlist if wgnw>=4
*outreg2 using xyz.doc , replace ctitle (Model 1)

********************************************************* Model 2 ********************************************************************************

regress log_wages log_hr $xlist if wgnw>=4
*outreg2 using xyz.doc , append ctitle (Model 2)

*****Testing******
test log_hr == 1

*****we accept Ho: B (log_hr == 1) 

****************************************************tobit model*********************************************************
replace log_hrlywage = 0 if log_hrlywage==. & wgnw==3
replace log_hrlywage = 0 if log_hrlywage<0
replace log_hrlywage =0 if log_hrlywage <=0
replace log_hrlywage =0 if log_hrlywage ==.

tobit log_hrlywage $xlist if wgnw>=3 , ll

*outreg2 using xyz.doc , replace ctitle (TOBIT Model)

regress log_hrlywage $xlist if wgnw>=3

*outreg2 using xyz.doc , append ctitle (OLS Model)




*********************************************************QUestion 3**********************************************************
gen dy = log_hrlywage > 0
quietly tobit log_hrlywage $xlist if wgnw>=3 , ll
predict xb, xb
matrix btobit = e(b)
scalar sigma = btobit[1, e(df_m)+2]
generate threshold = (0 - xb)/sigma
gen lambda = normalden(threshold)/(1-normal(threshold))
gen uifdywq1= (log_hrlywage - xb )/sigma if dy==1
gen double gres1 = uifdywq1
replace gres1 = -lambda if dy ==0 
summ gres1
gen double gres2 = uifdywq1^2 -1
replace gres2 = -threshold*lambda if dy==0
gen double gres3 = uifdywq1^3
replace gres3 = -(2 + threshold^2)*lambda if dy==0
gen double gres4 = uifdywq1^4 - 3
replace gres4 = -(3*threshold + threshold^3)*lambda if dy==0

gen score_exp =gres1*exp
gen score_exp_sq =gres1*exp_sq
forvalues i=1/32{
gen score_STATEID`i' = gres1*STATEID`i'
}

forvalues i=1/6{
gen score_educd`i' = gres1*educd`i'
}


forvalues i=1/3{
gen score_region`i' = gres1*region`i'
}

forvalues i=1/7{
gen score_newindus2`i' = gres1*newindus2`i'
}

forvalues i=1/7{
gen score_newoccup1`i' = gres1*newoccup1`i'
}

forvalues i=1/8{
gen score_newoccup2`i' = gres1*newoccup2`i'
}

          
gen score_dosgr =gres1*dosgr
gen score_dst =gres1*dst
gen score_dsc =gres1*dsc
gen score_dobc =gres1*dobc
gen score_duch =gres1*duch
gen score_dorelg =gres1*dorelg
gen score_djain =gres1*djain
gen score_dsikh =gres1*dsikh
gen score_dchrstn =gres1*dchrstn
gen score_dmuslim =gres1*dmuslim
gen score_dhindu =gres1*dhindu

global scores score* gres1 gres2

gen one =1

regress one gres3 gres4 $scores, noconstant

display "N R^2 = "  e(N)*e(r2)  " with p-value = " chi2tail(2,e(N)*e(r2))
*** No normality

gen score2_exp =gres1*exp
gen score2_exp_sq =gres1*exp_sq
forvalues i=1/32{
gen score2_STATEID`i' = gres1*STATEID`i'
}

forvalues i=1/6{
gen score2_educd`i' = gres1*educd`i'
}


forvalues i=1/3{
gen score2_region`i' = gres1*region`i'
}

forvalues i=1/7{
gen score2_newindus2`i' = gres1*newindus2`i'
}

forvalues i=1/7{
gen score2_newoccup1`i' = gres1*newoccup1`i'
}

forvalues i=1/7{
gen score2_newoccup2`i' = gres1*newoccup2`i'
}

gen score2_dosgr =gres1*dosgr
gen score2_dst =gres1*dst
gen score2_dsc =gres1*dsc
gen score2_dobc =gres1*dobc
gen score2_duch =gres1*duch
gen score2_dorelg =gres1*dorelg
gen score2_djain =gres1*djain
gen score2_dsikh =gres1*dsikh
gen score2_dchrstn =gres1*dchrstn
gen score2_dmuslim =gres1*dmuslim
gen score2_dhindu =gres1*dhindu

global scores2 score* score2* gres1 gres2

regress one gres3 gres4 $scores2, noconstant

display "N R^2 = "  e(N)*e(r2)  " with p-value = " chi2tail(2,e(N)*e(r2))

*******///heteroskedicity present



*************Now we regress two part model*************************

quietly probit dy $xlist if wgnw>=3
*outreg2 using probitm.doc , replace ctitle (Part 1)

scalar llprobit = e(ll)
quietly regress log_hrlywage $xlist if wgnw>=3 & dy==1
*outreg2 using probitm.doc , append ctitle (Part 2)
scalar lllognormal = e(ll)
predict rlambexp, residual
scalar lltwopart = lllognormal+llprobit
display " two part log normal = " lltwopart
******** -30373.851

quietly regress log_hrlywage $xlist if wgnw>=3 & dy==1

hettest
sktest rlambexp

***************************************** Selection model******************************************

heckman log_hrlywage $xlist , select (dy = $xlist) nolog
outreg2 using heckmanm.doc , replace ctitle (Part 1)

******** the LR test shows that the errors are not normally distributed.
heckman log_hrlywage $xlist , select (dy = $xlist) twostep
*outreg2 using heckmanmstep.doc , replace ctitle (Part 2)
*** here as well there is heteroskedicity present

********************************* Model prediction tobit******************************************

gen yhat_tobit= exp(xb+0.5*sigma^2)*(1-normal((0-xb-sigma^2)/sigma))
gen yhat_tobit_UC= yhat_tobit/(1-normal(threshold )) if dy==1
summ log_hrlywage yhat_tobit
summ log_hrlywage yhat_tobit yhat_tobit_UC if dy==1

********************************* Model prediction Two part******************************************
quietly probit dy $xlist
predict dyhat, pr
quietly regress log_hrlywage  $xlist if dy==1
predict xbpos, xb
gen yhat_2step_UC = exp( xbpos+ 0.5*e(rmse)^2 )
gen yhat_2step = dyhat*yhat_2step_UC
summ yhat_2step log_hrlywage if dy==1
summ yhat_2step log_hrlywage
summ yhat_2step_UC log_hrlywage if dy==1
***************
********************************************Prediction from selection model********************************

quietly heckman log_hrlywage $xlist , select (dy = $xlist)
predict probpos, psel
predict x1b1, xbsel
predict x2b2, xb
scalar sig2sq = e(sigma)^2
scalar sig12sq = e(rho)*(e(sigma)^2)
display "sigma1sq = 1 " " sigma12sq = " sig12sq "sigma2sq = " sig2sq
gen yhat_heck = exp(x2b2 +0.5*(sig2sq))*(1-normal(-x1b1-sig12sq))
gen yhat_heck_UC = yhat_heck/probpos
summ yhat_heck log_hrlywage  prob dy
summ yhat_heck log_hrlywage  prob dy if dy==1



