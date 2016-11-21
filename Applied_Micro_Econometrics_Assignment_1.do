drop if mrg1==2
gen age_sq= age^2
gen eduyrs_sq=eduyrs^2
gen log_NPERSONS= log(NPERSONS)
gen nchild0_5_sh= nchild0_5/NPERSONS
gen nchild6_15_sh= ( nchild6_11+ nchild12_15) / NPERSONS
tab martlst, gen (martlst)
tab eduadlt_cd , gen (eduadlt_cd )
tab educd, gen(educd)
tab URBAN4_2011 , gen (URBAN4_2011)
tab STATEID , gen (STATEID)
tab pcincq, gen (pcincq)
gen nadlfdep_sh= nadlfdep/NPERSONS
gen nadlmdep_sh =nadlmdep/NPERSONS
keep if gender==2 & ( age>=15 | age<=65 ) & study_only==0


****************************************Q1.a Create summary of all the variables******************************************************************

sum age eduyrs martlst* log_NPERSONS nchild0_5_sh nchild6_15_sh nfadlt_sh nadlfdep nadlmdep  eduadlt_cd* dst dsc dobc duch dosgr dhindu dmuslim dchrstn djain dorelg dsikh URBAN4_2011* psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim

*******************************************Q1.a Variant 1 (LFP = fa(individual, household, time)************************************************************************

probit dwork age age_sq eduyrs eduyrs_sq martlst2 martlst3 log_NPERSONS  nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh dst dobc duch dosgr dhindu dmuslim dchrstn

*******************************************Q1.a Variant 2 (Model 1) (LFP = fa(individual, household, time,location, employment)************************************************************************

probit dwork age age_sq eduyrs eduyrs_sq martlst2 martlst3 log_NPERSONS  nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim

*******************************************Q1.b Variant 3 (using edu_cd2 edu_cd3 edu_cd4 edu_cd5 edu_cd6)************************************************************************

probit dwork age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS  nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim

********************************************Q1b.ii To draw Graph***********************************************************************
predict yhat
gen phat= yhat if  URBAN4_20111==1
gen peduyrs= eduyrs if  URBAN4_20111==1
gen qhat= yhat if  URBAN4_20112==1
gen qeduyrs= eduyrs if  URBAN4_20112==1
gen rhat= yhat if  URBAN4_20113==1
gen reduyrs= eduyrs if  URBAN4_20113==1
gen shat= yhat if  URBAN4_20114==1
gen seduyrs= eduyrs if  URBAN4_20114==1
twoway (qfit phat peduyrs) (qfit qhat qeduyrs) (qfit rhat reduyrs) (qfit shat seduyrs)

****************************************************Q2.a Model 2a (using citerc_pcag2 citerc_pcag3)***************************************************************

probit dwork age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS  nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim citerc_pcag2 citerc_pcag3


****************************************************Q2.b Model 2b (using psu_sh_cig1 psu_sh_cig2 psu_sh_cig3)***********************************************

probit dwork age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3

****************************************************Q3. Testing equality of coefficients of the share of workers in the different sectors ***********

test psu_sh_wkagwag=psu_sh_wkanim=psu_sh_wkbsns=psu_sh_wkfarm=psu_sh_wknonag=psu_sh_wknrega=psu_sh_wksalry

****************************************************Q4.a Model 2b.1 (using state dummies)************************************************************

probit dwork STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3

************************************************Q4.b Model 2b.2 (using economic status... quin_g2 quin_g3 quin_g4 quin_g5)***********************************************

probit dwork quin_g2 quin_g3 quin_g4 quin_g5 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3

*************************************************Q4.c Model 2b.3 (using per capita income... pcincq2 pcincq3 pcincq4 pcincq4 pcincq5)***********************************************

probit dwork pcincq2 pcincq3 pcincq4 pcincq4 pcincq5 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3


*************************************************Q4.d Model 2b.4 (using occupation of the head of the household  )***********************************************

probit dwork incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3

***********************************************************Q6. Logit Model*************************************************************************************

logit dwork incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3

***********************************************************Q7. To calculate Odds ratio*************************************************************************************

logistic dwork incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3
**********************
*   SECTION 2   	 *
**********************
**********************************************************Q1. Multinominal Logit Model with RRR*************************************************************************************

mlogit wrk_3cat incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3, rr

**********************************************************Q2.a for testing the effects of education*************************************************
test [part_time]educd2 = [full_time]educd2
test [part_time]educd3 = [full_time]educd3
test [part_time]educd4 = [full_time]educd4
test [part_time]educd5 = [full_time]educd5
test [part_time]educd6 = [full_time]educd6
**********************************************************Q2.b testing part-time and full-time work are indistinguishable*****************************

test [part_time = full_time]

************************************************************Q2.c Hausman Test*****************************************************************************

mlogit wrk_3cat incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3
estimates store m1
mlogit wrk_3cat incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3 if  wrk_3cat != "part time":  wrk_3cat
estimate store m2
hausman m1 m2, alleqs constant
suest m1 m2, noomitted
test [m1_full_time =m2_full_time]


***************************************************************Ordered Logit Model*************************************************************************************

ologit wrk_3cat incohh2 incohh3 incohh4 incohh5 incohh6 incohh7 STATEID2 STATEID3 STATEID4 STATEID5 STATEID6 STATEID7 STATEID8 STATEID9 STATEID10 STATEID11 STATEID12 STATEID13 STATEID14 STATEID15 STATEID16 STATEID17 STATEID18 STATEID19 STATEID20 STATEID21 STATEID22 STATEID23 STATEID24 STATEID25 STATEID26 STATEID27 STATEID28 STATEID29 STATEID30 STATEID31 STATEID32 age age_sq educd2 educd3 educd4 educd5 educd6 martlst2 martlst3 log_NPERSONS nchild0_5_sh nchild6_15_sh  nadlfdep_sh nadlmdep_sh  dst dobc duch dosgr dhindu dmuslim dchrstn URBAN4_20112 URBAN4_20113 URBAN4_20114 psu_sh_wkfarm psu_sh_wkagwag psu_sh_wkbsns psu_sh_wknonag psu_sh_wknrega psu_sh_wksalry psu_sh_wkanim psu_sh_cig1 psu_sh_cig2 psu_sh_cig3
