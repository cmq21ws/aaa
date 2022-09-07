setwd("C:/Sheffield/HEDM/Dissertation/My dissertation/Dissertation/R code of Lifesim/lifesim-simulator-policies")
library(foreign)
#SET n EQUAL TO THE NUMBER OF INDIVIDUALS THAT YOU WISH TO SIMULATE (IN EACH CORE OF THE CLUSTER, IF USING A HPC CLUSTER, OR IN TOTAL -- IF USING A STANDARD PC )

#want to know a population level
n=5
#leva ran 20,000, with justifications (she took justification)

#CLUSTER OR A STANDARD PC? 
#COMMENT THE NEXT LINE OUT IF USING A HPC CLUSTER
 rs_ext =  1
#COMMENT THE NEXT TWO LINES OUT IF USING A STANDARD PC AND NOT A HPC CLUSTER
 # args = commandArgs(trailingOnly=TRUE)
 # rs_ext =  as.integer(args[1])
 
#CREATE A LOCAL FOLDER(S) "/res" WHERE THE RESULTS WILL BE SAVED
dir.create("res")

#SET SEED
numbergen=123456

load("target_data/initial.RData")
####################################################################################################################
####################################################################################################################
#LOAD THE CODE FOR SIMULATING THE DESIRED SCENARIOS
#load the code for simulating the baseline scenario
install.packages("tidyverse")


source("scenarios/no_policy/version_11feb21/Person.R")

#new! load the code for simulating the policy scenario
source("scenarios/policy/version_11feb21/Person_pol.R")
####################################################################################################################
####################################################################################################################
#LOAD THE TARGET DATASETS
load("target_data/initialdata.RData")
load("target_data/mortality_data.RData")
load("target_data/targetdata_sexsepage.RData")
load("target_data/targetdata_sexage.RData")
load("target_data/targetdata_trends.RData")
load("target_data/average_data_total.RData")
load("target_data/hcosts.RData")
load("target_data/eq5d.RData")
####################################################################################################################
####################################################################################################################
 #THE FOLLOWING LINE IS TO SET A CRITERIOR IF NEED TO SEPARATE  INDIVIDUALS INTO A GROUP OF RECIPIENTS "rdf_recip" AND NON-RECIPIENTS "rdf_norecip" USING SOME CRITERIOR  (IN THIS EXAMPLE THE CRITERIOR TO DEFINE THE RECIPIENT GROUP IS SDQ CONDUCT PROBLEM SCORE AT AGE 5 (MCS WAVE 3) BEING 4 OR ABOVE)
df<-mutate(initialdata, recip=ifelse(sdq_cond4>=4, 1, 0))
####################################################################################################################
####################################################################################################################
#SETTING SEED
RNGversion("3.5.1")
set.seed(rs_ext*123)
####################################################################################################################
####################################################################################################################
#THE LINE BELOW SAMPLES THE INITIAL CHILDHOOD DATASET AND STORES IT IN A DATAFRAME rdf  
rdf=df[sample(nrow(df),  replace=TRUE, n, prob=df$fovwt1),]
#THE FOLLOWING LINES SPLIT rdf INTO TWO GROUPS -- THE RECIPIENTS AND NON-RECIPIENTS -- AND THEN COUNT THE N IN EACH GROUP
rdf_recip=subset(rdf,recip==1)
rdf_norecip=subset(rdf, recip==0)
# rdf_norecip<-mutate(rdf_norecip, recip=1)
no_recip=summarize(rdf_recip, count=n())[[1]]
no_total=summarize(rdf_recip, count=n())[[1]]+summarize(rdf_norecip, count=n())[[1]]
####################################################################################################################
####################################################################################################################
#THE FOLLOWING LINES SPECIFY THE FUNCTION TO RUN MODEL ASSUMING THE SCENARIO WITHOUT POLICY (THE BASELINE SCENARIO)
run_model = function(  rdf_dat, mortality_data, life_expectancy, number_of_patients, targetdata_mort,targetdata_sex,  targetdata_tot, targetdata_tre, eq5d_data, hcosts_data, numbergenerator, no_reci, no_tota){
  rdf_da=rdf_dat
  mortality = filter(mortality_data)%>%select(AGE,PROB_MORT, SEP, MALE)
  le = life_expectancy=filter(life_expectancy)$le
  targetdata_m=targetdata_mort
  targetdata_se=targetdata_sex
  targetdata_to= targetdata_tot
  targetdata_tr= targetdata_tre
  eq5d_dat=filter(eq5d_data)%>%select(AGE, EQ5D, SEP, MALE, sex)
  hcosts_dat=filter(hcosts_data)%>%select(AGE, HCOSTS, SEP, MALE, sex)
  no_rec=no_reci
  no_tot=no_tota
  results = list()
  for(i in 1:number_of_patients){
RNGversion("3.5.1")
    set.seed(rs_ext*numbergen + i)
    results[[i]] = Person$new( rdf_da, mortality,le, i, targetdata_m,targetdata_se, targetdata_to,  targetdata_tr, eq5d_dat, hcosts_dat, numbergen, no_rec, no_tot )$live_life()$get_life_history()
  }

  return(results)
}
####################################################################################################################
####################################################################################################################
#NEW! THE FOLLOWING LINES SPECIFY THE FUNCTION TO RUN MODEL ASSUMING THE SCENARIO WITH POLICY (COMMENT THIS PART OUT IF NEED TO RUN ONLY THE BASELINE SIMULATION WITHOUT POLICY SCENARIO)
####################################################################################################################
####################################################################################################################
run_model_pol = function(  rdf_dat, mortality_data, life_expectancy, number_of_patients, targetdata_mort,targetdata_sex,  targetdata_tot, targetdata_tre, eq5d_data, hcosts_data, numbergenerator, no_reci, no_tota){
  rdf_da=rdf_dat
  mortality = filter(mortality_data)%>%select(AGE,PROB_MORT, SEP, MALE)
  le = life_expectancy=filter(life_expectancy)$le
  targetdata_m=targetdata_mort
  targetdata_se=targetdata_sex
  targetdata_to= targetdata_tot 
  targetdata_tr= targetdata_tre
  eq5d_dat=filter(eq5d_data)%>%select(AGE, EQ5D, SEP, MALE, sex)
  hcosts_dat=filter(hcosts_data)%>%select(AGE, HCOSTS, SEP, MALE, sex)
  no_rec=no_reci
  no_tot=no_tota
  results = list()
  for(i in 1:number_of_patients){
    RNGversion("3.5.1")
    set.seed(rs_ext*numbergen + i)
    results[[i]] = Person_pol$new( rdf_da, mortality,le, i, targetdata_m,targetdata_se, targetdata_to,  targetdata_tr, eq5d_dat, hcosts_dat, numbergen, no_rec, no_tot )$live_life()$get_life_history() 
  }
  
  return(results)
}
####################################################################################################################
####################################################################################################################
#THE FOLLOWING LINES TELL TO RUN THE SIMULATION FOR NONRECIPIENT GROUP AND RECIPIENT GROUP  (WITH POLICY AND WITHOUT POLICY) AND SAVE THE DATA

#NONRECIPIENT GROUP  (WITHOUT POLICY=WITH POLICY)

n=summarize(rdf_norecip, count=n())[[1]]
s=123456
rdf=rdf_norecip
RNGversion("3.5.1")
  set.seed(rs_ext*123487)
  
#take a while to run
norecips = run_model(rdf_dat=rdf, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n, targetdata_mort=targetdata_sexsepage, targetdata_sex=targetdata_sexage, targetdata_tot=average_data_total, targetdata_tre=targetdata_trends, eq5d_data=eq5d, hcosts_data=hcosts, numbergenerator=s, no_reci=no_recip,  no_tota=no_total)
save(norecips , file = paste("res/norecip-", rs_ext, ".RData", sep=""))

#RECIPIENT GROUP WITHOUT POLICY
n=summarize(rdf_recip, count=n())[[1]]
s=663456
rdf=rdf_recip
recips = run_model(rdf_dat=rdf, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n, targetdata_mort=targetdata_sexsepage, targetdata_sex=targetdata_sexage, targetdata_tot=average_data_total, targetdata_tre=targetdata_trends, eq5d_data=eq5d, hcosts_data=hcosts, numbergenerator=s, no_reci=no_recip,  no_tota=no_total)
save(recips , file = paste("res/recip-", rs_ext, ".RData", sep=""))




#NEW! RECIPIENT GROUP WITH POLICY
n=summarize(rdf_recip, count=n())[[1]]
s=663456
rdf=rdf_recip
recips_pol = run_model_pol(rdf_dat=rdf, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n, targetdata_mort=targetdata_sexsepage, targetdata_sex=targetdata_sexage, targetdata_tot=average_data_total, targetdata_tre=targetdata_trends, eq5d_data=eq5d, hcosts_data=hcosts, numbergenerator=s, no_reci=no_recip,  no_tota=no_total)
save(recips_pol , file = paste("res/recip_pol-", rs_ext, ".RData", sep=""))


