#SET WD

library(tidyverse)

memory.limit(size=56000)

#Directory where the simulation results are saved
directory1<-"res/"


#Directory where the full final compiled dataset will be saved
dir.create("final_dataset")



#set w to equal the total number of output files saved in the "\res" folder, i.e. if using a standard PC then set w=1, if using a multi-cluster computer then w>1

w=1
m=0

#Set LifeSimN equal to the total number of individuals in the cohort that you simulate (100,000 -- in the paper)
LifeSimN=5

####################################################################################################################
####################################################################################################################
#Load,  join and clean the datafiles for non recipients - baseline scenario

for(i in 1:w){
   load(paste(directory1, "norecip-", i, ".RData", sep=""))
   x<-norecips
   x<-bind_rows( x ,.id="id")
   x<-transform(x, id = as.numeric(id)) 
   assign("y_number", summarize(subset(x, age==0), count=n())[[1]])
   
   x<-mutate(x,  POLICY=FALSE, id=id+m)
   m<-y_number+m
   assign(paste("norecip-", i, ".RData", sep=""), x)}
rm(norecips)
all_set1 <- do.call(rbind, lapply( ls(patt="norecip"), get) )
rm(list=ls(pattern="norecip"))


keeps<-c("id",   "recip", "POLICY", "life_stage", "sex", "age", "sep" , "sep_b", "edu_p", 
          "mhealth_p2", "rutter_p", 
         "cognitive_capital", "social_emotional_capital", "impact", "effect", "effect2", "pr_cd", "cd", "pr_edu", "edu",
         "pr_unh_beh",  "pr_unh_beh_np",    "pr_unh_beh_p" , "unh_beh" , "pr_chd","pr_chd_trend", "chd", 
         "pr_mhealth","pr_mhealth_trend" , "mhealth", "health", "pr_employed", 
         "employed" , "earnings", "consumption", "consumption_op", "depr", "wealth", 
         "pension", "savings", "interest",
         "pr_prison" , "prison", "pr_res", "res", "pdeath",
         "taxes" , "benefits",  "check", 
         "cd_costs" , "prison_costs", "res_costs", "chd_costs", "mhealth_costs", "ahcosts", "public_costs"  )


all_set1<-select(all_set1, all_of(keeps) )

####################################################################################################################
####################################################################################################################
#Load,  join and clean the datafiles for recipients - baseline scenario
see<-subset(all_set1, age==0)
nnr=summarize(see, count=n())[[1]]

m=nnr
for(i in 1:w){
   load(paste(directory1, "recip-", i, ".RData", sep=""))
   x<-recips
   x<-bind_rows( x ,.id="id")
   x<-transform(x, id = as.numeric(id)) 
   assign("y_number", summarize(subset(x, age==0), count=n())[[1]])
   
   x<-mutate(x,  POLICY=FALSE, id=id+m)
   m<-y_number+m
   assign(paste("recip-", i, ".RData", sep=""), x)}
rm(recips)

all_set2 <- do.call(rbind, lapply( ls(patt="recip"), get) )
rm(list=ls(pattern="recip"))

all_set2<-select(all_set2, all_of(keeps) )


see<-subset(all_set2, age==0)
nr=summarize(see, count=n())[[1]]
m=nr+nnr
# ####################################################################################################################
# ####################################################################################################################
#Join the nonrecipient and recipient datasets (baseline scenario)
all_set<-bind_rows(all_set1, all_set2)
# ####################################################################################################################
# ####################################################################################################################
#Save the baseline dataset
save(all_set,   file="final_dataset/LifeSim_lives.RData" )
########################################################################################################
########################################################################################################
# #Store the datafiles for nonrecipients - policy scenario (identical to baseline scenario)
 all_set1_pol<-subset(all_set, recip==0)
see<-subset(all_set1_pol, age==0)
nnr=summarize(see, count=n())[[1]]
nr=LifeSimN-nnr
m=nr+nnr
all_set1_pol<-mutate(all_set1_pol,  POLICY=TRUE, id=id+m)
########################################################################################################
########################################################################################################
#Load,  join and clean the datafiles for recipients - policy scenario
m=nr+2*nnr
rm(see)

for(i in 1:w){
   load(paste(directory1, "/recip_pol-", i, ".RData", sep=""))
   x<-recips_pol
   x<-bind_rows( x ,.id="id")
   x<-transform(x, id = as.numeric(id)) 
   assign("y_number", summarize(subset(x, age==0), count=n())[[1]])
   
   x<-mutate(x,  POLICY=TRUE, id=id+m)
   m<-y_number+m
   assign(paste("recip_pol-", i, ".RData", sep=""), x)}
rm(recips_pol)
all_set2_pol <- do.call(rbind, lapply( ls(patt="recip_pol"), get) )

all_set2_pol<-select(all_set2_pol, all_of(keeps) )

see<-subset(all_set2_pol, age==0)
nr=summarize(see, count=n())[[1]]
rm(list=ls(pattern="recip_pol"))
#####################################################################################################################
#####################################################################################################################
#Join the nonrecipient and recipient datasets (policy scenario)
all_set_pol<-bind_rows(all_set1_pol, all_set2_pol)
rm(all_set1_pol, all_set2_pol, all_set1, all_set2)
#####################################################################################################################
#####################################################################################################################
#use the consumption that takes into account the opportunity costs as a result of the policy (optional -- simplified way of modelling this)
all_set_pol<-mutate(all_set_pol, cons=consumption)
all_set_pol<-mutate(all_set_pol, consumption=ifelse(age>=5 & age <=14, consumption_op, cons))
#####################################################################################################################
#####################################################################################################################
#Save the policy scenario dataset
save(all_set_pol,  file= "final_dataset/LifeSim_lives_pol.RData" )






#check that total n
all_set_pol<-bind_rows(all_set, all_set_pol)

see<-subset(all_set_pol, age==0)
n=summarize(see, count=n())[[1]]

#take into account op costs

#ys added A and B
A <- (1000^(1-1.26))/(1000^(1-1.26)-24000^(1-1.26))
B <- 1/(1000^(1-1.26)-24000^(1-1.26))



all_set_pol<-mutate(all_set_pol, cons=consumption)

all_set_pol<-mutate(all_set_pol, consumption=ifelse(POLICY==TRUE&(age>=5 & age <=14), consumption_op, cons))

all_set_pol<-mutate(all_set_pol,  consumptiond=consumption/(1+0.015)^{age}, healthd=health/(1+0.015)^{age},
                    QALY=health+A-B*(consumption)^(1-1.26), 
                    QALYd=(health+A-B*(consumption)^(1-1.26))/(1+0.015)^{age}, wealthd=wealth/(1+0.015)^{age})

save(all_set_pol , file = paste(directory2,"/all_set_",polname,".RData", sep=""))

save(all_set_pol, file=  "final_dataset/LifeSim_lives_pol.RData" )

#Convert the R dataframe into excel version  
#for data with policy
load("final_dataset/LifeSim_lives.RData")
write.table(all_set_pol,"excel version with policy.csv",sep=",")

#for data without policy
load("final_dataset/LifeSim_lives.RData")
write.table(all_set,"excel version without policy.csv",sep=",")


