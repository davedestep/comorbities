rm(list=ls())

install.packages("icd")
install.packages("tidyverse")
install.packages("naniar")
install.packages("janitor")
install.packages("pccc")
install.packages("sqldf")
install.packages("summarytools")
library(tidyverse)
library(icd)
library(naniar)
library(pccc)
library(sqldf)
library(summarytools)
setwd("C:\\Users\\Dave\\Documents\\comorbid R")
comorbid<-haven::read_sas("visit3.sas7bdat")
comorbid2<-comorbid%>% select(ID:new31)


comorbid3<- comorbid2 %>% 
  pivot_longer("new_DX_1":"new31",
               names_to = "number",
               values_to = "diagnosis" 
  ) 




comorbid4<-comorbid3 %>% replace_with_na(replace = list(diagnosis ="" )) %>%
  na.omit()

comorbid5<- comorbid4%>%
  select(c(-visit_before,-number))%>%
  distinct()
comorbid5$diagnosis2<-decimal_to_short(comorbid5$diagnosis)


#Get the PCC classifications instead. The CCI code will not be used because it can not account for ICD10 codes
ccc_result<-ccc(comorbid5, 
                id=ID,
                dx_cols = diagnosis2,
                pc_cols = ,
                icdv=09)

ccc_result2<-ccc(comorbid5, 
                id=ID,
                dx_cols = diagnosis2,
                pc_cols = ,
                icdv=10)
ccc_combined<-rbind(ccc_result, ccc_result2)

ccc_final<-sqldf("select ID, max(neuromusc) as neuromusc, max(cvd) as cvd, max(respiratory) as respiratory, max(renal) as renal, max(gi) as gi, max(hemato_immu) as hemato_immu, 
                  max(metabolic) as metabolic, max(congeni_genetic) as congeni_genetic, max(malignancy) as malignancy, max(neonatal) as neonatal, max(tech_dep) as tech_dep, 
                  max(transplant) as transplant, max(ccc_flag) as ccc_flag
      from ccc_combined
       group by ID")
#ICD9
ccc_9<-sqldf("select ID, max(neuromusc), max(cvd), max(respiratory), max(renal), max(gi), max(hemato_immu), max(metabolic), max(congeni_genetic), max(malignancy), max(neonatal), max(tech_dep), max(transplant), max(ccc_flag)
      from ccc_result
       group by ID")
#ICD10
ccc_10<-sqldf("select ID, max(neuromusc), max(cvd), max(respiratory), max(renal), max(gi), max(hemato_immu), max(metabolic), max(congeni_genetic), max(malignancy), max(neonatal), max(tech_dep), max(transplant), max(ccc_flag)
      from ccc_result2
       group by ID")



dfSummary(ccc_final, style = "grid",  plain.ascii = TRUE)

##########################################################################################
#Remove malignancy, transplant, neuromusc, and congenital(besides scoliosis and craniofacial)
###########################################################################################
ccc_final2<-ccc_final %>% 
  filter(malignancy==0, transplant==0, neuromusc==0)

dfSummary(ccc_final2, style = "grid",  plain.ascii = TRUE)

congenital_codes<-ccc_final2 %>% 
  filter(congeni_genetic==1) 

comorbid6<-comorbid5 %>% mutate(diagnosis2=as.character(diagnosis2))


#Adding scoliosis kids back to the dataset


congenital_codes<-sqldf("select a.congeni_genetic, b.*
from congenital_codes as a
left join comorbid6 as b
on a.ID=b.ID")

# 73730. Idiopathic scoliosis
# 73739. Scoliosis NEC
# 73734. Thoracogenic scoliosis
# 73732. Progr idiopath scoliosis
# 73731. Resolv idiopath scolios
#I am including kyphosis as well.
#7560. Anomal skull/face bones


congenital_codes<-congenital_codes %>% 
  mutate(scoliosis=if_else(diagnosis2 %in% c(73730, 7373, 7371, 737, 73743, 73719, 73739, 73734, 73732, 73731, 7560, "M4120", "M412", "M4100", "M965", "M4130", "M4180", "M419", "M4", "M4000", "M40209", "M962", "M963", "M40299", "M4140", "M4150","M40299","M4180","M419","M4130", "M4100", "M4100", "Q759", "Q752","Q750","Q75"), 1, 0)) %>% 
  filter(scoliosis==1) %>% 
  distinct(ID)

ccc_final3<-ccc_final2 %>% 
  filter(congeni_genetic==0)


scol<-sqldf("select a.*, b.*
      from congenital_codes as a
      left join ccc_final2 as b
      on a.ID=b.ID") %>% 
  select(-ID..2 )

ccc_final4<-rbind(ccc_final3, scol) %>% 
  rename(scol=congeni_genetic) 
  mutate()


 
######################################################################################################
######################################################################################################
######################################################################################################
#CCI
getwd()
cci<-read.csv("cci2015.csv") %>% janitor::clean_names()


cci$icd_9_cm_code <- gsub("'", '', cci$icd_9_cm_code)
cci$category_description <- gsub("'", '', cci$category_description)
cci$body_system <- gsub("'", '', cci$body_system)
######################################################################################################
######################################################################################################
######################################################################################################


#Remove trailing zeros?
comorbid5$test<-gsub('0+$', '', comorbid5$diagnosis2)
cci$diagnosis2<-as.icd_short_diag(cci$x_icd_9_cm_code, value = TRUE)
cci$test<-gsub('0+$', '', cci$diagnosis2)
cci$test2<-gsub('0+$', '', cci$test)


library(sqldf)


cci$test2<-as.character(cci$test)
cci<-cci %>% 
  mutate(test2 = str_trim(test2))
cci$test2<-gsub('0+$', '', cci$test2)

comorbid5$test2<-as.character(comorbid5$test)




test<- left_join(comorbid5, cci, by=c("test2"))


comorbidities_final<-sqldf("select a.id, a.diagnosis, a.test2, b.x_icd_9_cm_code_description, b.x_icd_9_cm_code, b.test2, b.x_category_description 
      from comorbid5 as a
      left join cci as b
      on a.test2=b.test2 ")

#V681
#7847

comorbidities_final2<- comorbidities_final %>% 
 rename(dx_cii=test2..6)

haven::write_sav(comorbidities_final2, "comobidities_final.sav")


####################################################################
###Including ICD10 codes as well as ICD9
###Importing the most recent CCI table, so the previous code can be skipped
####################################################################

CCI_final<-haven::read_sas("comorbidites_final.sas7bdat")
#Change variable for ICD10 to match with CCI by removing the period
CCI_final$diagnosis_ICD10_cohort <- gsub('\\.', '', CCI_final$diagnosis)

ICD10<-read.csv("cci_icd10cm_2019_1.csv") %>% janitor::clean_names()
ICD10$x_icd_10_cm_code <- gsub("'", '', ICD10$x_icd_10_cm_code)
ICD10$x_chronic_indicator <- gsub("'", '', ICD10$x_chronic_indicator)
ICD10$x_body_system <- gsub("'", '', ICD10$x_body_system)


CCI_with_ICD10<-sqldf("select a.*, b.x_icd_10_cm_code_description, b.x_icd_10_cm_code, b.x_chronic_indicator
      from CCI_final as a
      left join ICD10 as b
      on a.diagnosis_ICD10_cohort=x_icd_10_cm_code")

#Merge the ICD10 and ICD9 variables
CCI_end<-CCI_with_ICD10 %>%
  mutate(DV_1 = coalesce(x_icd_9_cm_code_description, x_icd_10_cm_code_description),
         DV_2 = coalesce(x_icd_9_cm_code, x_icd_10_cm_code),
         DV_3 = coalesce(x_category_description, x_chronic_indicator))
CCI_end<-CCI_end %>%
  mutate(DV_1 = coalesce(x_icd_10_cm_code_description, x_icd_9_cm_code_description),
         DV_2 = coalesce(x_icd_10_cm_code, x_icd_9_cm_code),
         DV_3 = coalesce(x_chronic_indicator, x_category_description))


save(CCI_end, file="CCI_end.Rda")

load("CCI_end.Rda")





######################################################################################################
######################################################################################################
#Clean CCI all over again, since we need body system, combine ICD9 and ICD10 files
#do not want to ammend previous code (first task for comorbidity, was not planning to build off of it)
######################################################################################################
######################################################################################################


#CCI
getwd()
ICD9<-read.csv("cci2015.csv") %>% janitor::clean_names()

#ICD9
ICD9$icd_9_cm_code <- gsub("'", '', ICD9$icd_9_cm_code)
ICD9$category_description <- gsub("'", '', ICD9$category_description)
ICD9$body_system <- gsub("'", '', ICD9$body_system)


#ICD10
ICD10<-read.csv("cci_icd10cm_2019_1.csv") %>% janitor::clean_names()
ICD10$x_icd_10_cm_code <- gsub("'", '', ICD10$x_icd_10_cm_code)
ICD10$x_chronic_indicator <- gsub("'", '', ICD10$x_chronic_indicator)
ICD10$x_body_system <- gsub("'", '', ICD10$x_body_system)
#Rename col names?
names(ICD10)<- substring(names(ICD10), 3) 

#Combine both
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}
ICD910<-force_bind(ICD9, ICD10) 
ICD910<- ICD910 %>% distinct(code_description, body_system)


#Get Body system for CCI_end
#This results in an extra 7 records, aparrently a few DX descriptions had different body system classifications.. Not important enough to make perfect
CCI_end_body<-
  sqldf("select a.*, b.body_system
      from CCI_end as a
      left join ICD910 as b
      on a.DV_1 = b.code_description")
save(CCI_end_body, file="CCI_end_body.Rda")





#########################################################################################
#########################################################################################
####Join ccc_final4 with CCI_end, get max of cci for whether or not the are chronic######
#########################################################################################
#########################################################################################

test<-sqldf("select ID, max(DV_3) as chronic, min(DV_3) as acute, body_system 
      from CCI_end_body
      group by ID, body_system") %>% 
  na.omit()

cci_end2 <- test %>%  
  arrange(body_system) %>%
  mutate(acute=ifelse(acute==0, 1, 0)) %>% 
  pivot_wider(id_cols = ID,
              names_from = "body_system",
              values_from=c(chronic, acute)) %>% 
  select(ID, 
         "chronic_None",
         "chronic_1",
         "chronic_2",
         "chronic_3",
         "chronic_4",
         "chronic_5",
         "chronic_6",
         "chronic_7",
         "chronic_8",
         "chronic_9",
         "chronic_10",
         "chronic_11",
         "chronic_12",
         "chronic_13",
         "chronic_14",
         "chronic_15",
         "chronic_16",
         "chronic_17",
         "chronic_18",
         "acute_None",
         "acute_1",
         "acute_2",
         "acute_3",
         "acute_4",
         "acute_5",
         "acute_6",
         "acute_7",
         "acute_8",
         "acute_9",
         "acute_10",
         "acute_11",
         "acute_12",
         "acute_13",
         "acute_14",
         "acute_15",
         "acute_16",
         "acute_17",
         "acute_18"
         ) %>% 
  mutate(acute_overall=ifelse(acute_1==0, 1, 0))


cci_end3<-cci_end2


cci_end3$chronic_overall <- apply(cci_end3[,c("chronic_None",
                                              "chronic_1",
                                              "chronic_2",
                                              "chronic_3",
                                              "chronic_4",
                                              "chronic_5",
                                              "chronic_6",
                                              "chronic_7",
                                              "chronic_8",
                                              "chronic_9",
                                              "chronic_10",
                                              "chronic_11",
                                              "chronic_12",
                                              "chronic_13",
                                              "chronic_14",
                                              "chronic_15",
                                              "chronic_16",
                                              "chronic_17",
                                              "chronic_18")] == 1,1,any)

cci_end3$acute_overall <- apply(cci_end3[,c("acute_None",
                                              "acute_1",
                                              "acute_2",
                                              "acute_3",
                                              "acute_4",
                                              "acute_5",
                                              "acute_6",
                                              "acute_7",
                                              "acute_8",
                                              "acute_9",
                                              "acute_10",
                                              "acute_11",
                                              "acute_12",
                                              "acute_13",
                                              "acute_14",
                                              "acute_15",
                                              "acute_16",
                                              "acute_17",
                                              "acute_18")] == 1,1,any)

cci_end3$acute_overall<-cci_end3$acute_overall %in% TRUE
cci_end3$chronic_overall<-cci_end3$chronic_overall %in% TRUE
cci_end3[is.na(cci_end3)] <- 0

  


save(cci_end3, file="CCI_end_body.Rda")

#Join

FINAL_CCC_CCI<-sqldf("select a.*, b.*
      from ccc_final4 as a
      left join cci_end3 as b
      on a.ID=b.ID") %>% 
  select(-ID..15)

save(FINAL_CCC_CCI, file="FINAL_CCC_CCI")




library(summarytools)

FINAL_CCC_CCI2<-FINAL_CCC_CCI %>% 
  select(-ID)
  view(dfSummary(FINAL_CCC_CCI2))
