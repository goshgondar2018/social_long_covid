library(tidyverse)
library(table1)
library(table1)

data_combined_cohort=read.csv("./data/QALDs_full_dataset_combined_cohort.csv")
data_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
data_UK=read.csv("./data/QALDs_full_dataset_UK.csv")

# COMBINED MIC COHORT
data_combined_cohort_binary_vars=data_combined_cohort%>%
  dplyr::select(-QALD_final_weighted,-AGE,-SEX,-employment_status,
                -employment_status_category,-COUNTRY,-severity_indicator)

sort(colMeans(data_combined_cohort_binary_vars)) # proportions of each comorbidity 

data_combined_cohort_binary_vars[data_combined_cohort_binary_vars==1]='PRESENT'
data_combined_cohort_binary_vars[data_combined_cohort_binary_vars==0]='ABSENT'

data_combined_cohort_for_table=cbind.data.frame(data_combined_cohort_binary_vars,
                                                data_combined_cohort[,c("QALD_final_weighted","AGE","SEX","COUNTRY")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

common_vars_ALL=intersect(intersect(colnames(data_Norway_for_table),
                                    colnames(data_UK_for_table)),
                          colnames(data_combined_cohort_for_table))

data_ALL_for_table=rbind.data.frame(cbind.data.frame(data_Norway_for_table[,common_vars_ALL],
                                                     country=rep('Norway',nrow(data_Norway_for_table))),
                                    cbind.data.frame(data_UK_for_table[,common_vars_ALL],
                                                     country=rep('UK',nrow(data_UK_for_table))))
data_ALL_for_table=rbind.data.frame(data_ALL_for_table, 
                                    cbind.data.frame(data_combined_cohort_for_table[,common_vars_ALL],
                                                     country=rep('Combined MIC cohort',nrow(data_combined_cohort_for_table))))

colnames(data_ALL_for_table)[1:20]=c("ASTHMA",
                                     "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                     "CHRONIC HAEMATOLOGICAL DISEASE",
                                     "CHRONIC KIDNEY DISEASE","CHRONIC NEUROLOGICAL DISORDER",
                                     "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                     "DIABETES MELLITUS (TYPE 1)",
                                     "DIABETES MELLITUS (TYPE 2)",
                                     "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                     "HYPERTENSION",
                                     "MALIGNANT NEOPLASM","PSYCHOLOGICAL DISORDER",
                                     "OBESITY","OTHER","RHEUMATOLOGICAL DISORDER",
                                     "SMOKING","Long COVID QALDs",
                                     "AGE","SEX",
                                     "country")

# NORWAY

data_Norway_sex=data_Norway%>%
  filter(SEX!='U')%>%
  mutate(SEX=ifelse(SEX=='F',1,0))

data_Norway_binary_vars=data_Norway%>%
  dplyr::select(-QALD_final_weighted,-AGE,-SEX,-educ_yrs,-educ_yrs_quintile,
                -is_vaccinated)#,-vaccination_type)

sort(colMeans(data_Norway_binary_vars)) # proportions of each comorbidity 
data_Norway_binary_vars[data_Norway_binary_vars==1]='PRESENT'
data_Norway_binary_vars[data_Norway_binary_vars==0]='ABSENT'

data_Norway_for_table=cbind.data.frame(data_Norway_binary_vars,
                                       data_Norway[,c("QALD_final_weighted","AGE","SEX")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

prop_vaccinated=mean(data_Norway$is_vaccinated,na.rm=TRUE)

# UK
data_UK_sex=data_UK%>%
  mutate(SEX=ifelse(SEX=='F',1,0))

data_UK_binary_vars=data_UK%>%
  dplyr::select(-QALD_final_weighted,-AGE,-SEX,-employment_status,
                -employment_status_category,-is_treated_antiviral,
                -severity_indicator)#-antiviral_type,

sort(colMeans(data_UK_binary_vars)) # proportions of each comorbidity 
data_UK_binary_vars[data_UK_binary_vars==1]='PRESENT'
data_UK_binary_vars[data_UK_binary_vars==0]='ABSENT'

data_UK_for_table=cbind.data.frame(data_UK_binary_vars,
                                       data_UK[,c("QALD_final_weighted","AGE","SEX")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

table1_all=table1 (~AGE+
          SEX+ASTHMA+`CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)`+
          `CHRONIC HAEMATOLOGICAL DISEASE`+`CHRONIC KIDNEY DISEASE`+
          `CHRONIC NEUROLOGICAL DISORDER`+`CHRONIC PULMONARY DISEASE (NOT ASTHMA)`+
          `DIABETES MELLITUS (TYPE 1)`+`DIABETES MELLITUS (TYPE 2)`+
          `DIABETES MELLITUS (TYPE NOT SPECIFIED)`+HYPERTENSION+
          `MALIGNANT NEOPLASM`+
          `PSYCHOLOGICAL DISORDER`+
          OBESITY+`RHEUMATOLOGICAL DISORDER`+
            SMOKING+`Long COVID QALDs`|country,data=data_ALL_for_table,
          topclass="Rtable1-zebra")
table1_all
save(table1_all, file = "out.rda")

IQR_QALDs_Norway=quantile(data_Norway_for_table$QALD_final_weighted, probs = c(0.25,0.75))
IQR_QALDs_UK=quantile(data_UK_for_table$QALD_final_weighted, probs = c(0.25,0.75))
IQR_QALDs_combined_cohort=quantile(data_combined_cohort_for_table$QALD_final_weighted, probs = c(0.25,0.75))

