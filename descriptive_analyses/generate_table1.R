library(tidyverse)
library(table1)
library(table1)

ata_Russia=read.csv("./data/QALDs_full_dataset_Russia_sep3_2025.csv")
data_Norway=read.csv("./data/QALDs_full_dataset_Norway_aug29_2025.csv")
data_UK=read.csv("./data/QALDs_full_dataset_UK_aug29_2025.csv")

# Norway 

data_Norway_sex=data_Norway%>%
  filter(SEX!='U')%>%
  mutate(SEX=ifelse(SEX=='F',1,0))

data_Norway_binary_vars=data_Norway%>%
  dplyr::select(-QALD,-AGE,-SEX,-educ_yrs,-educ_yrs_quintile,
                -is_vaccinated,-USUBJID)#,-vaccination_type)

sort(colMeans(data_Norway_binary_vars)) # proportions of each comorbidity 
data_Norway_binary_vars[data_Norway_binary_vars==1]='PRESENT'
data_Norway_binary_vars[data_Norway_binary_vars==0]='ABSENT'

data_Norway_for_table=cbind.data.frame(data_Norway_binary_vars,
                                       data_Norway[,c("QALD","AGE","SEX")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

prop_vaccinated=mean(data_Norway$is_vaccinated,na.rm=TRUE)

# UK
data_UK_sex=data_UK%>%
  mutate(SEX=ifelse(SEX=='F',1,0))

data_UK_binary_vars=data_UK%>%
  dplyr::select(-QALD,-AGE,-SEX,-employment_status,
                -employment_status_category,-is_treated_antiviral,
                -severity_indicator,-USUBJID)#-antiviral_type,

sort(colMeans(data_UK_binary_vars)) # proportions of each comorbidity 
data_UK_binary_vars[data_UK_binary_vars==1]='PRESENT'
data_UK_binary_vars[data_UK_binary_vars==0]='ABSENT'

data_UK_for_table=cbind.data.frame(data_UK_binary_vars,
                                       data_UK[,c("QALD","AGE","SEX")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

# Russia
data_Russia_binary_vars=data_Russia%>%
  dplyr::select(-QALD,-AGE,-SEX,-employment_status,
                -employment_status_category,-severity_indicator,-USUBJID)

sort(colMeans(data_Russia_binary_vars)) # proportions of each comorbidity 

data_Russia_binary_vars[data_Russia_binary_vars==1]='PRESENT'
data_Russia_binary_vars[data_Russia_binary_vars==0]='ABSENT'

data_Russia_for_table=cbind.data.frame(data_Russia_binary_vars,
                                       data_Russia[,c("QALD","AGE","SEX")])%>%
  mutate(SEX=ifelse(SEX=='F','Female',ifelse(SEX=='M','Male','Unknown')))

# combine tables
common_vars_ALL=intersect(intersect(colnames(data_Norway_for_table),
                                    colnames(data_UK_for_table)),
                          colnames(data_Russia_for_table))

data_ALL_for_table=rbind.data.frame(cbind.data.frame(data_Norway_for_table[,common_vars_ALL],
                                                     country=rep('Norway',nrow(data_Norway_for_table))),
                                    cbind.data.frame(data_UK_for_table[,common_vars_ALL],
                                                     country=rep('UK',nrow(data_UK_for_table))))
data_ALL_for_table=rbind.data.frame(data_ALL_for_table, 
                                    cbind.data.frame(data_Russia_for_table[,common_vars_ALL],
                                                     country=rep('Russia',nrow(data_Russia_for_table))))

colnames(data_ALL_for_table)[1:18]=c("ASTHMA",
                                     "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                     "CHRONIC HAEMATOLOGICAL DISEASE",
                                     "CHRONIC KIDNEY DISEASE","CHRONIC NEUROLOGICAL DISORDER",
                                     "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                     "DIABETES MELLITUS (TYPE 2)",
                                     "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                     "HYPERTENSION",
                                     "MALIGNANT NEOPLASM",
                                     "OBESITY","OTHER","RHEUMATOLOGICAL DISORDER",
                                     "SMOKING","Long COVID utility scores",
                                     "AGE","SEX",
                                     "country")

table1_all=table1 (~AGE+
          SEX+ASTHMA+`CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)`+
          `CHRONIC HAEMATOLOGICAL DISEASE`+`CHRONIC KIDNEY DISEASE`+
          `CHRONIC NEUROLOGICAL DISORDER`+`CHRONIC PULMONARY DISEASE (NOT ASTHMA)`+
          `DIABETES MELLITUS (TYPE 2)`+
          `DIABETES MELLITUS (TYPE NOT SPECIFIED)`+HYPERTENSION+
          `MALIGNANT NEOPLASM`+
          OBESITY+`RHEUMATOLOGICAL DISORDER`+
            SMOKING+`Long COVID utility scores`|country,data=data_ALL_for_table,
          topclass="Rtable1-zebra")
table1_all
save(table1_all, file = "out.rda")



IQR_QALDs_Norway=quantile(data_Norway_for_table$QALD, probs = c(0.25,0.75))
median(data_Norway_for_table$QALD)
IQR_QALDs_UK=quantile(data_UK_for_table$QALD, probs = c(0.25,0.75))
median(data_UK_for_table$QALD)
IQR_QALDs_Russia=quantile(data_Russia_for_table$QALD, probs = c(0.25,0.75))

