library(tidyverse)
library(medoutcon)
library(sl3)
library(nnls)
library(hal9001)
library(speedglm)
library(biglm)
library(lightgbm)
library(nnet)

data_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
data_UK=read.csv("./data/QALDs_full_dataset_UK.csv")
data_Russia=read.csv("./data/QALDs_full_dataset_Russia.csv")

# instantiate learners 
## # ref: https://code.nimahejazi.org/medoutcon/articles/intro_medoutcon.html

### for binary variables
mean_lrnr <- Lrnr_mean$new()
gam_lrnr <- Lrnr_gam$new(family = binomial())
boosted_lrnr <- Lrnr_lightgbm$new(force_row_wise=TRUE) 
nnet_lrnr <- Lrnr_nnet$new(outcome_type='binomial') 

lrnr_lib <- Stack$new(mean_lrnr,gam_lrnr, boosted_lrnr,nnet_lrnr) 
sl_lrnr <- Lrnr_sl$new(learners = lrnr_lib, metalearner=Lrnr_nnls$new())

### for continuous variables
mean_lrnr2 <- Lrnr_mean$new()
gam_lrnr2 <- Lrnr_gam$new(family = gaussian())
boosted_lrnr2 <- Lrnr_lightgbm$new(force_row_wise=TRUE) 
nnet_lrnr2 <- Lrnr_nnet$new(outcome_type='continuous')

lrnr_lib2 <- Stack$new(mean_lrnr2,gam_lrnr2, boosted_lrnr2,nnet_lrnr2) 
sl_lrnr2 <- Lrnr_sl$new(learners = lrnr_lib2, metalearner=Lrnr_nnls$new())

# NORWAY

all_output_Norway = data.frame(matrix(nrow=15,ncol=4))
colnames(all_output_Norway)=c("LL","est","UL","effect")

data_mediation_Norway_educ_binary=data_Norway%>%
  mutate(SEX=as.factor(SEX))%>%
  mutate(high_educ=ifelse(educ_yrs_quintile<3,0,1))

data_mediation_Norway_educ_5_1=data_Norway%>%
  mutate(SEX=as.factor(SEX))%>%
  mutate(category_five_indicator=ifelse(educ_yrs_quintile==5,1,0))

data_mediation_Norway_educ_4_1=data_Norway%>%
  mutate(SEX=as.factor(SEX))%>%
  filter(educ_yrs_quintile%in%c(1,4))%>%
  mutate(category_four_indicator=ifelse(educ_yrs_quintile==4,1,0))

data_mediation_Norway_educ_3_1=data_Norway%>%
  mutate(SEX=as.factor(SEX))%>%
  filter(educ_yrs_quintile%in%c(1,3))%>%
  mutate(category_three_indicator=ifelse(educ_yrs_quintile==3,1,0))

data_mediation_Norway_sex=data_Norway%>%
  mutate(SEX=ifelse(SEX=='F',1,0))

## define functions applying medoutcon to separately estimate NDE, NIE, and proportion mediated for high SES indicator

tmle_de_educ_function_Norway <- function(data,educ_var) {
  tmle_de <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data[,educ_var],
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_educ_function_Norway <- function(data,educ_var) {
  tmle_ie <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data[,educ_var],
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_educ_function_Norway <- function(data,educ_var) {
  tmle_pm <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data[,educ_var],
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

## define functions applying medoutcon to separately estimate NDE, NIE, and proportion mediated for female sex indicator

tmle_de_sex_function_Norway <- function(data) {
  tmle_de <- medoutcon(W = data[,c("educ_yrs_quintile")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_sex_function_Norway <- function(data) {
  tmle_ie <- medoutcon(W = data[,c("educ_yrs_quintile")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_sex_function_Norway <- function(data) {
  tmle_pm <- medoutcon(W = data[,c("educ_yrs_quintile")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                    "DIABETES_MELLITUS_TYPE_1",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "SMOKING","is_vaccinated")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

## run all functions and store results
set.seed(123)
all_output_Norway[1,]<-c(tmle_de_educ_function_Norway(data_mediation_Norway_educ_binary,"high_educ"),"de_educ_binary")
set.seed(123)
all_output_Norway[2,]<-c(tmle_ie_educ_function_Norway(data_mediation_Norway_educ_binary,"high_educ"),"ie_educ_binary")
set.seed(123)
all_output_Norway[3,]<-c(tmle_pm_educ_function_Norway(data_mediation_Norway_educ_binary,"high_educ"),"pm_educ_binary")
set.seed(123)
all_output_Norway[4,]<-c(tmle_de_educ_function_Norway(data_mediation_Norway_educ_5_1,"category_five_indicator"),"de_educ_5_1")
set.seed(123)
all_output_Norway[5,]<-c(tmle_ie_educ_function_Norway(data_mediation_Norway_educ_5_1,"category_five_indicator"),"ie_educ_5_1")
set.seed(123)
all_output_Norway[6,]<-c(tmle_pm_educ_function_Norway(data_mediation_Norway_educ_5_1,"category_five_indicator"),"pm_educ_5_1")
set.seed(123)
all_output_Norway[7,]<-c(tmle_de_educ_function_Norway(data_mediation_Norway_educ_4_1,"category_four_indicator"),"de_educ_4_1")
set.seed(123)
all_output_Norway[8,]<-c(tmle_ie_educ_function_Norway(data_mediation_Norway_educ_4_1,"category_four_indicator"),"ie_educ_4_1")
set.seed(123)
all_output_Norway[9,]<-c(tmle_pm_educ_function_Norway(data_mediation_Norway_educ_4_1,"category_four_indicator"),"pm_educ_4_1")
set.seed(123)
all_output_Norway[10,]<-c(tmle_de_educ_function_Norway(data_mediation_Norway_educ_3_1,"category_three_indicator"),"de_educ_3_1")
set.seed(123)
all_output_Norway[11,]<-c(tmle_ie_educ_function_Norway(data_mediation_Norway_educ_3_1,"category_three_indicator"),"ie_educ_3_1")
set.seed(123)
all_output_Norway[12,]<-c(tmle_pm_educ_function_Norway(data_mediation_Norway_educ_3_1,"category_three_indicator"),"pm_educ_3_1")
set.seed(123)
all_output_Norway[13,]<-c(tmle_de_sex_function_Norway(data_mediation_Norway_sex),"de_sex")
set.seed(123)
all_output_Norway[14,]<-c(tmle_ie_sex_function_Norway(data_mediation_Norway_sex),"ie_sex")
set.seed(123)
all_output_Norway[15,]<-c(tmle_pm_sex_function_Norway(data_mediation_Norway_sex),"pm_sex")

write.csv(all_output_Norway,"./output_files/all_mediation_output_Norway.csv",row.names=F)

# UK

all_output_UK = data.frame(matrix(nrow=9,ncol=4))
colnames(all_output_UK)=c("LL","est","UL","effect")

data_mediation_UK_emp=data_UK%>%
  mutate(SEX=as.factor(SEX))%>%
  filter(employment_status_category!='Retired')%>%
  mutate(employment_status_binary=ifelse(employment_status_category=='Full-time employment',1,0))

data_mediation_UK_emp2=data_UK%>%
  mutate(SEX=as.factor(SEX))%>%
  filter(employment_status_category%in%c('Full-time employment',
                                         'Unemployed'))%>%
  mutate(employment_status_binary=ifelse(employment_status_category=='Full-time employment',1,0))

data_mediation_UK_sex=data_UK%>%
  mutate(SEX=ifelse(SEX=='F',1,0))%>%
  filter(employment_status_category!='Retired')%>%
  mutate(employment_status_binary=ifelse(employment_status_category=='Full-time employment',1,0))#%>%

## define functions applying medoutcon to separately estimate NDE, NIE, and proportion mediated for high SES indicator

tmle_de_emp_function_UK <- function(data) {
  tmle_de <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_emp_function_UK <- function(data) {
  tmle_ie <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_emp_function_UK <- function(data) {
  tmle_pm <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

## define functions applying medoutcon to separately estimate NDE, NIE, and proportion mediated for female sex indicator

tmle_de_sex_function_UK <- function(data) {
  tmle_de <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_sex_function_UK <- function(data) {
  tmle_ie <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_sex_function_UK <- function(data) {
  tmle_pm <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "MENTAL_DISORDER",
                                   "ISCHAEMIC_HEART_DISEASE",
                                   "SMOKING","OBESITY","is_treated_antiviral")],
                       Y = data$QALD_final_weighted,
                       # for population correction sensitivity analysis, use the below svy_weights argument
                       # svy_weights = data$weight,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

## run all functions and store results
set.seed(123)
all_output_UK[1,]<-c(tmle_de_emp_function_UK(data_mediation_UK_emp),"de_emp_full_time_vs_all")
set.seed(123)
all_output_UK[2,]<-c(tmle_ie_emp_function_UK(data_mediation_UK_emp),"ie_emp_full_time_vs_all")
set.seed(123)
all_output_UK[3,]<-c(tmle_pm_emp_function_UK(data_mediation_UK_emp),"pm_emp_full_time_vs_all")
set.seed(123)
all_output_UK[4,]<-c(tmle_de_emp_function_UK(data_mediation_UK_emp2),"de_emp_full_time_vs_unemp")
set.seed(123)
all_output_UK[5,]<-c(tmle_ie_emp_function_UK(data_mediation_UK_emp2),"ie_emp_full_time_vs_unemp")
set.seed(123)
all_output_UK[6,]<-c(tmle_pm_emp_function_UK(data_mediation_UK_emp2),"pm_emp_full_time_vs_unemp")
set.seed(123)
all_output_UK[7,]<-c(tmle_de_sex_function_UK(data_mediation_UK_sex),"de_sex")
set.seed(123)
all_output_UK[8,]<-c(tmle_ie_sex_function_UK(data_mediation_UK_sex),"ie_sex")
set.seed(123)
all_output_UK[9,]<-c(tmle_pm_sex_function_UK(data_mediation_UK_sex),"pm_sex")

write.csv(all_output_UK,"./output_files/all_mediation_output_UK.csv",row.names=F)

# RUSSIA

all_output_Russia = data.frame(matrix(nrow=,ncol=4))
colnames(all_output_Russia)=c("LL","est","UL","effect")

data_mediation_Russia_emp=data_Russia%>%
  mutate(SEX=as.factor(SEX))%>%
  mutate(employment_status_binary=ifelse(employment_status_category=='Full-time employment',1,0))

data_mediation_Russia_sex=data_Russia%>%
  mutate(SEX=ifelse(SEX=='F',1,0))%>%
  mutate(employment_status_binary=ifelse(employment_status_category=='Full-time employment',1,0))

tmle_de_emp_function_Russia <- function(data) {
  tmle_de <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_emp_function_Russia <- function(data) {
  tmle_ie <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M =data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                  "HYPERTENSION",
                                  "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                  "DIABETES_MELLITUS_TYPE_2",
                                  "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                  "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_emp_function_Russia <- function(data) {
  tmle_pm <- medoutcon(W = data[,c("AGE","SEX")],
                       A = data$employment_status_binary,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

tmle_de_sex_function_Russia <- function(data) {
  tmle_de <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "direct",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_de)[1]),as.numeric(summary(tmle_de)[2]),as.numeric(summary(tmle_de)[3])))
}

tmle_ie_sex_function_Russia <- function(data) {
  tmle_ie <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "indirect",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_ie)[1]),as.numeric(summary(tmle_ie)[2]),as.numeric(summary(tmle_ie)[3])))
}

tmle_pm_sex_function_Russia <- function(data) {
  tmle_pm <- medoutcon(W = data[,c("employment_status_binary")],
                       A = data$SEX,
                       Z = NULL,
                       M = data[,c("ASTHMA","CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
                                   "HYPERTENSION",
                                   "CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
                                   "DIABETES_MELLITUS_TYPE_2",
                                   "DIABETES_MELLITUS_TYPE_NOT_SPECIFIED",
                                   "SMOKING","OBESITY")],
                       Y = data$QALD_final_weighted,
                       g_learners = sl_lrnr,
                       h_learners = sl_lrnr,
                       b_learners = sl_lrnr2,
                       effect = "pm",
                       estimator = "tmle")
  return(c(as.numeric(summary(tmle_pm)[1]),as.numeric(summary(tmle_pm)[2]),as.numeric(summary(tmle_pm)[3])))
}

set.seed(123)
all_output_Russia[1,]<-c(tmle_de_emp_function_Russia(data_mediation_Russia_emp),"de_emp_full_time_vs_all")
set.seed(123)
all_output_Russia[2,]<-c(tmle_ie_emp_function_Russia(data_mediation_Russia_emp),"ie_emp_full_time_vs_all")
set.seed(123)
all_output_Russia[3,]<-c(tmle_pm_emp_function_Russia(data_mediation_Russia_emp),"pm_emp_full_time_vs_all")
set.seed(123)
all_output_Russia[4,]<-c(tmle_de_sex_function_Russia(data_mediation_Russia_sex),"de_sex")
set.seed(123)
all_output_Russia[5,]<-c(tmle_ie_sex_function_Russia(data_mediation_Russia_sex),"ie_sex")
set.seed(123)
all_output_Russia[6,]<-c(tmle_pm_sex_function_Russia(data_mediation_Russia_sex),"pm_sex")

write.csv(all_output_Russia,"./output_files/all_mediation_output_Russia.csv",row.names=F)



