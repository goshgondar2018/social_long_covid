library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(makedummies)

# COMBINED MIC COHORT
dataset_combined_cohort=read.csv("./data/QALDs_full_dataset_combined_cohort.csv")
comparator_group_emp='Full-time employment'
comparator_group_country='RUS'

dataset_combined_cohort_for_cor=dataset_combined_cohort%>%
  filter(SEX!='U')%>%
  mutate(SEX=ifelse(SEX=='F',1,0))%>%
  mutate(employment_status_category=as.factor(employment_status_category))%>%
  mutate(employment_status_category=relevel(employment_status_category, comparator_group_emp))%>%
  mutate(COUNTRY=as.factor(COUNTRY))%>%
  mutate(COUNTRY=relevel(COUNTRY, comparator_group_country))%>%
  dplyr::select(-employment_status)%>%
  mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
  dplyr::select(-MENTAL_DISORDER)

categorical_data_combined_cohort=as.data.frame(dataset_combined_cohort_for_cor[,c("employment_status_category","COUNTRY")])
dummy_vars_combined_cohort=makedummies(categorical_data_combined_cohort)

dataset_combined_cohort_for_cor_final=cbind.data.frame(dataset_combined_cohort_for_cor%>%dplyr::select(-employment_status_category)%>%dplyr::select(-COUNTRY),
                                                       dummy_vars_combined_cohort)

correlation_matrix_combined_cohort = cor(dataset_combined_cohort_for_cor_final)
ggcorrplot(correlation_matrix_combined_cohort ,tl.cex=8,type='upper',
           colors = c("blue", "azure1", "red1"))
ggsave("./figures/correlation_matrix_combined_cohort.png")


# NORWAY
dataset_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")

## computes Phi coefficient for binary variables and Point-Biserial Correlation for binary vs continuous variables
dataset_Norway_for_cor=dataset_Norway%>%
  mutate(SEX=ifelse(SEX=='F',1,0))%>%
  mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
  dplyr::select(-MENTAL_DISORDER)

correlation_matrix_Norway = cor(dataset_Norway_for_cor)

ggcorrplot(correlation_matrix_Norway,tl.cex=8,type='upper',
           colors = c("blue", "azure1", "red1"))
ggsave("./figures/correlation_matrix_Norway.png")

# UK
dataset_UK=read.csv("./data/QALDs_full_dataset_UK.csv")

dataset_UK_for_cor=dataset_UK%>%
  mutate(SEX=ifelse(SEX=='F',1,0))%>%
  mutate(employment_status_category=as.factor(employment_status_category))%>%
  dplyr::select(-employment_status)%>%
  mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
  dplyr::select(-MENTAL_DISORDER)

categorical_data_UK=as.data.frame(dataset_UK_for_cor[,"employment_status_category"])
colnames(categorical_data_UK)<-"employment_status_category"
dummy_vars_UK=makedummies(categorical_data_UK)

dataset_UK_for_cor_final=cbind.data.frame(dataset_UK_for_cor%>%dplyr::select(-employment_status_category),
                                          dummy_vars_UK)

correlation_matrix_UK = cor(dataset_UK_for_cor_final)
ggcorrplot(correlation_matrix_UK,tl.cex=8,type='upper',
           colors = c("blue", "azure1", "red1"))
ggsave("./figures/correlation_matrix_UK.png")

