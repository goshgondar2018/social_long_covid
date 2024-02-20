library(tidyverse)

data_combined_cohort=read.csv("./data/QALDs_full_dataset_combined_cohort.csv") #QALDs_full_dataset_combined_cohort_original.csv
data_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
data_UK=read.csv("./data/QALDs_full_dataset_UK.csv") 

# COMBINED MIC COHORT

## distribution of different employment status categories 
employment_status_distribution_combined_cohort=data_combined_cohort%>%
  group_by(employment_status_category)%>%
  summarise(n=n())%>%
  mutate(prop=n/sum(n))

ggplot(employment_status_distribution_combined_cohort,aes(x=employment_status_category,y=prop,fill=employment_status_category))+
  geom_bar(stat='identity')+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10))+
  geom_text(aes(label = n), vjust = -0.2)+
  theme(legend.position='none')+xlab("employment status (grouped)")

ggsave("./figures/descriptive/employment_status_category_distribution_combined_cohort.pdf")

## distribution of long COVID QALDs by educational attainment categories 
QALD_employment_status_distribution_combined_cohort=data_combined_cohort%>%
  group_by(employment_status_category)%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))

## distribution of long COVID QALDs by sex
QALD_sex_distribution_combined_cohort=data_combined_cohort%>%
  group_by(SEX)%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))

## statistical comparison of long COVID QALDs by sex
percent_diff_QALDs_sex_combined_cohort=(QALD_sex_distribution_combined_cohort$mean_QALD_final_weighted[QALD_sex_distribution_combined_cohort$SEX=='M']-QALD_sex_distribution_combined_cohort$mean_QALD_final_weighted[QALD_sex_distribution_combined_cohort$SEX=='F'])/(QALD_sex_distribution_combined_cohort$mean_QALD_final_weighted[QALD_sex_distribution_combined_cohort$SEX=='F'])*100

model_significance_sex_combined_cohort<-wilcox.test(QALD_final_weighted ~ SEX, data_combined_cohort, alternative = "two.sided")
model_significance_sex_combined_cohort

# NORWAY

prop_vaccinated=table(data_Norway$is_vaccinated)[[2]]/sum(table(data_Norway$is_vaccinated))

## distribution of long COVID QALDs by educational attainment quintile 
QALD_educ_distribution_Norway=data_Norway%>%
  mutate(educ_yrs_quintile=ifelse(is.na(educ_yrs_quintile)==TRUE,'UNKNOWN',educ_yrs_quintile))%>%
  group_by(educ_yrs_quintile)%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))%>%
  mutate(educ_yrs_quintile=as.factor(educ_yrs_quintile))

## distribution of long COVID QALDs by sex
QALD_sex_distribution_Norway=data_Norway%>%
  group_by(SEX)%>%
  filter(SEX!='U')%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))

## statistical comparison of long COVID QALDs by sex
percent_diff_QALDs_sex_Norway=(QALD_sex_distribution_Norway$mean_QALD_final_weighted[QALD_sex_distribution_Norway$SEX=='M']-QALD_sex_distribution_Norway$mean_QALD_final_weighted[QALD_sex_distribution_Norway$SEX=='F'])/(QALD_sex_distribution_Norway$mean_QALD_final_weighted[QALD_sex_distribution_Norway$SEX=='F'])*100

data_Norway_no_unknown=data_Norway%>%
  filter(SEX!='U')

model_significance_sex_Norway<-wilcox.test(QALD_final_weighted ~ SEX, data_Norway_no_unknown, exact=FALSE)

# UK

## distribution of different employment status categories 
employment_status_distribution_UK=data_UK%>%
  group_by(employment_status_category)%>%
  summarise(n=n())%>%
  mutate(prop=n/sum(n))

## distribution of long COVID QALDs by educational attainment category
QALD_employment_status_distribution_UK=data_UK%>%
  group_by(employment_status_category)%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))

## distribution of long COVID QALDs by sex
QALD_sex_distribution_UK=data_UK%>%
  group_by(SEX)%>%
  summarise(mean_QALD_final_weighted=mean(QALD_final_weighted,na.rm=TRUE))

## statistical comparison of long COVID QALDs by sex
percent_diff_QALDs_sex=(QALD_sex_distribution_UK$mean_QALD_final_weighted[QALD_sex_distribution_UK$SEX=='M']-QALD_sex_distribution_UK$mean_QALD_final_weighted[QALD_sex_distribution_UK$SEX=='F'])/(QALD_sex_distribution_UK$mean_QALD_final_weighted[QALD_sex_distribution_UK$SEX=='F'])*100




