library(tidyverse)

QALD_data=read.csv("./data/QALD_data.csv")
demographic_data=read.csv("./data/demographic_data.csv")
eq5d_data=read.csv("./data/eq5d_data.csv")

df_weights_Norway=read.csv("./data/df_weights_Norway.csv")
df_weights_UK=read.csv("./data/df_weights_UK.csv")
df_weights_Russia=read.csv("./data/df_weights_Russia.csv")

data_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
data_Norway_IDs=unique(data_Norway$USUBJID)
data_UK=read.csv("./data/QALDs_full_dataset_UK.csv")
data_UK_IDs=unique(data_UK$USUBJID)
data_Russia=read.csv("./data/QALDs_full_dataset_Russia.csv")
data_Russia_IDs=unique(data_Russia$USUBJID)

# NORWAY

# format QALD data to EQ-5D-5L dimension by subject
QALD_data_weights_Norway_today_by_dimension=merge(QALD_data,
                              df_weights_Norway,by=c("CQTEST","CQSTRESC"))%>%
  mutate(CQSTRESC=as.numeric(CQSTRESC))%>%
  mutate(weight=as.numeric(weight))%>%
  filter(CQEVINTX=='TODAY')%>%
  group_by(USUBJID)%>%
  # subset to subjects w/ only 1 entry for each of the 5q-5d-5l categories
  filter(n()==5)%>%
  filter(USUBJID%in%data_Norway_IDs)

QALD_data_weights_Norway_by_dimension=QALD_data_weights_Norway_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

## merge QALD data by dimension w/ demographic data
QALDs_all_demographics_Norway_by_dimension=merge(QALD_data_weights_Norway_by_dimension,
                                                 data_Norway)%>%
  mutate(DIMENSION_NAME=ifelse(CQTEST=='EQ5D02-Anxiety/Depression','Anxiety/Depression',
                               ifelse(CQTEST=='EQ5D02-Mobility','Mobility',
                                      ifelse(CQTEST=='EQ5D02-Pain/Discomfort','Pain/Discomfort',
                                             ifelse(CQTEST=='EQ5D02-Self-Care','Self-Care',
                                                    'Usual Activities')))))%>%
  mutate(educ_yrs_quintile=as.factor(educ_yrs_quintile))

## plot summarizing EQ-5D-5L dimension by educ attainment quintile
ggplot(QALDs_all_demographics_Norway_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=educ_yrs_quintile))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="educational attainment quintile")+
  theme(legend.position='bottom')

ggsave("./figures/compare_eq5d5l_responses_educ_attainment_Norway.png")

## plot summarizing EQ-5D-5L dimension by sex
ggplot(QALDs_all_demographics_Norway_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=SEX))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=7))+
  labs(fill="sex")

ggsave("./figures/compare_eq5d5l_responses_sex_Norway.png")


# UK

QALD_data_weights_UK_today_by_dimension=merge(QALD_data,df_weights_UK,by=c("CQTEST","CQSTRESC"))%>%
  mutate(CQSTRESC=as.numeric(CQSTRESC))%>%
  mutate(weight=as.numeric(weight))%>%
  filter(CQEVINTX=='TODAY')%>%
  group_by(USUBJID)%>%
  filter(n()==5)%>%
  filter(USUBJID%in%data_UK_IDs)

QALD_data_weights_UK_by_dimension=QALD_data_weights_UK_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

QALDs_all_demographics_UK_by_dimension=merge(QALD_data_weights_UK_by_dimension,
                                             data_UK)%>%
  mutate(DIMENSION_NAME=ifelse(CQTEST=='EQ5D02-Anxiety/Depression','Anxiety/Depression',
                               ifelse(CQTEST=='EQ5D02-Mobility','Mobility',
                                      ifelse(CQTEST=='EQ5D02-Pain/Discomfort','Pain/Discomfort',
                                             ifelse(CQTEST=='EQ5D02-Self-Care','Self-Care',
                                                    'Usual Activities')))))
ggplot(QALDs_all_demographics_UK_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=employment_status_category))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="employment status category")

ggsave("./figures/compare_eq5d5l_responses_employment_status_UK.png")

ggplot(QALDs_all_demographics_UK_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=SEX))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="sex")

ggsave("./figures/compare_eq5d5l_responses_sex_UK.png")

# Russia

QALD_data_weights_Russia_today_by_dimension=merge(QALD_data,df_weights_Russia,by=c("CQTEST","CQSTRESC"))%>%
  mutate(CQSTRESC=as.numeric(CQSTRESC))%>%
  mutate(weight=as.numeric(weight))%>%
  filter(CQEVINTX=='TODAY')%>%
  group_by(USUBJID)%>%
  filter(n()==5)%>%
  filter(USUBJID%in%data_Russia_IDs)

QALD_data_weights_Russia_by_dimension=QALD_data_weights_Russia_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

QALDs_all_demographics_Russia_by_dimension=merge(QALD_data_weights_Russia_by_dimension,
                                                 data_Russia)%>%
  mutate(DIMENSION_NAME=ifelse(CQTEST=='EQ5D02-Anxiety/Depression','Anxiety/Depression',
                               ifelse(CQTEST=='EQ5D02-Mobility','Mobility',
                                      ifelse(CQTEST=='EQ5D02-Pain/Discomfort','Pain/Discomfort',
                                             ifelse(CQTEST=='EQ5D02-Self-Care','Self-Care',
                                                    'Usual Activities')))))

ggplot(QALDs_all_demographics_Russia_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=employment_status_category))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="employment status category")

ggsave("./figures/compare_eq5d5l_responses_employment_status_Russia.png")

ggplot(QALDs_all_demographics_Russia_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=SEX))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="sex")

ggsave("./figures/compare_eq5d5l_responses_sex_Russia.png")





ggsave("./figures/compare_eq5d5l_responses_sex_UK.png")

