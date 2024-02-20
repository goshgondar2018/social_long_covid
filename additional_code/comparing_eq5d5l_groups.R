QALD_data=read.csv("./data/QALD_data.csv")
demographic_data=read.csv("./data/demographic_data.csv")
eq5d_data=read.csv("./data/eq5d_data.csv")

df_weights_combined_cohort=read.csv("./data/df_weights_combined_cohort.csv")
df_weights_Norway=read.csv("./data/df_weights_Norway.csv")
df_weights_UK=read.csv("./data/df_weights_UK.csv")

# COMBINED MIC COHORT

QALD_data_weights_combined_cohort_today_by_dimension=merge(QALD_data,df_weights_combined_cohort,by=c("CQTEST","CQSTRESC"))%>%
  mutate(CQSTRESC=as.numeric(CQSTRESC))%>%
  mutate(weight=as.numeric(weight))%>%
  filter(CQEVINTX=='TODAY')%>%
  group_by(USUBJID)%>%
  # subset to subjects w/ only 1 entry for each of the 5q-5d-5l categories
  filter(n()==5)

QALD_data_weights_combined_cohort_by_dimension=QALD_data_weights_combined_cohort_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

## merge QALD data w/ demographic & emp data
demographic_data_combined_cohort= demographic_data %>%  filter(COUNTRY%in%c('BRA','COL','IND','ZAF','ISR','GIB'))
IDs_combined_cohort=unique(demographic_data_combined_cohort$USUBJID)

## merge this QALD data with demographic educ data
emp_data_combined_cohort=eq5d_data%>%
  filter(CQEVINTX=='BEFORE ACUTE COVID-19 ILLNESS')%>%
  filter(CQTEST=='CQ013-Employment Status')%>%
  dplyr::select(USUBJID,CQORRES)%>%
  filter(USUBJID%in%IDs_combined_cohort)

QALDs_emp_combined_cohort_by_dimension=merge(QALD_data_weights_combined_cohort_by_dimension,
                                             emp_data_combined_cohort)%>%
  mutate(employment_status=CQORRES)%>%
  mutate(employment_status_category=ifelse(employment_status%in%c('Medically Retired','Retired'),
                                           'Retired',
                                           ifelse(employment_status%in%c('Unemployed','Unable to Work Due to Chronic Illness'),
                                                  'Unemployed',
                                                  ifelse(employment_status=='Working Full-Time',
                                                         'Full-time employment',
                                                         ifelse(employment_status=='Carer',
                                                                'Carer',
                                                                ifelse(employment_status=='Working Part-Time',
                                                                       'Part-time employment', 'Student'))))))


QALDs_all_demographics_combined_cohort_by_dimension=merge(QALDs_emp_combined_cohort_by_dimension,
                                                          demographic_data_combined_cohort[,c("USUBJID","AGE","SEX")])%>%
  mutate(DIMENSION_NAME=ifelse(CQTEST=='EQ5D02-Anxiety/Depression','Anxiety/Depression',
                        ifelse(CQTEST=='EQ5D02-Mobility','Mobility',
                        ifelse(CQTEST=='EQ5D02-Pain/Discomfort','Pain/Discomfort',
                        ifelse(CQTEST=='EQ5D02-Self-Care','Self-Care',
                               'Usual Activities')))))

ggplot(QALDs_all_demographics_combined_cohort_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=employment_status_category))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="employment status category")

ggsave("./figures/compare_eq5d5l_responses_employment_status_combined_cohort.png")

ggplot(QALDs_all_demographics_combined_cohort_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=SEX))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="sex")

ggsave("./figures/compare_eq5d5l_responses_sex_combined_cohort.png")

# NORWAY

QALD_data_weights_Norway_today_by_dimension=merge(QALD_data,df_weights_Norway,by=c("CQTEST","CQSTRESC"))%>%
  mutate(CQSTRESC=as.numeric(CQSTRESC))%>%
  mutate(weight=as.numeric(weight))%>%
  filter(CQEVINTX=='TODAY')%>%
  group_by(USUBJID)%>%
  # subset to subjects w/ only 1 entry for each of the 5q-5d-5l categories
  filter(n()==5)

QALD_data_weights_Norway_by_dimension=QALD_data_weights_Norway_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

## merge QALD data w/ demographic & educ data
demographic_data_Norway= demographic_data %>% filter(COUNTRY=='NOR')
IDs_Norway=unique(demographic_data_Norway$USUBJID)

educ_data_Norway=eq5d_data%>%
  filter(CQTEST=='Number of Years of Education')%>% 
  dplyr::select(USUBJID,CQORRES)%>%
  filter(USUBJID%in%IDs_Norway)
  
# subset to today and generate quintiles for # of years of education
QALDs_educ_Norway_by_dimension=merge(QALD_data_weights_Norway_by_dimension,
                                          educ_data_Norway)%>%
  mutate(educ_yrs=as.numeric(CQORRES))

QALDs_educ_Norway_by_dimension$educ_yrs_quintile=ntile(QALDs_educ_Norway_by_dimension$educ_yrs,5)

QALDs_all_demographics_Norway_by_dimension=merge(QALDs_educ_Norway_by_dimension,
                                                 demographic_data_Norway[,c("USUBJID","AGE","SEX")])%>%
  mutate(educ_yrs_quintile=as.factor(educ_yrs_quintile))%>%
  mutate(DIMENSION_NAME=ifelse(CQTEST=='EQ5D02-Anxiety/Depression','Anxiety/Depression',
                               ifelse(CQTEST=='EQ5D02-Mobility','Mobility',
                                      ifelse(CQTEST=='EQ5D02-Pain/Discomfort','Pain/Discomfort',
                                             ifelse(CQTEST=='EQ5D02-Self-Care','Self-Care',
                                                    'Usual Activities')))))

ggplot(QALDs_all_demographics_Norway_by_dimension,
       aes(x=DIMENSION_NAME,y=CQSTRESC,fill=educ_yrs_quintile))+
  geom_boxplot()+
  xlab("EQ-5D-5L dimension")+ylab("EQ-5D-5L response")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10))+
  labs(fill="educational attainment quintile")+
  theme(legend.position='bottom')

ggsave("./figures/compare_eq5d5l_responses_educ_attainment_Norway.png")

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
  # subset to subjects w/ only 1 entry for each of the 5q-5d-5l categories
  filter(n()==5)

QALD_data_weights_UK_by_dimension=QALD_data_weights_UK_today_by_dimension%>%
  dplyr::select(USUBJID,CQTEST,CQSTRESC,weight)

## merge QALD data w/ demographic & emp data
demographic_data_UK= demographic_data %>% filter(COUNTRY=='GBR')
IDs_UK=unique(demographic_data_UK$USUBJID)

emp_data_UK=eq5d_data%>%
  filter(CQEVINTX=='BEFORE ACUTE COVID-19 ILLNESS')%>%
  filter(CQTEST=='CQ013-Employment Status')%>%
  dplyr::select(USUBJID,CQORRES)%>%
  filter(USUBJID%in%IDs_UK)

QALDs_emp_UK_by_dimension=merge(QALD_data_weights_UK_by_dimension,
                                emp_data_UK)%>%
  mutate(employment_status=CQORRES)%>%
  mutate(employment_status_category=ifelse(employment_status%in%c('Medically retired','Retired','Unable to work due to chronic illness_Retired'),
                                           'Retired',
                                           ifelse(employment_status%in%c('Unemployed','Unable to work due to chronic illness'),
                                                  'Unemployed',
                                                  ifelse(employment_status%in%c('Full-time employment','Carer','Full-time employment_Prefer not to say'),
                                                         'Full-time employment',
                                                         ifelse(employment_status%in%c('Furloughed','Full-time employment_Furloughed'),'Furloughed',
                                                                ifelse(employment_status=='Student', 'Student','Part-time employment'))))))
  
QALDs_all_demographics_UK_by_dimension=merge(QALDs_emp_UK_by_dimension,
                                    demographic_data_UK[,c("USUBJID","AGE","SEX")])%>%
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

