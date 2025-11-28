library(tidyverse)

data_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
data_UK=read.csv("./data/QALDs_full_dataset_UK.csv") 
data_Russia=read.csv("./data/QALDs_full_dataset_Russia.csv") 

# NORWAY

prop_vaccinated=table(data_Norway$is_vaccinated)[[2]]/sum(table(data_Norway$is_vaccinated))

## distribution of long COVID QALDs by educational attainment quintile 
# distribution of mean long COVID QALDs by educational attainment quintile 
QALD_educ_distribution_Norway=data_Norway%>%
  mutate(educ_yrs_quintile=ifelse(is.na(educ_yrs_quintile)==TRUE,'UNKNOWN',educ_yrs_quintile))%>%
  group_by(educ_yrs_quintile)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))%>%
  mutate(educ_yrs_quintile=as.factor(educ_yrs_quintile))

ggplot(QALD_educ_distribution_Norway,aes(x=educ_yrs_quintile,y=mean_QALD,fill=educ_yrs_quintile))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("quintile of # of years of education (years)")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_educational_attainment_Norway.pdf")

QALD_educ_distribution_Norway_full=data_Norway%>%
  mutate(educ_yrs_quintile=ifelse(is.na(educ_yrs_quintile)==TRUE,'UNKNOWN',educ_yrs_quintile))
  
pairwise.t.test(QALD_educ_distribution_Norway_full$QALD,
                QALD_educ_distribution_Norway_full$educ_yrs_quintile,
                p.adjust.method = "bonferroni")

# distribution of mean long COVID QALDs by sex
QALD_sex_distribution_Norway=data_Norway%>%
  group_by(SEX)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_Norway,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_Norway.pdf")

percent_diff_QALDs_sex_Norway=(QALD_sex_distribution_Norway$mean_QALD[QALD_sex_distribution_Norway$SEX=='M']-QALD_sex_distribution_Norway$mean_QALD[QALD_sex_distribution_Norway$SEX=='F'])/(QALD_sex_distribution_Norway$mean_QALD[QALD_sex_distribution_Norway$SEX=='F'])*100

model_significance_sex_Norway<-wilcox.test(QALD ~ SEX, 
                                           data_Norway, 
                                           alternative = "less",
                                           conf.int=TRUE,
                                           exact=FALSE)
model_significance_sex_Norway

## BEFORE ILLNESS COMPARISONdata_Norway_b4=read.csv("./data/QALDs_full_dataset_Norway_b4.csv")
QALD_educ_distribution_Norway_b4=data_Norway_b4%>%
  mutate(educ_yrs_quintile=ifelse(is.na(educ_yrs_quintile)==TRUE,'UNKNOWN',educ_yrs_quintile))%>%
  group_by(educ_yrs_quintile)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))%>%
  mutate(educ_yrs_quintile=as.factor(educ_yrs_quintile))

ggplot(QALD_educ_distribution_Norway_b4,aes(x=educ_yrs_quintile,y=mean_QALD,fill=educ_yrs_quintile))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("quintile of # of years of education (years)")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_educational_attainment_Norway_b4.pdf")

# distribution of mean long COVID QALDs by sex
QALD_sex_distribution_Norway_b4=data_Norway_b4%>%
  group_by(SEX)
summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_Norway_b4,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_Norway_b4.pdf")

percent_diff_QALDs_sex_Norway_b4=(QALD_sex_distribution_Norway_b4$mean_QALD[QALD_sex_distribution_Norway_b4$SEX=='M']-QALD_sex_distribution_Norway_b4$mean_QALD[QALD_sex_distribution_Norway_b4$SEX=='F'])/(QALD_sex_distribution_Norway_b4$mean_QALD[QALD_sex_distribution_Norway_b4$SEX=='F'])*100

model_significance_sex_Norway<-wilcox.test(QALD ~ SEX, data_Norway_b4, exact=FALSE)


# UK

QALD_sex_distribution_UK=data_UK%>%
  group_by(SEX)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_UK,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_UK.pdf")

percent_diff_QALDs_sex=(QALD_sex_distribution_UK$mean_QALD[QALD_sex_distribution_UK$SEX=='M']-QALD_sex_distribution_UK$mean_QALD[QALD_sex_distribution_UK$SEX=='F'])/(QALD_sex_distribution_UK$mean_QALD[QALD_sex_distribution_UK$SEX=='F'])*100

model_significance_sex_UK<-wilcox.test(QALD ~ SEX, 
                                       data_UK, 
                                       alternative = "less",
                                       conf.int=TRUE,
                                       exact=FALSE)
model_significance_sex_UK
#### BEFORE ILLNESS COMPARISON
data_UK_b4=read.csv("./data/QALDs_full_dataset_UK_b4.csv")

employment_status_distribution_UK_b4=data_UK_b4%>%
  group_by(employment_status_category)%>%
  summarise(n=n())%>%
  mutate(prop=n/sum(n))

ggplot(employment_status_distribution_UK_b4,aes(x=employment_status_category,y=prop,fill=employment_status_category))+
  geom_bar(stat='identity')+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10))+
  geom_text(aes(label = n), vjust = -0.2)+
  theme(legend.position='none')+xlab("employment status (grouped)")

ggsave("./figures/descriptive/employment_status_category_distribution_UK_b4.pdf")

# distribution of Mean Long COVID QALDses by educational attainment categories (focus on subjects w Mean Long COVID QALDses)
QALD_employment_status_distribution_UK_b4=data_UK_b4%>%
  group_by(employment_status_category)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_employment_status_distribution_UK_b4,aes(x=reorder(employment_status_category,mean_QALD),y=mean_QALD,fill=employment_status_category))+
  geom_bar(stat='identity')+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10))+
  theme(legend.position='none')+xlab("employment status (grouped)")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_employment_status_category_UK_b4.pdf")

# distribution of mean long COVID QALDs by sex
QALD_sex_distribution_UK_b4=data_UK_b4%>%
  group_by(SEX)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_UK_b4,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_UK_b4.pdf")

percent_diff_QALDs_sex_b4=(QALD_sex_distribution_UK_b4$mean_QALD[QALD_sex_distribution_UK_b4$SEX=='M']-QALD_sex_distribution_UK_b4$mean_QALD[QALD_sex_distribution_UK_b4$SEX=='F'])/(QALD_sex_distribution_UK_b4$mean_QALD[QALD_sex_distribution_UK_b4$SEX=='F'])*100

# Russia
QALD_sex_distribution_Russia=data_Russia%>%
  group_by(SEX)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_Russia,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_Russia.pdf")

percent_diff_QALDs_sex_Russia=(QALD_sex_distribution_Russia$mean_QALD[QALD_sex_distribution_Russia$SEX=='M']-QALD_sex_distribution_Russia$mean_QALD[QALD_sex_distribution_Russia$SEX=='F'])/(QALD_sex_distribution_Russia$mean_QALD[QALD_sex_distribution_Russia$SEX=='F'])*100

model_significance_sex_Russia<-wilcox.test(QALD ~ SEX, 
                                           data_Russia, 
                                           alternative = "less",
                                           conf.int=TRUE,
                                           exact=FALSE)
model_significance_sex_Russia

#### BEFORE ILLNESS COMPARISON

data_Russia_b4=read.csv("./data/QALDs_full_dataset_Russia_b4.csv") #QALDs_full_dataset_Russia_original.csv

# distribution of different employment status categories 
employment_status_distribution_Russia_b4=data_Russia_b4%>%
  group_by(employment_status_category)%>%
  summarise(n=n())%>%
  mutate(prop=n/sum(n))

ggplot(employment_status_distribution_Russia_b4,aes(x=employment_status_category,y=prop,fill=employment_status_category))+
  geom_bar(stat='identity')+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10))+
  geom_text(aes(label = n), vjust = -0.2)+
  theme(legend.position='none')+xlab("employment status (grouped)")

ggsave("./figures/descriptive/employment_status_category_distribution_Russia_b4.pdf")


# distribution of Mean Long COVID QALDses by employment status categories 
QALD_employment_status_distribution_Russia_b4=data_Russia_b4%>%
  group_by(employment_status_category)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_employment_status_distribution_Russia_b4,aes(x=reorder(employment_status_category,mean_QALD),y=mean_QALD,fill=employment_status_category))+
  geom_bar(stat='identity')+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10))+
  theme(legend.position='none')+xlab("employment status (grouped)")+
  ylab("Mean Long COVID QALDs")

QALD_sex_distribution_Russia_b4=data_Russia_b4%>%
  group_by(SEX)%>%
  summarise(mean_QALD=mean(QALD,na.rm=TRUE))

ggplot(QALD_sex_distribution_Russia_b4,aes(x=SEX,y=mean_QALD,fill=SEX))+
  geom_bar(stat='identity')+theme_classic()+
  theme(legend.position='none')+xlab("SEX")+
  ylab("Mean Long COVID QALDs")

ggsave("./figures/descriptive/QALDs_sex_Russia_b4.pdf")




