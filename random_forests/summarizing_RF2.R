library(tidyverse)

# MAIN ANALYSES

## Norway
list_selected_groups_Norway=read.csv("./output/random_forests/list_selected_groups_Norway.csv")
colnames(list_selected_groups_Norway)<-"selected cluster"
hist(list_selected_groups_Norway$`selected cluster`,
     main="Histogram of selected clusters (Norway)",
     xlab="cluster number")

list_selected_vars_Norway=read.csv("./output/random_forests/list_selected_vars_Norway.csv")
colnames(list_selected_vars_Norway)<-"variable"
list_selected_vars_Norway_df=cbind.data.frame(variable=names(table(list_selected_vars_Norway)),
                                                           frequency=as.vector(table(list_selected_vars_Norway)))
list_selected_vars_Norway_df$variable=c("AGE","ASTHMA",
                                         "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                         "CHRONIC HAEMATOLOGICAL DISEASE",
                                         "CHRONIC KIDNEY DISEASE","CHRONIC NEUROLOGICAL DISORDER",
                                        "CHRONIC PULMONARY DISEASE (NOT ASTHMA)","DIABETES MELLITUS (TYPE 1)",
                                        "DIABETES MELLITUS (TYPE 2)", "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                        "EDUCATIONAL ATTAINMENT (YEARS)", 
                                        "EDUCATIONAL ATTAINMENT_quintile2", 
                                        "EDUCATIONAL ATTAINMENT_quintile3", 
                                        "EDUCATIONAL ATTAINMENT_quintile4", 
                                        "EDUCATIONAL ATTAINMENT_quintile5", 
                                        "HYPERTENSION",
                                        "VACCINATION STATUS", 
                                        "LIVER DISEASE (SEVERITY NOT SPECIFIED)",
                                        "MALIGNANT NEOPLASM","PSYCHOLOGICAL DISORDER",
                                        "OBESITY","OTHER","RHEUMATOLOGICAL DISORDER",
                                        "SEX","SMOKING")

ggplot(list_selected_vars_Norway_df,aes(x=reorder(variable,frequency,decreasing=TRUE),
                                                     y=frequency,fill=variable))+
  geom_bar(stat='identity')+theme_classic()+
  guides(fill = guide_legend(nrow = 6))+
  xlab("variable")+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=8),
        legend.text=element_text(size=5),
        legend.key.size = unit(0.5, 'cm'),
        legend.title=element_blank(),
        legend.position='bottom')

ggsave("./figures/prediction/frequency_selected_vars_RF2_Norway.pdf")

## UK
list_selected_groups_UK=read.csv("./output/random_forests/list_selected_groups_UK.csv")
colnames(list_selected_groups_UK)<-"selected cluster"
hist(list_selected_groups_UK$`selected cluster`,
     main="Histogram of selected clusters (combined cohort)",
     xlab="cluster number")

list_selected_vars_UK=read.csv("./output/random_forests/list_selected_vars_UK.csv")
colnames(list_selected_vars_UK)<-"variable"
list_selected_vars_UK_df=cbind.data.frame(variable=names(table(list_selected_vars_UK)),
                                             frequency=as.vector(table(list_selected_vars_UK)))
list_selected_vars_UK_df$variable=c("AGE","ATRIAL FIBRILLATION",
                                    "BRONCIECTASIS", 
                                    "CHRONIC NEUROLOGICAL DISORDER",
                                    "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                    "TYPE 1 DIABETES", "TYPE 2 DIABETES",
                                    "DIABETES (TYPE NOT SPECIFIED)",
                                    "EMPLOYMENT STATUS",
                                    "EMPLOYMENT STATUS CATEGORY",
                                    "GI DISEASE",
                                    "HYPOTHYROIDISM",
                                    "ANTIVIRAL TREATMENT",
                                    "ISCHAEMIC HEART DISEASE",
                                    "PSYCHOLOGICAL DISORDER", "MI",
                                    "OBESITY", "OTHER",
                                    "RHEUMATOLOGICAL DISORDER",
                                    "SEVERITY INDICATOR",
                                    "SEX","SMOKING")

ggplot(list_selected_vars_UK_df,aes(x=reorder(variable,frequency,decreasing=TRUE),
                                       y=frequency,fill=variable))+
  geom_bar(stat='identity')+theme_classic()+theme(legend.title=element_blank(),
                                                  legend.text=element_text(size=8),
                                                  legend.key.size = unit(0.5, 'cm'),
                                                  legend.position='bottom')+
  guides(fill = guide_legend(nrow = 8))+
  xlab("variable")+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=7))

ggsave("./figures/prediction/frequency_selected_vars_RF2_UK.pdf")

## combined_cohort
list_selected_groups_combined_cohort=read.csv("./output/random_forests/list_selected_groups_combined_cohort.csv")
colnames(list_selected_groups_combined_cohort)<-"selected cluster"
hist(list_selected_groups_combined_cohort$`selected cluster`,
     main="Histogram of selected clusters (combined cohort)",
     xlab="cluster number")

list_selected_vars_combined_cohort=read.csv("./output/random_forests/list_selected_vars_combined_cohort.csv")
colnames(list_selected_vars_combined_cohort)<-"variable"
list_selected_vars_combined_cohort_df=cbind.data.frame(variable=names(table(list_selected_vars_combined_cohort)),
                                                       frequency=as.vector(table(list_selected_vars_combined_cohort)))
list_selected_vars_combined_cohort_df$variable=c("AGE","ASTHMA","CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                                 "CHRONIC HEMATOLOGICAL DISEASE",
                                                 "CHRONIC KIDNEY DISEASE","CHRONIC NEUROLOGICAL DISORDER",
                                                 "CHRONIC PULMONARY DISEASE (NOT ASTHMA)","COUNTRY","DEMENTIA",
                                                 "DIABETES MELLITUS (TYPE 1)",
                                                 "DIABETES MELLITUS (TYPE 2)", "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                                 "EMPLOYMENT STATUS","EMPLOYMENT STATUS CATEGORY","HIV",
                                                 "HYPERTENSION","MALIGNANT NEOPLASM","PSYCHOLOGICAL DISORDER","MILD LIVER DISEASE",
                                                 "OBESITY","OTHER","RHEUMATOLOGICAL DISORDER","SEVERITY INDICATOR","SEX","SMOKING")
ggplot(list_selected_vars_combined_cohort_df,aes(x=reorder(variable,frequency,decreasing=TRUE),
                                                 y=frequency,fill=variable))+
  geom_bar(stat='identity')+theme_classic()+theme(legend.title=element_blank(),
                                                  legend.text=element_text(size=6),
                                                  legend.key.size = unit(0.5, 'cm'),
                                                  legend.position='bottom')+
  guides(fill = guide_legend(nrow = 6))+
  xlab("variable")+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=8))

ggsave("./figures/prediction/frequency_selected_vars_RF2_combined_cohort.pdf")

# SENSITIVITY ANALYSES

## Norway
list_selected_groups_Norway=read.csv("./output/random_forests/list_selected_groups_Norway_sensitivity.csv")
colnames(list_selected_groups_Norway)<-"selected cluster"
hist(list_selected_groups_Norway$`selected cluster`,
     main="Histogram of selected clusters (Norway)",
     xlab="cluster number")

list_selected_vars_Norway=read.csv("./output/random_forests/list_selected_vars_Norway_sensitivity.csv")
colnames(list_selected_vars_Norway)<-"variable"
list_selected_vars_Norway_df=cbind.data.frame(variable=names(table(list_selected_vars_Norway)),
                                              frequency=as.vector(table(list_selected_vars_Norway)))
list_selected_vars_Norway_df$variable=c("AGE","ASTHMA",
                                        "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                        "CHRONIC HAEMATOLOGICAL DISEASE",
                                        "CHRONIC KIDNEY DISEASE",
                                        "CHRONIC NEUROLOGICAL DISORDER",
                                        "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                        "DIABETES MELLITUS (TYPE 1)",
                                        "DIABETES MELLITUS (TYPE 2)", 
                                        "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                        "EDUCATIONAL ATTAINMENT (YEARS)", 
                                        "EDUCATIONAL ATTAINMENT QUINTILES (YEARS)",
                                        "HYPERTENSION",
                                        "VACCINATION STATUS", "LIVER DISEASE (SEVERITY NOT SPECIFIED)",
                                        "MALIGNANT NEOPLASM","PSYCHOLOGICAL DISORDER",
                                        "OBESITY",
                                        "OTHER","RHEUMATOLOGICAL DISORDER","SEX","SMOKING")

ggplot(list_selected_vars_Norway_df,aes(x=reorder(variable,frequency,decreasing=TRUE),
                                        y=frequency,fill=variable))+
  geom_bar(stat='identity')+theme_classic()+
  guides(fill = guide_legend(nrow = 6))+
  xlab("variable")+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=6),
        legend.text=element_text(size=5),
        legend.key.size = unit(0.5, 'cm'),
        legend.title=element_blank(),
        legend.position='bottom')

ggsave("./figures/prediction/frequency_selected_vars_RF2_Norway_sensitivity.pdf")

## UK
list_selected_groups_UK=read.csv("./output/random_forests/list_selected_groups_UK_sensitivity.csv")
colnames(list_selected_groups_UK)<-"selected cluster"
hist(list_selected_groups_UK$`selected cluster`,
     main="Histogram of selected clusters (combined cohort)",
     xlab="cluster number")

list_selected_vars_UK=read.csv("./output/random_forests/list_selected_vars_UK_sensitivity.csv")
colnames(list_selected_vars_UK)<-"variable"
list_selected_vars_UK_df=cbind.data.frame(variable=names(table(list_selected_vars_UK)),
                                          frequency=as.vector(table(list_selected_vars_UK)))
list_selected_vars_UK_df$variable=c("AGE","ASTHMA","ATRIAL FIBRILLATION",
                                    "CHRONIC KIDNEY DISEASE",
                                    "CHRONIC NEUROLOGICAL DISORDER",
                                    "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                    "TYPE 1 DIABETES", "TYPE 2 DIABETES",
                                    "DIABETES (TYPE NOT SPECIFIED)", "EMPLOYMENT STATUS",
                                    "EMPLOYMENT STATUS CATEGORY",
                                    "GI DISEASE", "HYPERTENSION",
                                    "ANTIVIRAL TREATMENT INDICATOR",
                                    "ISCHAEMIC HEART DISEASE",
                                    "PSYCHOLOGICAL DISORDER", "MI", "OBESITY", "OTHER",
                                    "RHEUMATOLOGICAL DISORDER","SEVERITY INDICATOR",
                                    "SEX", "SMOKING")

ggplot(list_selected_vars_UK_df,aes(x=reorder(variable,frequency,decreasing=TRUE),
                                    y=frequency,fill=variable))+
  geom_bar(stat='identity')+theme_classic()+theme(legend.title=element_blank(),
                                                  legend.text=element_text(size=5),
                                                  legend.key.size = unit(0.5, 'cm'),
                                                  legend.position='bottom')+
  guides(fill = guide_legend(nrow = 8))+
  xlab("variable")+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=4.5))

ggsave("./figures/prediction/frequency_selected_vars_RF2_UK_sensitivity.pdf")



