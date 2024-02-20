library(tidyverse)
library(PCAmixdata)
library(FactoMineR)
library(caret)
library(CoVVSURF)
library(randomForest)

# read in data and subset
dataset_Norway=read.csv("./data/QALDs_full_dataset_Norway.csv")
dataset_UK=read.csv("./data/QALDs_full_dataset_UK.csv")
dataset_combined_cohort=read.csv("./data/QALDs_full_dataset_combined_cohort.csv")

# COMBINED MIC COHORT

dataset_combined_cohort_binary_vars=dataset_combined_cohort%>%
  dplyr::select(-AGE,-SEX,-QALD_final_weighted,-employment_status,-employment_status_category,-severity_indicator,-COUNTRY)
dataset_combined_cohort_binary_vars[dataset_combined_cohort_binary_vars==1]='PRESENT'
dataset_combined_cohort_binary_vars[dataset_combined_cohort_binary_vars==0]='ABSENT'

dataset_combined_cohort_final=cbind.data.frame(dataset_combined_cohort[,c("AGE","SEX","COUNTRY","employment_status",
                                                                          "employment_status_category","severity_indicator","QALD_final_weighted")],
                                               dataset_combined_cohort_binary_vars)%>%
  mutate(employment_status=as.factor(employment_status))%>%
  mutate(employment_status_category=as.factor(employment_status_category))

## INDIVIDUAL RF ## 

store_RF_results_combined_cohort=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF_results_combined_cohort)=c("variable","inc_MSE","run")

for (i in 1:100){
  
  train_split_indices_combined_cohort=createDataPartition(dataset_combined_cohort_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_combined_cohort=dataset_combined_cohort_final[train_split_indices_combined_cohort,]
  #https://stackoverflow.com/questions/31968623/how-to-check-whether-a-column-contains-only-identical-elements-in-r
  dataset_train_combined_cohort=dataset_train_combined_cohort[,apply(dataset_train_combined_cohort, 2, function(a) length(unique(a))>1)]
  
  RF_train_combined_cohort=randomForest(QALD_final_weighted~.,data=dataset_train_combined_cohort,importance=TRUE)
  store_RF_results_combined_cohort=rbind.data.frame(store_RF_results_combined_cohort,
                                                    cbind.data.frame(variable=rownames(RF_train_combined_cohort$importance),
                                                                     inc_MSE=as.data.frame(RF_train_combined_cohort$importance)[,1],
                                                                     run=rep(i,ncol(dataset_train_combined_cohort)-1)))
  
}

store_RF_results_combined_cohort_summarized=store_RF_results_combined_cohort%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

store_RF_results_combined_cohort_summarized$variable<-c("AGE","ASTHMA",
                                                        "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                                        "CHRONIC HEMATOLOGICAL DISEASE",
                                                        "CHRONIC KIDNEY DISEASE",
                                                        "CHRONIC NEUROLOGICAL DISORDER",
                                                        "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                                        "COUNTRY","DEMENTIA",
                                                        "TYPE 1 DIABETES", "TYPE 2 DIABETES",
                                                        "DIABETES (TYPE NOT SPECIFIED)", 
                                                        "HIV", "HYPERTENSION",
                                                        "MALIGNANT NEOPLASM", 
                                                        "PSYCHOLOGICAL_DISORDER", "MILD LIVER DISEASE",
                                                        "OBESITY", "OTHER",
                                                        "RHEUMATOLOGICAL DISORDER",
                                                        "SEX", "SMOKING", 
                                                        "EMPLOYMENT STATUS", "EMPLOYMENT STATUS CATEGORY","SEVERITY INDICATOR")


ggplot(store_RF_results_combined_cohort_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_individual_variable_importance_combined_cohort.pdf")

## PRE-GROUPED RF ## 

store_RF2_results_combined_cohort=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF2_results_combined_cohort)=c("variable","inc_MSE","run")

## run across 100 trees
for (i in 1:100){
  
  train_split_indices_combined_cohort=createDataPartition(dataset_combined_cohort_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_combined_cohort=dataset_combined_cohort_final[train_split_indices_combined_cohort,]
  
  dataset_train_combined_cohort_x=dataset_train_combined_cohort%>%
    mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
    dplyr::select(-QALD_final_weighted,-MENTAL_DISORDER)
  
  dataset_train_combined_cohort_y=dataset_train_combined_cohort%>%
    dplyr::select(QALD_final_weighted)
  
  train_combined_cohort=c()
  train_combined_cohort$X=dataset_train_combined_cohort_x
  train_combined_cohort$y=as.matrix(dataset_train_combined_cohort_y)
  
  group_1="AGE"
  group_2=c("employment_status","SEX")
  group_3=c("ASTHMA","CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
            "SMOKING")
  group_4=c("OBESITY",
            "DIABETES_MELLITUS_TYPE_1",
            "DIABETES_MELLITUS_TYPE_2","DIABETES_MELLITUS_TYPE_NOT_SPECIFIED")
  group_5=c("CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
            "HYPERTENSION")
  group_6=c("CHRONIC_KIDNEY_DISEASE","MILD_LIVER_DISEASE")
  group_7="RHEUMATOLOGICAL_DISORDER"
  group_8=c("PSYCHOLOGICAL_DISORDER","CHRONIC_NEUROLOGICAL_DISORDER")
  group_9="MALIGNANT_NEOPLASM"
  group_10="OTHER"
  group_11="COUNTRY"
  group_12="severity_indicator"
  
  train_combined_cohort_X_ordered=cbind.data.frame(AGE=train_combined_cohort$X[,group_1],
                                                   train_combined_cohort$X[,group_2],
                                                   train_combined_cohort$X[,group_3],
                                                   train_combined_cohort$X[,group_4],
                                                   train_combined_cohort$X[,group_5],
                                                   train_combined_cohort$X[,group_6],
                                                   RHEMUATOLOGICAL_DISORDER=train_combined_cohort$X[,group_7],
                                                   train_combined_cohort$X[,group_8],
                                                   MALIGNANT_NEOPLASM=train_combined_cohort$X[,group_9],
                                                   OTHER=train_combined_cohort$X[,group_10],
                                                   COUNTRY=train_combined_cohort$X[,group_11],
                                                   severity_indicator=train_combined_cohort$X[,group_12])
  
  factor_analysis2_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_2],graph=FALSE)
  factor_analysis3_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_3],graph=FALSE)
  factor_analysis4_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_4],graph=FALSE)
  factor_analysis5_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_5],graph=FALSE)
  factor_analysis6_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_6],graph=FALSE)
  factor_analysis8_train_combined_cohort=FAMD(train_combined_cohort_X_ordered[,group_8],graph=FALSE)
  
  w_cluster2_quali_combined_cohort=(factor_analysis2_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis2_train_combined_cohort$eig[1,1])
  
  weighted_cluster2_vars_combined_cohort=train_combined_cohort_X_ordered[,group_2]%>%
    mutate(employment_status=ifelse(employment_status=='Full Time Carer (Children or Other)',
                                    w_cluster2_quali_combined_cohort[["Full Time Carer (Children or Other)"]],
                                    ifelse(employment_status=='Medically Retired',
                                           w_cluster2_quali_combined_cohort[["Medically Retired"]],
                                           ifelse(employment_status=='Retired',
                                                  w_cluster2_quali_combined_cohort[["Retired"]],
                                                  ifelse(employment_status=='Student',
                                                         w_cluster2_quali_combined_cohort[["Student"]],             
                                                         ifelse(employment_status=='Unable to Work due to Chronic Illness',
                                                                w_cluster2_quali_combined_cohort[["Unable to Work due to Chronic Illness"]],
                                                                ifelse(employment_status=='Unemployed',
                                                                       w_cluster2_quali_combined_cohort[["Unemployed"]],
                                                                       ifelse(employment_status=='Working Full-Time',
                                                                              w_cluster2_quali_combined_cohort[["Working Full-Time"]],
                                                                              w_cluster2_quali_combined_cohort[["Working Part-Time"]]))))))))%>%
    mutate(SEX=ifelse(SEX=='F',w_cluster2_quali_combined_cohort[["F"]],
                      w_cluster2_quali_combined_cohort[["M"]]))
  
  combined_component_2_combined_cohort=data.frame(rowSums(weighted_cluster2_vars_combined_cohort))
  
  w2_cluster2_quali_combined_cohort=(factor_analysis2_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis2_train_combined_cohort$eig[2,1])
  
  weighted2_cluster2_vars_combined_cohort=train_combined_cohort_X_ordered[,group_2]%>%
    mutate(employment_status=ifelse(employment_status=='Full Time Carer (Children or Other)',
                                    w2_cluster2_quali_combined_cohort[["Full Time Carer (Children or Other)"]],
                                    ifelse(employment_status=='Medically Retired',
                                           w2_cluster2_quali_combined_cohort[["Medically Retired"]],
                                           ifelse(employment_status=='Retired',
                                                  w2_cluster2_quali_combined_cohort[["Retired"]],
                                                  ifelse(employment_status=='Student',
                                                         w2_cluster2_quali_combined_cohort[["Student"]],             
                                                         ifelse(employment_status=='Unable to Work due to Chronic Illness',
                                                                w2_cluster2_quali_combined_cohort[["Unable to Work due to Chronic Illness"]],
                                                                ifelse(employment_status=='Unemployed',
                                                                       w2_cluster2_quali_combined_cohort[["Unemployed"]],
                                                                       ifelse(employment_status=='Working Full-Time',
                                                                              w2_cluster2_quali_combined_cohort[["Working Full-Time"]],
                                                                              w2_cluster2_quali_combined_cohort[["Working Part-Time"]]))))))))%>%
    mutate(SEX=ifelse(SEX=='F',w2_cluster2_quali_combined_cohort[["F"]],
                      w2_cluster2_quali_combined_cohort[["M"]]))
  
  combined2_component_2_combined_cohort=data.frame(rowSums(weighted2_cluster2_vars_combined_cohort))
  
  w_cluster3_quali_combined_cohort=(factor_analysis3_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis3_train_combined_cohort$eig[1,1])
  
  weighted_cluster3_vars_combined_cohort=train_combined_cohort_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w_cluster3_quali_combined_cohort[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w_cluster3_quali_combined_cohort[[4]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w_cluster3_quali_combined_cohort[[6]],0))
  
  combined_component_3_combined_cohort=data.frame(rowSums(weighted_cluster3_vars_combined_cohort))
  
  w2_cluster3_quali_combined_cohort=(factor_analysis3_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis3_train_combined_cohort$eig[2,1])
  
  weighted2_cluster3_vars_combined_cohort=train_combined_cohort_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w2_cluster3_quali_combined_cohort[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w2_cluster3_quali_combined_cohort[[4]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w2_cluster3_quali_combined_cohort[[6]],0))
  
  combined2_component_3_combined_cohort=data.frame(rowSums(weighted2_cluster3_vars_combined_cohort))
  
  w_cluster4_quali_combined_cohort=(factor_analysis4_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis4_train_combined_cohort$eig[1,1])
  
  weighted_cluster4_vars_combined_cohort=train_combined_cohort_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w_cluster4_quali_combined_cohort[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w_cluster4_quali_combined_cohort[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w_cluster4_quali_combined_cohort[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w_cluster4_quali_combined_cohort[[8]],0))
  
  combined_component_4_combined_cohort=data.frame(rowSums(weighted_cluster4_vars_combined_cohort))
  
  w2_cluster4_quali_combined_cohort=(factor_analysis4_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis4_train_combined_cohort$eig[2,1])
  
  weighted2_cluster4_vars_combined_cohort=train_combined_cohort_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w2_cluster4_quali_combined_cohort[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w2_cluster4_quali_combined_cohort[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w2_cluster4_quali_combined_cohort[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w2_cluster4_quali_combined_cohort[[8]],0))
  
  combined2_component_4_combined_cohort=data.frame(rowSums(weighted2_cluster4_vars_combined_cohort))
  
  w_cluster5_quali_combined_cohort=(factor_analysis5_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis5_train_combined_cohort$eig[1,1])
  
  weighted_cluster5_vars_combined_cohort=train_combined_cohort_X_ordered[,group_5]%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w_cluster5_quali_combined_cohort[[2]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w_cluster5_quali_combined_cohort[[4]],0))
  
  combined_component_5_combined_cohort=data.frame(rowSums(weighted_cluster5_vars_combined_cohort))
  
  w2_cluster5_quali_combined_cohort=(factor_analysis5_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis5_train_combined_cohort$eig[2,1])
  
  weighted2_cluster5_vars_combined_cohort=train_combined_cohort_X_ordered[,group_5]%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w2_cluster5_quali_combined_cohort[[2]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w2_cluster5_quali_combined_cohort[[4]],0))
  
  combined2_component_5_combined_cohort=data.frame(rowSums(weighted2_cluster5_vars_combined_cohort))
  
  #### cluster 6
  ##### PC#1
  w_cluster6_quali_combined_cohort=(factor_analysis6_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis6_train_combined_cohort$eig[1,1])
  
  weighted_cluster6_vars_combined_cohort=train_combined_cohort_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w_cluster6_quali_combined_cohort[[2]],0))%>%
    mutate(MILD_LIVER_DISEASE=ifelse(MILD_LIVER_DISEASE=='PRESENT',w_cluster6_quali_combined_cohort[[4]],0))
  
  combined_component_6_combined_cohort=data.frame(rowSums(weighted_cluster6_vars_combined_cohort))
  
  w2_cluster6_quali_combined_cohort=(factor_analysis6_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis6_train_combined_cohort$eig[2,1])
  
  weighted2_cluster6_vars_combined_cohort=train_combined_cohort_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w2_cluster6_quali_combined_cohort[[2]],0))%>%
    mutate(MILD_LIVER_DISEASE=ifelse(MILD_LIVER_DISEASE=='PRESENT',w2_cluster6_quali_combined_cohort[[4]],0))
  
  combined2_component_6_combined_cohort=data.frame(rowSums(weighted2_cluster6_vars_combined_cohort))
  
  w_cluster8_quali_combined_cohort=(factor_analysis8_train_combined_cohort$quali.var$coord[,1])/sqrt(factor_analysis8_train_combined_cohort$eig[1,1])
  
  weighted_cluster8_vars_combined_cohort=train_combined_cohort_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_combined_cohort[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_combined_cohort[[4]],0))
  combined_component_8_combined_cohort=data.frame(rowSums(weighted_cluster8_vars_combined_cohort))
  
  w2_cluster8_quali_combined_cohort=(factor_analysis8_train_combined_cohort$quali.var$coord[,2])/sqrt(factor_analysis8_train_combined_cohort$eig[2,1])
  
  weighted2_cluster8_vars_combined_cohort=train_combined_cohort_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_combined_cohort[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_combined_cohort[[4]],0))
  combined2_component_8_combined_cohort=data.frame(rowSums(weighted2_cluster8_vars_combined_cohort))
  
  all_components_df_train_combined_cohort=cbind.data.frame(cluster1=train_combined_cohort_X_ordered$AGE,
                                                           cluster2_PC1=unname(combined_component_2_combined_cohort),
                                                           cluster2_PC2=unname(combined2_component_2_combined_cohort),
                                                           cluster3_PC1=unname(combined_component_3_combined_cohort),
                                                           cluster3_PC2=unname(combined2_component_3_combined_cohort),
                                                           cluster4_PC1=unname(combined_component_4_combined_cohort),
                                                           cluster4_PC2=unname(combined2_component_4_combined_cohort),
                                                           cluster5_PC1=unname(combined_component_5_combined_cohort),
                                                           cluster5_PC2=unname(combined2_component_5_combined_cohort),
                                                           cluster6_PC1=unname(combined_component_6_combined_cohort),                                              
                                                           cluster6_PC2=unname(combined2_component_6_combined_cohort),
                                                           cluster7=train_combined_cohort_X_ordered$RHEMUATOLOGICAL_DISORDER,
                                                           cluster8_PC1=unname(combined_component_8_combined_cohort),
                                                           cluster8_PC2=unname(combined2_component_8_combined_cohort),
                                                           cluster_9=train_combined_cohort_X_ordered$MALIGNANT_NEOPLASM,
                                                           cluster_10=train_combined_cohort_X_ordered$OTHER,
                                                           cluster_11=train_combined_cohort_X_ordered$COUNTRY,
                                                           cluster_12=train_combined_cohort_X_ordered$severity_indicator,
                                                           QALD_final_weighted=dataset_train_combined_cohort_y[[1]])
  
  
  RF2_train_combined_cohort=randomForest(QALD_final_weighted~.,data=all_components_df_train_combined_cohort,importance=TRUE)
  store_RF2_results_combined_cohort=rbind.data.frame(store_RF2_results_combined_cohort,
                                                     cbind.data.frame(variable=rownames(RF2_train_combined_cohort$importance),
                                                                      inc_MSE=as.data.frame(RF2_train_combined_cohort$importance)[,1],
                                                                      run=rep(i,ncol(all_components_df_train_combined_cohort)-1)))
  
}

store_RF2_results_combined_cohort_summarized=store_RF2_results_combined_cohort%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

ggplot(store_RF2_results_combined_cohort_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_cluster_variable_importance_combined_cohort.pdf")

## MODEl-GROUPED RF (RF #3) ##

list_selected_groups_combined_cohort=c()
list_selected_vars_combined_cohort=c()

#### MUST RUN ON CLUSTER ####

covvsurf_combined_cohort <- function(data){
  for (i in 1:50){
    train_split_indices_combined_cohort=createDataPartition(data$QALD_final_weighted,p=0.8,list=FALSE)
    dataset_train_combined_cohort=data[train_split_indices_combined_cohort,]
    
    dataset_train_combined_cohort_x=dataset_train_combined_cohort%>%
      select(-QALD_final_weighted)
    
    dataset_train_combined_cohort_y=dataset_train_combined_cohort%>%
      select(QALD_final_weighted)
    
    dataset_train_combined_cohort_y=as.vector(dataset_train_combined_cohort_y$QALD_final_weighted)
    
    kval_combined_cohort <- 2:(ncol(dataset_train_combined_cohort_x)-1)
    
    set.seed(123)
    clustered_RF_combined_cohort_train=covsurf(dataset_train_combined_cohort_x, 
                                               dataset_train_combined_cohort_y, 
                                               kval_combined_cohort)
    list_selected_groups_combined_cohort=c(list_selected_groups_combined_cohort,clustered_RF_combined_cohort_train$vsel)
    list_selected_vars_combined_cohort=c(list_selected_vars_combined_cohort,names(clustered_RF_combined_cohort_train$ptree$cluster[which(clustered_RF_combined_cohort_train$ptree$cluster%in%clustered_RF_combined_cohort_train$vsel)]))
  }
  write.csv(list_selected_groups_combined_cohort,"list_selected_groups_combined_cohort.csv",row.names=F)
  write.csv(list_selected_vars_combined_cohort,"list_selected_vars_combined_cohort.csv",row.names=F)
  return(list(selected_groups=list_selected_groups_combined_cohort,
              selected_vars=list_selected_vars_combined_cohort))
}


# NORWAY

## subset to variables w/ both 0s and 1s & replace with ABSENTS and PRESENTS
### REF: https://stackoverflow.com/questions/69195830/r-summarising-factor-level-count-per-column
dataset_Norway_binary_vars=dataset_Norway%>%
  dplyr::select(-AGE,-SEX,-educ_yrs,-educ_yrs_quintile,-is_vaccinated,-QALD_final_weighted)
dataset_Norway_binary_vars[dataset_Norway_binary_vars==1]='PRESENT'
dataset_Norway_binary_vars[dataset_Norway_binary_vars==0]='ABSENT'

dataset_Norway_final=cbind.data.frame(dataset_Norway[,c("AGE","SEX","is_vaccinated",
                                                        "educ_yrs",
                                                        "educ_yrs_quintile","QALD_final_weighted")],
                                      dataset_Norway_binary_vars)%>%
  mutate(educ_yrs_quintile=factor(educ_yrs_quintile,ordered=TRUE))

## INDIVIDUAL RF (RF #1) ## 

### Run RF 100 times and summarized variable importance measures across runs

store_RF_results_Norway=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF_results_Norway)=c("variable","inc_MSE","run")

for (i in 1:100){
  
  train_split_indices_Norway=createDataPartition(dataset_Norway_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_Norway=dataset_Norway_final[train_split_indices_Norway,]
  
  RF_train_Norway=randomForest(QALD_final_weighted~.,
                               # for population correction sensitivity analysis, use sample_weight argument below
                               # sample_weight=dataset_train_Norway_weights,
                               data=dataset_train_Norway,importance=TRUE)
  store_RF_results_Norway=rbind.data.frame(store_RF_results_Norway,
                                           cbind.data.frame(variable=rownames(RF_train_Norway$importance),
                                                            inc_MSE=as.data.frame(RF_train_Norway$importance)[,1],
                                                            run=rep(i,ncol(dataset_train_Norway)-1)))
  
}

store_RF_results_Norway_summarized=store_RF_results_Norway%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

store_RF_results_Norway_summarized$variable=c("AGE","ASTHMA",
                                              "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                              "CHRONIC HAEMATOLOGICAL DISEASE",
                                              "CHRONIC KIDNEY DISEASE","CHRONIC NEUROLOGICAL DISORDER",
                                              "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                              "DIABETES MELLITUS (TYPE 1)",
                                              "DIABETES MELLITUS (TYPE 2)",
                                              "DIABETES MELLITUS (TYPE NOT SPECIFIED)",
                                              "HYPERTENSION","LIVER DISEASE (SEVERITY NOT SPECIFIED)",
                                              "MALIGNANT NEOPLASM","PSYCHOLOGICAL DISORDER",
                                              "OBESITY","OTHER","RHEUMATOLOGICAL DISORDER","SEX",
                                              "SMOKING",
                                              "EDUCATIONAL ATTAINMENT","EDUCATIONAL ATTAINMENT QUINTILE",
                                              "VACCINATION STATUS")

ggplot(store_RF_results_Norway_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_individual_variable_importance_Norway.pdf")

## PRE-GROUPED RF (RF #2) ## 

store_RF2_results_Norway=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF2_results_Norway)=c("variable","inc_MSE","run")

### run across 100 trees
for (i in 1:100){
  
  train_split_indices_Norway=createDataPartition(dataset_Norway_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_Norway=dataset_Norway_final[train_split_indices_Norway,]
  
  dataset_train_Norway_x=dataset_train_Norway%>%
    mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
    dplyr::select(-QALD_final_weighted,-MENTAL_DISORDER)
  
  dataset_train_Norway_y=dataset_train_Norway%>%
    dplyr::select(QALD_final_weighted)
  
  train_Norway=c()
  train_Norway$X=dataset_train_Norway_x
  train_Norway$y=as.matrix(dataset_train_Norway_y)
  
  #### pre-group variables
  group_1="AGE"
  group_2=c("educ_yrs","educ_yrs_quintile","SEX")
  group_3=c("ASTHMA","CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA","SMOKING")
  group_4=c("OBESITY","DIABETES_MELLITUS_TYPE_1",
            "DIABETES_MELLITUS_TYPE_2","DIABETES_MELLITUS_TYPE_NOT_SPECIFIED")
  group_5=c("CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION","HYPERTENSION")
  group_6=c("CHRONIC_KIDNEY_DISEASE","LIVER_DISEASE_SEVERITY_NOT_SPECIFIED")
  group_7=c("CHRONIC_HAEMATOLOGICAL_DISEASE","RHEUMATOLOGICAL_DISORDER")
  group_8=c("PSYCHOLOGICAL_DISORDER","CHRONIC_NEUROLOGICAL_DISORDER")
  group_9="MALIGNANT_NEOPLASM"
  group_10="OTHER"
  group_11='is_vaccinated'
  
  train_Norway_X_ordered=cbind.data.frame(AGE=train_Norway$X[,group_1],
                                          train_Norway$X[,group_2],
                                          train_Norway$X[,group_3],
                                          train_Norway$X[,group_4],
                                          train_Norway$X[,group_5],
                                          train_Norway$X[,group_6],
                                          train_Norway$X[,group_6],
                                          train_Norway$X[,group_7],
                                          train_Norway$X[,group_8],
                                          MALIGNANT.NEOPLASM=train_Norway$X[,group_9],
                                          OTHER=train_Norway$X[,group_10],
                                          is_vaccinated=train_Norway$X[,group_11])
  
  #### run PCA/MCA on each cluster of variables
  factor_analysis2_train_Norway=FAMD(train_Norway_X_ordered[,group_2],graph=FALSE)
  factor_analysis3_train_Norway=FAMD(train_Norway_X_ordered[,group_3],graph=FALSE)
  factor_analysis4_train_Norway=FAMD(train_Norway_X_ordered[,group_4],graph=FALSE)
  factor_analysis5_train_Norway=FAMD(train_Norway_X_ordered[,group_5],graph=FALSE)
  factor_analysis6_train_Norway=FAMD(train_Norway_X_ordered[,group_6],graph=FALSE)
  factor_analysis7_train_Norway=FAMD(train_Norway_X_ordered[,group_7],graph=FALSE)
  factor_analysis8_train_Norway=FAMD(train_Norway_X_ordered[,group_8],graph=FALSE)
  
  #### obtain weights for each variable = coordinates / square root of eigenvalue for PC (REF: https://scentellegher.github.io/machine-learning/2020/01/27/pca-loadings-sklearn.html)
  #### and apply to each subject
  
  #### cluster 2
  ##### PC#1
  w_cluster2_quali_Norway=(factor_analysis2_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis2_train_Norway$eig[1,1])
  w_cluster2_quanti_Norway=(factor_analysis2_train_Norway$quanti.var$coord[,1])/sqrt(factor_analysis2_train_Norway$eig[1,1])
  
  weighted_cluster2_vars_Norway=train_Norway_X_ordered[,group_2]%>%
    mutate(SEX=ifelse(SEX=='F',w_cluster2_quali_Norway[["F"]],
                      w_cluster2_quali_Norway[["M"]]))%>%
    mutate(educ_yrs_quintile=ifelse(educ_yrs_quintile=='1',w_cluster2_quali_Norway[['1']],
                                    ifelse(educ_yrs_quintile=='2',w_cluster2_quali_Norway[['2']],
                                           ifelse(educ_yrs_quintile=='3',w_cluster2_quali_Norway[['3']],
                                                  ifelse(educ_yrs_quintile=='4',w_cluster2_quali_Norway[['4']],
                                                         w_cluster2_quali_Norway[['5']])))))%>%
    mutate(educ_yrs=educ_yrs*w_cluster2_quanti_Norway)
  combined_component_2_Norway=data.frame(rowSums(weighted_cluster2_vars_Norway))
  
  ##### PC#2
  w2_cluster2_quali_Norway=(factor_analysis2_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis2_train_Norway$eig[2,1])
  w2_cluster2_quanti_Norway=(factor_analysis2_train_Norway$quanti.var$coord[,2])/sqrt(factor_analysis2_train_Norway$eig[2,1])
  
  weighted2_cluster2_vars_Norway=train_Norway_X_ordered[,group_2]%>%
    mutate(SEX=ifelse(SEX=='F',w2_cluster2_quali_Norway[["F"]],
                      w2_cluster2_quali_Norway[["M"]]))%>%
    mutate(educ_yrs_quintile=ifelse(educ_yrs_quintile=='1',w2_cluster2_quali_Norway[['1']],
                                    ifelse(educ_yrs_quintile=='2',w2_cluster2_quali_Norway[['2']],
                                           ifelse(educ_yrs_quintile=='3',w2_cluster2_quali_Norway[['3']],
                                                  ifelse(educ_yrs_quintile=='4',w2_cluster2_quali_Norway[['4']],
                                                         w2_cluster2_quali_Norway[['5']])))))%>%
    mutate(educ_yrs=educ_yrs*w2_cluster2_quanti_Norway)
  
  combined2_component_2_Norway=data.frame(rowSums(weighted2_cluster2_vars_Norway))
  
  #### cluster 3
  ##### PC#1
  w_cluster3_quali_Norway=(factor_analysis3_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis3_train_Norway$eig[1,1])
  
  weighted_cluster3_vars_Norway=train_Norway_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w_cluster3_quali_Norway[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w_cluster3_quali_Norway[[4]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w_cluster3_quali_Norway[[6]],0))
  
  combined_component_3_Norway=data.frame(rowSums(weighted_cluster3_vars_Norway))
  
  ##### PC #2
  w2_cluster3_quali_Norway=(factor_analysis3_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis3_train_Norway$eig[2,1])
  
  weighted2_cluster3_vars_Norway=train_Norway_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w2_cluster3_quali_Norway[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w2_cluster3_quali_Norway[[4]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w2_cluster3_quali_Norway[[6]],0))
  
  combined2_component_3_Norway=data.frame(rowSums(weighted2_cluster3_vars_Norway))
  
  
  #### cluster 4
  ##### PC#1
  w_cluster4_quali_Norway=(factor_analysis4_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis4_train_Norway$eig[1,1])
  
  weighted_cluster4_vars_Norway=train_Norway_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w_cluster4_quali_Norway[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w_cluster4_quali_Norway[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w_cluster4_quali_Norway[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w_cluster4_quali_Norway[[8]],0))
  
  combined_component_4_Norway=data.frame(rowSums(weighted_cluster4_vars_Norway))
  
  ##### PC#2
  w2_cluster4_quali_Norway=(factor_analysis4_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis4_train_Norway$eig[2,1])
  
  weighted2_cluster4_vars_Norway=train_Norway_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w2_cluster4_quali_Norway[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w2_cluster4_quali_Norway[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w2_cluster4_quali_Norway[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w2_cluster4_quali_Norway[[8]],0))
  
  combined2_component_4_Norway=data.frame(rowSums(weighted2_cluster4_vars_Norway))
  
  #### cluster 5
  ##### PC#1
  w_cluster5_quali_Norway=(factor_analysis5_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis5_train_Norway$eig[1,1])
  
  weighted_cluster5_vars_Norway=train_Norway_X_ordered[,group_5]%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w_cluster5_quali_Norway[[2]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w_cluster5_quali_Norway[[4]],0))
  
  combined_component_5_Norway=data.frame(rowSums(weighted_cluster5_vars_Norway))
  
  ##### PC#2
  w2_cluster5_quali_Norway=(factor_analysis5_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis5_train_Norway$eig[2,1])
  
  weighted2_cluster5_vars_Norway=train_Norway_X_ordered[,group_5]%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w2_cluster5_quali_Norway[[2]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w2_cluster5_quali_Norway[[4]],0))
  
  combined2_component_5_Norway=data.frame(rowSums(weighted2_cluster5_vars_Norway))
  
  #### cluster 6
  ##### PC#1
  w_cluster6_quali_Norway=(factor_analysis6_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis6_train_Norway$eig[1,1])
  
  weighted_cluster6_vars_Norway=train_Norway_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w_cluster6_quali_Norway[[2]],0))%>%
    mutate(LIVER_DISEASE_SEVERITY_NOT_SPECIFIED=ifelse(LIVER_DISEASE_SEVERITY_NOT_SPECIFIED=='PRESENT',w_cluster6_quali_Norway[[4]],0))
  
  combined_component_6_Norway=data.frame(rowSums(weighted_cluster6_vars_Norway))
  
  ##### PC#2
  w2_cluster6_quali_Norway=(factor_analysis6_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis6_train_Norway$eig[2,1])
  
  weighted2_cluster6_vars_Norway=train_Norway_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w2_cluster6_quali_Norway[[2]],0))%>%
    mutate(LIVER_DISEASE_SEVERITY_NOT_SPECIFIED=ifelse(LIVER_DISEASE_SEVERITY_NOT_SPECIFIED=='PRESENT',w2_cluster6_quali_Norway[[4]],0))
  
  combined2_component_6_Norway=data.frame(rowSums(weighted2_cluster6_vars_Norway))
  
  #### cluster 7
  ##### PC#1
  w_cluster7_quali_Norway=(factor_analysis7_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis7_train_Norway$eig[1,1])
  
  weighted_cluster7_vars_Norway=train_Norway_X_ordered[,group_7]%>%
    mutate(CHRONIC_HAEMATOLOGICAL_DISEASE=ifelse(CHRONIC_HAEMATOLOGICAL_DISEASE=='PRESENT',w_cluster7_quali_Norway[[2]],0))%>%
    mutate(RHEUMATOLOGICAL_DISORDER=ifelse(RHEUMATOLOGICAL_DISORDER=='PRESENT',w_cluster7_quali_Norway[[4]],0))
  
  combined_component_7_Norway=data.frame(rowSums(weighted_cluster7_vars_Norway))
  
  ##### PC#2
  w2_cluster7_quali_Norway=(factor_analysis7_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis7_train_Norway$eig[2,1])
  
  weighted2_cluster7_vars_Norway=train_Norway_X_ordered[,group_7]%>%
    mutate(CHRONIC_HAEMATOLOGICAL_DISEASE=ifelse(CHRONIC_HAEMATOLOGICAL_DISEASE=='PRESENT',w2_cluster7_quali_Norway[[2]],0))%>%
    mutate(RHEUMATOLOGICAL_DISORDER=ifelse(RHEUMATOLOGICAL_DISORDER=='PRESENT',w2_cluster7_quali_Norway[[4]],0))
  
  combined2_component_7_Norway=data.frame(rowSums(weighted2_cluster7_vars_Norway))
  
  #### cluster 8
  ##### PC#1
  w_cluster8_quali_Norway=(factor_analysis8_train_Norway$quali.var$coord[,1])/sqrt(factor_analysis8_train_Norway$eig[1,1])
  
  weighted_cluster8_vars_Norway=train_Norway_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_Norway[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_Norway[[4]],0))
  combined_component_8_Norway=data.frame(rowSums(weighted_cluster8_vars_Norway))
  
  ##### PC#2
  w2_cluster8_quali_Norway=(factor_analysis8_train_Norway$quali.var$coord[,2])/sqrt(factor_analysis8_train_Norway$eig[2,1])
  
  weighted2_cluster8_vars_Norway=train_Norway_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_Norway[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_Norway[[4]],0))
  combined2_component_8_Norway=data.frame(rowSums(weighted2_cluster8_vars_Norway))
  
  all_components_df_train_Norway=cbind.data.frame(cluster1=train_Norway_X_ordered$AGE,
                                                  cluster2_PC1=unname(combined_component_2_Norway),
                                                  cluster2_PC2=unname(combined2_component_2_Norway),
                                                  cluster3_PC1=unname(combined_component_3_Norway),
                                                  cluster3_PC2=unname(combined2_component_3_Norway),
                                                  cluster4_PC1=unname(combined_component_4_Norway),
                                                  cluster4_PC2=unname(combined2_component_4_Norway),
                                                  cluster5_PC1=unname(combined_component_5_Norway),
                                                  cluster5_PC2=unname(combined2_component_5_Norway),
                                                  cluster6_PC1=unname(combined_component_6_Norway),
                                                  cluster6_PC2=unname(combined2_component_6_Norway),
                                                  cluster7_PC1=unname(combined_component_7_Norway),
                                                  cluster7_PC2=unname(combined2_component_7_Norway),
                                                  cluster8_PC1=unname(combined_component_8_Norway),
                                                  cluster8_PC2=unname(combined2_component_8_Norway),
                                                  cluster_9=train_Norway_X_ordered$MALIGNANT.NEOPLASM,
                                                  cluster_10=train_Norway_X_ordered$OTHER,
                                                  cluster_11=train_Norway_X_ordered$is_vaccinated,
                                                  QALD_final_weighted=dataset_train_Norway_y[[1]])
  
  
  RF2_train_Norway=randomForest(QALD_final_weighted~.,
                                # for population correction sensitivity analysis, use sample_weight argument below
                                # sample_weight=dataset_train_Norway_weights,
                                data=all_components_df_train_Norway,importance=TRUE)
  store_RF2_results_Norway=rbind.data.frame(store_RF2_results_Norway,
                                            cbind.data.frame(variable=rownames(RF2_train_Norway$importance),
                                                             inc_MSE=as.data.frame(RF2_train_Norway$importance)[,1],
                                                             run=rep(i,ncol(all_components_df_train_Norway)-1)))
  
}

store_RF2_results_Norway_summarized=store_RF2_results_Norway%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

ggplot(store_RF2_results_Norway_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_cluster_variable_importance_Norway.pdf")

## MODEL-GROUPED RF (RF #3) ##

dataset_Norway_final_educ_quintile=as.data.frame(dataset_Norway_final$educ_yrs_quintile)
colnames(dataset_Norway_final_educ_quintile)<-"educ_yrs_quintile"
dummy_vars_educ_quintile=makedummies(dataset_Norway_final_educ_quintile)

dataset_Norway_final_without_educ_yrs_quintile=dataset_Norway_final%>%
  select(-educ_yrs_quintile)

dataset_Norway_final2=cbind.data.frame(dataset_Norway_final_without_educ_yrs_quintile,
                                       dummy_vars_educ_quintile)

### Run CoV-VSURF
#### MUST RUN ON CLUSTER ####

#### REF: https://robingenuer.github.io/CoVVSURF/

#### run 50 times as a robustness check and summarize cluster assignment results
list_selected_groups_Norway=c()
list_selected_vars_Norway=c()

covvsurf_Norway <- function(data){
  for (i in 1:50){
    train_split_indices_Norway=createDataPartition(data$QALD_final_weighted,p=0.8,list=FALSE)
    dataset_train_Norway=data[train_split_indices_Norway,]
    
    dataset_train_Norway_x=dataset_train_Norway%>%
      select(-QALD_final_weighted)
    
    dataset_train_Norway_y=dataset_train_Norway%>%
      select(QALD_final_weighted)
    
    dataset_train_Norway_y=as.vector(dataset_train_Norway_y$QALD_final_weighted)
    
    kval_Norway <- 2:(ncol(dataset_train_Norway_x)-1)
    
    set.seed(123)
    clustered_RF_Norway_train=covsurf(dataset_train_Norway_x, 
                                      # for population correction sensitivity analysis, use sample_weight argument below
                                      # sample_weight = dataset_train_Norway_weights
                                      dataset_train_Norway_y, kval_Norway)
    list_selected_groups_Norway=c(list_selected_groups_Norway,clustered_RF_Norway_train$vsel)
    list_selected_vars_Norway=c(list_selected_vars_Norway,names(clustered_RF_Norway_train$ptree$cluster[which(clustered_RF_Norway_train$ptree$cluster%in%clustered_RF_Norway_train$vsel)]))
  }
  write.csv(list_selected_groups_Norway,"list_selected_groups_Norway.csv",row.names=F)
  write.csv(list_selected_vars_Norway,"list_selected_vars_Norway.csv",row.names=F)
  return(list(selected_groups=list_selected_groups_Norway,
              selected_vars=list_selected_vars_Norway))
}


# UK

dataset_UK_binary_vars=dataset_UK%>%
  dplyr::select(-AGE,-SEX,-QALD_final_weighted,-employment_status,-employment_status_category,-is_treated_antiviral,-severity_indicator)
dataset_UK_binary_vars[dataset_UK_binary_vars==1]='PRESENT'
dataset_UK_binary_vars[dataset_UK_binary_vars==0]='ABSENT'

dataset_UK_final=cbind.data.frame(dataset_UK[,c("AGE","SEX","employment_status",
                                                "employment_status_category","QALD_final_weighted","is_treated_antiviral","severity_indicator")],
                                  dataset_UK_binary_vars)%>%
  mutate(employment_status=as.factor(employment_status))%>%
  mutate(employment_status_category=ifelse(employment_status_category=='Full-time employment',
                                           'Category_1',
                                           ifelse(employment_status_category=='Unemployed',
                                                  'Category_2',
                                                  ifelse(employment_status_category=='Part-time employment',
                                                         'Category_3',
                                                         ifelse(employment_status_category=='Retired',
                                                                'Category_4',
                                                                ifelse(employment_status_category=='Furloughed',
                                                                       'Category_5',
                                                                       'Category_6'))))))%>%
  mutate(employment_status_category=as.factor(employment_status_category))


## INDIVIDUAL RF (RF #1) ## 

store_RF_results_UK=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF_results_UK)=c("variable","inc_MSE","run")

for (i in 1:100){
  
  train_split_indices_UK=createDataPartition(dataset_UK_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_UK=dataset_UK_final[train_split_indices_UK,]
  
  RF_train_UK=randomForest(QALD_final_weighted~.,
                           # for population correction sensitivity analysis, use sample_weight argument below
                           # sample_weight = dataset_train_UK_weights,
                           data=dataset_train_UK,importance=TRUE)
  store_RF_results_UK=rbind.data.frame(store_RF_results_UK,
                                       cbind.data.frame(variable=rownames(RF_train_UK$importance),
                                                        inc_MSE=as.data.frame(RF_train_UK$importance)[,1],
                                                        run=rep(i,ncol(dataset_train_UK)-1)))
  
}

store_RF_results_UK_summarized=store_RF_results_UK%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

store_RF_results_UK_summarized$variable<-c("AGE","ASTHMA","ATRIAL FIBRILLATION",
                                           "BRONCIECTASIS", "CHRONIC CARDIAC DISEASE (NOT HYPERTENSION)",
                                           "CHRONIC HEMATOLOGICAL DISEASE",
                                           "CHRONIC INFECTION", "CHRONIC KIDNEY DISEASE",
                                           "CHRONIC METABOLIC ENDOCRINE DISEASE",
                                           "CHRONIC NEUROLOGICAL DISORDER",
                                           "CHRONIC PULMONARY DISEASE (NOT ASTHMA)",
                                           "CONGESTIVE HEART FAILURE",
                                           "TYPE 1 DIABETES", "TYPE 2 DIABETES",
                                           "DIABETES (TYPE NOT SPECIFIED)", "GI DISEASE",
                                           "GI REFLUX DISEASE", "HYPERTENSION",
                                           "HYPOTHYROIDISM",
                                           "ISCHAEMIC HEART DISEASE",
                                           "LIPID DISORDER", "MALIGNANT NEOPLASM", 
                                           "PSYCHOLOGICAL DISORDER", "MI", "OBESITY", "OTHER",
                                           "PERIPHERAL VASCULAR DISEASE",
                                           "RHEUMATOLOGICAL DISORDER", "SEX","SMOKING",
                                           "EMPLOYMENT STATUS", 
                                           "EMPLOYMENT STATUS CATEGORY",
                                           "ANTIVIRAL TREATMENT",
                                           "SEVERITY INDICATOR")


ggplot(store_RF_results_UK_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_individual_variable_importance_UK.pdf")

## PRE-GROUPED RF (RF #2) ## 

store_RF2_results_UK=as.data.frame(matrix(ncol=3,nrow=0))
colnames(store_RF2_results_UK)=c("variable","inc_MSE","run")

for (i in 1:100){
  
  train_split_indices_UK=createDataPartition(dataset_UK_final$QALD_final_weighted,p=0.8,list=FALSE)
  dataset_train_UK=dataset_UK_final[train_split_indices_UK,]
  
  dataset_train_UK_x=dataset_train_UK%>%
    mutate(PSYCHOLOGICAL_DISORDER=MENTAL_DISORDER)%>%
    dplyr::select(-QALD_final_weighted,-MENTAL_DISORDER)
  
  dataset_train_UK_y=dataset_train_UK%>%
    dplyr::select(QALD_final_weighted)
  
  train_UK=c()
  train_UK$X=dataset_train_UK_x
  train_UK$y=as.matrix(dataset_train_UK_y)
  
  group_1="AGE"
  group_2=c("employment_status","employment_status_category","SEX")
  group_3=c("ASTHMA","CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA",
            "BRONCHIECTASIS","SMOKING")
  group_4=c("OBESITY",
            "DIABETES_MELLITUS_TYPE_1",
            "DIABETES_MELLITUS_TYPE_2","DIABETES_MELLITUS_TYPE_NOT_SPECIFIED")
  group_5=c("CONGESTIVE_HEART_FAILURE","ISCHAEMIC_HEART_DISEASE",
            "ATRIAL_FIBRILLATION", "CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION",
            "HYPERTENSION","PERIPHERAL_VASCULAR_DISEASE")
  group_6=c("CHRONIC_KIDNEY_DISEASE",
            "GI_DISEASE","CHRONIC_METABOLIC_ENDOCRINE_DISEASE",
            "GI_REFLUX_DISEASE","HYPOTHYROIDISM","LIPID_DISORDER")
  group_7=c("CHRONIC_HAEMATOLOGICAL_DISEASE", 
            "RHEUMATOLOGICAL_DISORDER")
  group_8=c("PSYCHOLOGICAL_DISORDER","CHRONIC_NEUROLOGICAL_DISORDER")
  group_9="MALIGNANT_NEOPLASM"
  group_10='MI'
  group_11="CHRONIC_INFECTION"
  group_12="OTHER"
  group_13="is_treated_antiviral"
  group_14="severity_indicator"
  
  
  train_UK_X_ordered=cbind.data.frame(AGE=train_UK$X[,group_1],
                                      train_UK$X[,group_2],
                                      train_UK$X[,group_3],
                                      train_UK$X[,group_4],
                                      train_UK$X[,group_5],
                                      train_UK$X[,group_6],
                                      train_UK$X[,group_7],
                                      train_UK$X[,group_8],
                                      MALIGNANT_NEOPLASM=train_UK$X[,group_9],
                                      MI=train_UK$X[,group_10],
                                      CHRONIC_INFECTION=train_UK$X[,group_11],
                                      OTHER=train_UK$X[,group_12],
                                      is_treated_antiviral=train_UK$X[,group_13],
                                      severity_indicator=train_UK$X[,group_14])
  
  factor_analysis2_train_UK=FAMD(train_UK_X_ordered[,group_2],graph=FALSE)
  factor_analysis3_train_UK=FAMD(train_UK_X_ordered[,group_3],graph=FALSE)
  factor_analysis4_train_UK=FAMD(train_UK_X_ordered[,group_4],graph=FALSE)
  factor_analysis5_train_UK=FAMD(train_UK_X_ordered[,group_5],graph=FALSE)
  factor_analysis6_train_UK=FAMD(train_UK_X_ordered[,group_6],graph=FALSE)
  factor_analysis7_train_UK=FAMD(train_UK_X_ordered[,group_7],graph=FALSE)
  factor_analysis8_train_UK=FAMD(train_UK_X_ordered[,group_8],graph=FALSE)
  
  w_cluster2_quali_UK=(factor_analysis2_train_UK$quali.var$coord[,1])/sqrt(factor_analysis2_train_UK$eig[1,1])
  
  weighted_cluster2_vars_UK=train_UK_X_ordered[,group_2]%>%
    mutate(employment_status=ifelse(employment_status=='Carer',
                                    w_cluster2_quali_UK[["Carer"]],
                                    ifelse(employment_status=='Full-time employment',
                                           w_cluster2_quali_UK[["Full-time employment"]],
                                           ifelse(employment_status=='Full-time employment_Furloughed',
                                                  w_cluster2_quali_UK[["Full-time employment_Furloughed"]],             
                                                  ifelse(employment_status=='Full-time employment_Prefer not to say',
                                                         w_cluster2_quali_UK[["Full-time employment_Prefer not to say"]],
                                                         ifelse(employment_status=='Furloughed',
                                                                w_cluster2_quali_UK[["Furloughed"]],
                                                                ifelse(employment_status=='Medically retired',
                                                                       w_cluster2_quali_UK[["Medically retired"]],
                                                                       ifelse(employment_status=='Part-time employment',
                                                                              w_cluster2_quali_UK[["Part-time employment"]],
                                                                              ifelse(employment_status=='Part-time employment_Retired',
                                                                                     w_cluster2_quali_UK[["Part-time employment_Retired"]],
                                                                                     ifelse(employment_status=='Retired',
                                                                                            w_cluster2_quali_UK[["Retired"]],
                                                                                            ifelse(employment_status=='Student',
                                                                                                   w_cluster2_quali_UK[["Student"]],
                                                                                                   ifelse(employment_status=='Unable to work due to chronic illness',
                                                                                                          w_cluster2_quali_UK[["Unable to work due to chronic illness"]],
                                                                                                          w_cluster2_quali_UK[["Unemployed"]]))))))))))))%>%
    mutate(employment_status_category=ifelse(employment_status_category=='Category_1',
                                             w_cluster2_quali_UK[["Category_1"]],
                                             ifelse(employment_status_category=='Category_2',
                                                    w_cluster2_quali_UK[["Category_2"]],
                                                    ifelse(employment_status_category=='Category_3',
                                                           w_cluster2_quali_UK[["Category_3"]],
                                                           ifelse(employment_status_category=='Category_4',
                                                                  w_cluster2_quali_UK[["Category_4"]],
                                                                  ifelse(employment_status_category=='Category_5',
                                                                         w_cluster2_quali_UK[["Category_5"]],
                                                                         w_cluster2_quali_UK[["Category_6"]]))))))%>%
    mutate(SEX=ifelse(SEX=='F',w_cluster2_quali_UK[["F"]],
                      w_cluster2_quali_UK[["M"]]))
  
  combined_component_2_UK=data.frame(rowSums(weighted_cluster2_vars_UK))
  
  w2_cluster2_quali_UK=(factor_analysis2_train_UK$quali.var$coord[,2])/sqrt(factor_analysis2_train_UK$eig[2,1])
  
  weighted2_cluster2_vars_UK=train_UK_X_ordered[,group_2]%>%
    mutate(employment_status=ifelse(employment_status=='Carer',
                                    w2_cluster2_quali_UK[["Carer"]],
                                    ifelse(employment_status=='Full-time employment',
                                           w2_cluster2_quali_UK[["Full-time employment"]],
                                           ifelse(employment_status=='Full-time employment_Furloughed',
                                                  w2_cluster2_quali_UK[["Full-time employment_Furloughed"]],             
                                                  ifelse(employment_status=='Full-time employment_Prefer not to say',
                                                         w2_cluster2_quali_UK[["Full-time employment_Prefer not to say"]],
                                                         ifelse(employment_status=='Furloughed',
                                                                w2_cluster2_quali_UK[["Furloughed"]],
                                                                ifelse(employment_status=='Medically retired',
                                                                       w2_cluster2_quali_UK[["Medically retired"]],
                                                                       ifelse(employment_status=='Part-time employment',
                                                                              w2_cluster2_quali_UK[["Part-time employment"]],
                                                                              ifelse(employment_status=='Part-time employment_Retired',
                                                                                     w2_cluster2_quali_UK[["Part-time employment_Retired"]],
                                                                                     ifelse(employment_status=='Retired',
                                                                                            w2_cluster2_quali_UK[["Retired"]],
                                                                                            ifelse(employment_status=='Student',
                                                                                                   w2_cluster2_quali_UK[["Student"]],
                                                                                                   ifelse(employment_status=='Unable to work due to chronic illness',
                                                                                                          w2_cluster2_quali_UK[["Unable to work due to chronic illness"]],
                                                                                                          w2_cluster2_quali_UK[["Unemployed"]]))))))))))))%>%
    mutate(employment_status_category=ifelse(employment_status_category=='Category_1',
                                             w2_cluster2_quali_UK[["Category_1"]],
                                             ifelse(employment_status_category=='Category_2',
                                                    w2_cluster2_quali_UK[["Category_2"]],
                                                    ifelse(employment_status_category=='Category_3',
                                                           w2_cluster2_quali_UK[["Category_3"]],
                                                           ifelse(employment_status_category=='Category_4',
                                                                  w2_cluster2_quali_UK[["Category_4"]],
                                                                  ifelse(employment_status_category=='Category_5',
                                                                         w2_cluster2_quali_UK[["Category_5"]],
                                                                         w2_cluster2_quali_UK[["Category_6"]]))))))%>%
    mutate(SEX=ifelse(SEX=='F',w2_cluster2_quali_UK[["F"]],
                      w2_cluster2_quali_UK[["M"]]))
  
  combined2_component_2_UK=data.frame(rowSums(weighted2_cluster2_vars_UK))
  
  w_cluster3_quali_UK=(factor_analysis3_train_UK$quali.var$coord[,1])/sqrt(factor_analysis3_train_UK$eig[1,1])
  
  weighted_cluster3_vars_UK=train_UK_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w_cluster3_quali_UK[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w_cluster3_quali_UK[[4]],0))%>%
    mutate(BRONCHIECTASIS=ifelse(BRONCHIECTASIS=='PRESENT',w_cluster3_quali_UK[[6]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w_cluster3_quali_UK[[8]],0))
  
  combined_component_3_UK=data.frame(rowSums(weighted_cluster3_vars_UK))
  
  w2_cluster3_quali_UK=(factor_analysis3_train_UK$quali.var$coord[,2])/sqrt(factor_analysis3_train_UK$eig[2,1])
  
  weighted2_cluster3_vars_UK=train_UK_X_ordered[,group_3]%>%
    mutate(ASTHMA=ifelse(ASTHMA=='PRESENT',w2_cluster3_quali_UK[[2]],0))%>%
    mutate(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=ifelse(CHRONIC_PULMONARY_DISEASE_NOT_ASTHMA=='PRESENT',w2_cluster3_quali_UK[[4]],0))%>%
    mutate(BRONCHIECTASIS=ifelse(BRONCHIECTASIS=='PRESENT',w2_cluster3_quali_UK[[6]],0))%>%
    mutate(SMOKING=ifelse(SMOKING=='PRESENT',w2_cluster3_quali_UK[[8]],0))
  
  combined2_component_3_UK=data.frame(rowSums(weighted2_cluster3_vars_UK))
  
  w_cluster4_quali_UK=(factor_analysis4_train_UK$quali.var$coord[,1])/sqrt(factor_analysis4_train_UK$eig[1,1])
  
  weighted_cluster4_vars_UK=train_UK_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w_cluster4_quali_UK[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w_cluster4_quali_UK[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w_cluster4_quali_UK[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w_cluster4_quali_UK[[8]],0))
  
  combined_component_4_UK=data.frame(rowSums(weighted_cluster4_vars_UK))
  
  w2_cluster4_quali_UK=(factor_analysis4_train_UK$quali.var$coord[,2])/sqrt(factor_analysis4_train_UK$eig[2,1])
  
  weighted2_cluster4_vars_UK=train_UK_X_ordered[,group_4]%>%
    mutate(OBESITY=ifelse(OBESITY=='PRESENT',w2_cluster4_quali_UK[[2]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_1=ifelse(DIABETES_MELLITUS_TYPE_1=='PRESENT',w2_cluster4_quali_UK[[4]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_2=ifelse(DIABETES_MELLITUS_TYPE_2=='PRESENT',w2_cluster4_quali_UK[[6]],0))%>%
    mutate(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=ifelse(DIABETES_MELLITUS_TYPE_NOT_SPECIFIED=='PRESENT',w2_cluster4_quali_UK[[8]],0))
  
  combined2_component_4_UK=data.frame(rowSums(weighted2_cluster4_vars_UK))
  
  w_cluster5_quali_UK=(factor_analysis5_train_UK$quali.var$coord[,1])/sqrt(factor_analysis5_train_UK$eig[1,1])
  
  weighted_cluster5_vars_UK=train_UK_X_ordered[,group_5]%>%
    mutate(CONGESTIVE_HEART_FAILURE=ifelse(CONGESTIVE_HEART_FAILURE=='PRESENT',w_cluster5_quali_UK[[2]],0))%>%
    mutate(ISCHAEMIC_HEART_DISEASE=ifelse(ISCHAEMIC_HEART_DISEASE=='PRESENT',w_cluster5_quali_UK[[4]],0))%>%
    mutate(ATRIAL_FIBRILLATION=ifelse(ATRIAL_FIBRILLATION=='PRESENT',w_cluster5_quali_UK[[6]],0))%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w_cluster5_quali_UK[[8]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w_cluster5_quali_UK[[10]],0))%>%
    mutate(PERIPHERAL_VASCULAR_DISEASE=ifelse(PERIPHERAL_VASCULAR_DISEASE=='PRESENT',w_cluster5_quali_UK[[12]],0))
  
  combined_component_5_UK=data.frame(rowSums(weighted_cluster5_vars_UK))

  w2_cluster5_quali_UK=(factor_analysis5_train_UK$quali.var$coord[,2])/sqrt(factor_analysis5_train_UK$eig[2,1])
  
  weighted2_cluster5_vars_UK=train_UK_X_ordered[,group_5]%>%
    mutate(CONGESTIVE_HEART_FAILURE=ifelse(CONGESTIVE_HEART_FAILURE=='PRESENT',w2_cluster5_quali_UK[[2]],0))%>%
    mutate(ISCHAEMIC_HEART_DISEASE=ifelse(ISCHAEMIC_HEART_DISEASE=='PRESENT',w2_cluster5_quali_UK[[4]],0))%>%
    mutate(ATRIAL_FIBRILLATION=ifelse(ATRIAL_FIBRILLATION=='PRESENT',w2_cluster5_quali_UK[[6]],0))%>%
    mutate(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=ifelse(CHRONIC_CARDIAC_DISEASE_NOT_HYPERTENSION=='PRESENT',w2_cluster5_quali_UK[[8]],0))%>%
    mutate(HYPERTENSION=ifelse(HYPERTENSION=='PRESENT',w2_cluster5_quali_UK[[10]],0))%>%
    mutate(PERIPHERAL_VASCULAR_DISEASE=ifelse(PERIPHERAL_VASCULAR_DISEASE=='PRESENT',w2_cluster5_quali_UK[[12]],0))
  
  combined2_component_5_UK=data.frame(rowSums(weighted2_cluster5_vars_UK))
  
  w_cluster6_quali_UK=(factor_analysis6_train_UK$quali.var$coord[,1])/sqrt(factor_analysis6_train_UK$eig[1,1])
  
  weighted_cluster6_vars_UK=train_UK_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w_cluster6_quali_UK[[2]],0))%>%
    mutate(GI_DISEASE=ifelse(GI_DISEASE=='PRESENT',w_cluster6_quali_UK[[4]],0))%>%
    mutate(CHRONIC_METABOLIC_ENDOCRINE_DISEASE=ifelse(CHRONIC_METABOLIC_ENDOCRINE_DISEASE=='PRESENT',w_cluster6_quali_UK[[6]],0))%>%
    mutate(GI_REFLUX_DISEASE=ifelse(GI_REFLUX_DISEASE=='PRESENT',w_cluster6_quali_UK[[8]],0))%>%
    mutate(HYPOTHYROIDISM=ifelse(HYPOTHYROIDISM=='PRESENT',w_cluster6_quali_UK[[10]],0))%>%
    mutate(LIPID_DISORDER=ifelse(LIPID_DISORDER=='PRESENT',w_cluster6_quali_UK[[12]],0))
  
  combined_component_6_UK=data.frame(rowSums(weighted_cluster6_vars_UK))
  
  w2_cluster6_quali_UK=(factor_analysis6_train_UK$quali.var$coord[,2])/sqrt(factor_analysis6_train_UK$eig[2,1])
  
  weighted2_cluster6_vars_UK=train_UK_X_ordered[,group_6]%>%
    mutate(CHRONIC_KIDNEY_DISEASE=ifelse(CHRONIC_KIDNEY_DISEASE=='PRESENT',w2_cluster6_quali_UK[[2]],0))%>%
    mutate(GI_DISEASE=ifelse(GI_DISEASE=='PRESENT',w2_cluster6_quali_UK[[4]],0))%>%
    mutate(CHRONIC_METABOLIC_ENDOCRINE_DISEASE=ifelse(CHRONIC_METABOLIC_ENDOCRINE_DISEASE=='PRESENT',w2_cluster6_quali_UK[[6]],0))%>%
    mutate(GI_REFLUX_DISEASE=ifelse(GI_REFLUX_DISEASE=='PRESENT',w2_cluster6_quali_UK[[8]],0))%>%
    mutate(HYPOTHYROIDISM=ifelse(HYPOTHYROIDISM=='PRESENT',w2_cluster6_quali_UK[[10]],0))%>%
    mutate(LIPID_DISORDER=ifelse(LIPID_DISORDER=='PRESENT',w2_cluster6_quali_UK[[12]],0))
  
  combined2_component_6_UK=data.frame(rowSums(weighted2_cluster6_vars_UK))
  
  w_cluster7_quali_UK=(factor_analysis7_train_UK$quali.var$coord[,1])/sqrt(factor_analysis7_train_UK$eig[1,1])
  
  weighted_cluster7_vars_UK=train_UK_X_ordered[,group_7]%>%
    mutate(CHRONIC_HAEMATOLOGICAL_DISEASE=ifelse(CHRONIC_HAEMATOLOGICAL_DISEASE=='PRESENT',w_cluster7_quali_UK[[2]],0))%>%
    mutate(RHEUMATOLOGICAL_DISORDER=ifelse(RHEUMATOLOGICAL_DISORDER=='PRESENT',w_cluster7_quali_UK[[4]],0))
  
  combined_component_7_UK=data.frame(rowSums(weighted_cluster7_vars_UK))
  
  w2_cluster7_quali_UK=(factor_analysis7_train_UK$quali.var$coord[,2])/sqrt(factor_analysis7_train_UK$eig[2,1])
  
  weighted2_cluster7_vars_UK=train_UK_X_ordered[,group_7]%>%
    mutate(CHRONIC_HAEMATOLOGICAL_DISEASE=ifelse(CHRONIC_HAEMATOLOGICAL_DISEASE=='PRESENT',w2_cluster7_quali_UK[[2]],0))%>%
    mutate(RHEUMATOLOGICAL_DISORDER=ifelse(RHEUMATOLOGICAL_DISORDER=='PRESENT',w2_cluster7_quali_UK[[4]],0))
  
  combined2_component_7_UK=data.frame(rowSums(weighted2_cluster7_vars_UK))
  
  w_cluster8_quali_UK=(factor_analysis8_train_UK$quali.var$coord[,1])/sqrt(factor_analysis8_train_UK$eig[1,1])
  
  weighted_cluster8_vars_UK=train_UK_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_UK[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w_cluster8_quali_UK[[4]],0))
  combined_component_8_UK=data.frame(rowSums(weighted_cluster8_vars_UK))
  
  w2_cluster8_quali_UK=(factor_analysis8_train_UK$quali.var$coord[,2])/sqrt(factor_analysis8_train_UK$eig[2,1])
  
  weighted2_cluster8_vars_UK=train_UK_X_ordered[,group_8]%>%
    mutate(PSYCHOLOGICAL_DISORDER=ifelse(PSYCHOLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_UK[[2]],0))%>%
    mutate(CHRONIC_NEUROLOGICAL_DISORDER=ifelse(CHRONIC_NEUROLOGICAL_DISORDER=='PRESENT',w2_cluster8_quali_UK[[4]],0))
  combined2_component_8_UK=data.frame(rowSums(weighted2_cluster8_vars_UK))
  
  all_components_df_train_UK=cbind.data.frame(cluster1=train_UK_X_ordered$AGE,
                                              cluster2_PC1=unname(combined_component_2_UK),
                                              cluster2_PC2=unname(combined2_component_2_UK),
                                              cluster3_PC1=unname(combined_component_3_UK),
                                              cluster3_PC2=unname(combined2_component_3_UK),
                                              cluster4_PC1=unname(combined_component_4_UK),
                                              cluster4_PC2=unname(combined2_component_4_UK),
                                              cluster5_PC1=unname(combined_component_5_UK),
                                              cluster5_PC2=unname(combined2_component_5_UK),
                                              cluster6_PC1=unname(combined_component_6_UK),
                                              cluster6_PC2=unname(combined2_component_6_UK),
                                              cluster7_PC1=unname(combined_component_7_UK),
                                              cluster7_PC2=unname(combined2_component_7_UK),
                                              cluster8_PC1=unname(combined_component_8_UK),
                                              cluster8_PC2=unname(combined2_component_8_UK),
                                              cluster_9=train_UK_X_ordered$MALIGNANT_NEOPLASM,
                                              cluster_10=train_UK_X_ordered$MI,
                                              cluster_11=train_UK_X_ordered$CHRONIC_INFECTION,
                                              cluster_12=train_UK_X_ordered$OTHER,
                                              cluster_13=train_UK_X_ordered$is_treated_antiviral,
                                              cluster_14=train_UK_X_ordered$severity_indicator,
                                              QALD_final_weighted=dataset_train_UK_y[[1]])
  
  RF2_train_UK=randomForest(QALD_final_weighted~.,
                            # for population correction sensitivity analysis, use sample_weight argument below
                            # sample_weight = dataset_train_UK_weights,
                            data=all_components_df_train_UK,importance=TRUE)
  store_RF2_results_UK=rbind.data.frame(store_RF2_results_UK,
                                        cbind.data.frame(variable=rownames(RF2_train_UK$importance),
                                                         inc_MSE=as.data.frame(RF2_train_UK$importance)[,1],
                                                         run=rep(i,ncol(all_components_df_train_UK)-1)))
  
}

store_RF2_results_UK_summarized=store_RF2_results_UK%>%
  group_by(variable)%>%
  summarise(mean_inc_MSE=mean(inc_MSE))

ggplot(store_RF2_results_UK_summarized,aes(x=reorder(variable,mean_inc_MSE),y=mean_inc_MSE,fill=variable))+
  geom_col(show.legend=FALSE)+coord_flip()+theme_classic()+ylab("% increase in MSE (averaged over runs)")+xlab("variable")

ggsave("./figures/prediction/RF_cluster_variable_importance_UK.pdf")

## MODEl-GROUPED RF (RF #3) ##

dataset_UK=read.csv("./data/QALDs_full_dataset_UK.csv")%>%
  mutate(SEX=as.factor(SEX))

dataset_UK_binary_vars=dataset_UK%>%
  dplyr::select(-AGE,-SEX,-QALD_final_weighted,-employment_status,-employment_status_category,-is_treated_antiviral,-severity_indicator)
dataset_UK_binary_vars[dataset_UK_binary_vars==1]='PRESENT'
dataset_UK_binary_vars[dataset_UK_binary_vars==0]='ABSENT'

dataset_UK_final=cbind.data.frame(dataset_UK[,c("AGE","SEX","employment_status",
                                                "employment_status_category",
                                                "QALD_final_weighted","is_treated_antiviral","severity_indicator")],
                                  dataset_UK_binary_vars)%>%
  mutate(employment_status=as.factor(employment_status))%>%
  mutate(employment_status_category=as.factor(employment_status_category))


### REF: https://robingenuer.github.io/CoVVSURF/

#### MUST RUN ON CLUSTER ####

list_selected_groups_UK=c()
list_selected_vars_UK=c()

covvsurf_UK <- function(data){
  for (i in 1:50){
    train_split_indices_UK=createDataPartition(data$QALD_final_weighted,p=0.8,list=FALSE)
    dataset_train_UK=data[train_split_indices_UK,]
    
    dataset_train_UK_x=dataset_train_UK%>%
      select(-QALD_final_weighted)
    
    dataset_train_UK_y=dataset_train_UK%>%
      select(QALD_final_weighted)
    
    dataset_train_UK_y=as.vector(dataset_train_UK_y$QALD_final_weighted)
    
    kval_UK <- 2:(ncol(dataset_train_UK_x)-1)
    
    set.seed(123)
    clustered_RF_UK_train=covsurf(dataset_train_UK_x, dataset_train_UK_y, 
                                  # for population correction sensitivity analysis, use sample_weight argument below
                                  # sample_weight = dataset_train_UK_weights
                                  kval_UK)
    list_selected_groups_UK=c(list_selected_groups_UK,clustered_RF_UK_train$vsel)
    list_selected_vars_UK=c(list_selected_vars_UK,names(clustered_RF_UK_train$ptree$cluster[which(clustered_RF_UK_train$ptree$cluster%in%clustered_RF_UK_train$vsel)]))
  }
  write.csv(list_selected_groups_UK,"list_selected_groups_UK.csv",row.names=F)
  write.csv(list_selected_vars_UK,"list_selected_vars_UK.csv",row.names=F)
  return(list(selected_groups=list_selected_groups_UK,
              selected_vars=list_selected_vars_UK))
}


