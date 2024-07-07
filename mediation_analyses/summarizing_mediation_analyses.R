library(tidyverse)

all_mediation_output_Norway=read.csv("./output/mediation_analyses/all_mediation_output_Norway.csv")
all_mediation_output_UK=read.csv("./output/mediation_analyses/all_mediation_output_UK.csv")
all_mediation_output_Russia=read.csv("./output/mediation_analyses/all_mediation_output_Russia.csv")

all_mediation_output_Norway_sensitivity_analysis=read.csv("./output/mediation_analyses/all_mediation_output_Norway_sensitivity_analysis.csv")
all_mediation_output_UK_sensitivity_analysis=read.csv("./output/mediation_analyses/all_mediation_output_UK_sensitivity_analysis.csv")

# NORWAY

## main analysis
all_mediation_output_Norway_effects=all_mediation_output_Norway%>%
  filter(effect!='pm_educ_binary')%>%
  filter(effect!='pm_educ_5_1')%>%
  filter(effect!='pm_educ_4_1')%>%
  filter(effect!='pm_educ_3_1')%>%
  filter(effect!='pm_sex')

all_mediation_output_Norway_effects$measure=c("NDE_educ_binary","NIE_educ_binary",
                                              "NDE_educ_quintile_5_1","NIE_educ_quintile_5_1",
                                              "NDE_educ_quintile_4_1","NIE_educ_quintile_4_1",
                                              "NDE_educ_quintile_3_1","NIE_educ_quintile_3_1",
                                              "NDE_sex","NIE_sex")

all_mediation_output_Norway_effects$measure_group=c("SES_1","SES_1",
                                                    "SES_2","SES_2",
                                                    "SES_3","SES_3",
                                                    "SES_4","SES_4",
                                                    "Sex","Sex")

ggplot(all_mediation_output_Norway_effects,aes(x=measure,y=est))+
  geom_point(aes(group=measure_group,col=measure_group),size=4)+
  geom_errorbar(aes(ymin=LL,ymax=UL,col=measure_group),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c('NDE for high educ', 
                            'NDE for quintile 3 vs 1 educ',
                            'NDE for quintile 4 vs 1 educ',
                            'NDE for quintile 5 vs 1 educ',
                            'NDE for female sex at birth',
                            'NIE for high educ',
                            'NIE for quintile 3 vs 1 educ',
                            'NIE for quintile 4 vs 1 educ',
                            'NIE for quintile 5 vs 1 educ',
                            'NIE for female sex at birth'))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_effects_Norway.png")

all_mediation_output_Norway_pnm=all_mediation_output_Norway%>%
  filter(effect%in%c('pm_educ_binary','pm_educ_5_1','pm_educ_4_1',
                     'pm_educ_3_1','pm_sex'))%>%
  mutate(LL_p_non_mediated=1-UL)%>%
  mutate(est_p_non_mediated=1-est)%>%
  mutate(UL_p_non_mediated=1-LL)
  
all_mediation_output_Norway_pnm$measure=c("prop_non_mediated_educ_binary","prop_non_mediated_educ_quintile_5_1",
                                          "prop_non_mediated_educ_quintile_4_1","prop_non_mediated_educ_quintile_3_1",
                                          "prop_non_mediated_sex")

ggplot(all_mediation_output_Norway_pnm,aes(x=measure,y=est_p_non_mediated))+
  geom_point(aes(group=measure,col=measure),size=4)+
  geom_errorbar(aes(ymin=LL_p_non_mediated,ymax=UL_p_non_mediated,col=measure),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, hjust=1,size=9),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c("proportion non-mediated for high educ",
                            "proportion non-mediated for quintile 3 vs 1 educ",
                            "proportion non-mediated for quintile 4 vs 1 educ",
                            "proportion non-mediated for quintile 5 vs 1 educ",
                             "proportion non-mediated for female sex at birth"))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_pnm_Norway.png")

## sensitivity analysis
all_mediation_output_Norway_effects_sensitivity=all_mediation_output_Norway_sensitivity_analysis%>%
  filter(effect!='pm_educ_binary')%>%
  filter(effect!='pm_educ_5_1')%>%
  filter(effect!='pm_educ_4_1')%>%
  filter(effect!='pm_educ_3_1')%>%
  filter(effect!='pm_sex')

all_mediation_output_Norway_effects_sensitivity$measure=c("NDE_educ_binary","NIE_educ_binary",
                                              "NDE_educ_quintile_5_1","NIE_educ_quintile_5_1",
                                              "NDE_educ_quintile_4_1","NIE_educ_quintile_4_1",
                                              "NDE_educ_quintile_3_1","NIE_educ_quintile_3_1",
                                              "NDE_sex","NIE_sex")

all_mediation_output_Norway_effects_sensitivity$measure_group=c("SES_1","SES_1",
                                                    "SES_2","SES_2",
                                                    "SES_3","SES_3",
                                                    "SES_4","SES_4",
                                                    "Sex","Sex")

ggplot(all_mediation_output_Norway_effects_sensitivity,aes(x=measure,y=est))+
  geom_point(aes(group=measure_group,col=measure_group),size=4)+
  geom_errorbar(aes(ymin=LL,ymax=UL,col=measure_group),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c('NDE for high educ', 
                            'NDE for quintile 3 vs 1 educ',
                            'NDE for quintile 4 vs 1 educ',
                            'NDE for quintile 5 vs 1 educ',
                            'NDE for female sex at birth',
                            'NIE for high educ',
                            'NIE for quintile 3 vs 1 educ',
                            'NIE for quintile 4 vs 1 educ',
                            'NIE for quintile 5 vs 1 educ',
                            'NIE for female sex at birth'))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_effects_Norway_sensitivity.png")

all_mediation_output_Norway_pnm_sensitivity=all_mediation_output_Norway_sensitivity_analysis%>%
  filter(effect%in%c('pm_educ_binary','pm_educ_5_1','pm_educ_4_1',
                     'pm_educ_3_1','pm_sex'))%>%
  mutate(LL_p_non_mediated=1-UL)%>%
  mutate(est_p_non_mediated=1-est)%>%
  mutate(UL_p_non_mediated=1-LL)

all_mediation_output_Norway_pnm_sensitivity$measure=c("prop_non_mediated_educ_binary","prop_non_mediated_educ_quintile_5_1",
                                          "prop_non_mediated_educ_quintile_4_1","prop_non_mediated_educ_quintile_3_1",
                                          "prop_non_mediated_sex")

ggplot(all_mediation_output_Norway_pnm_sensitivity,aes(x=measure,y=est_p_non_mediated))+
  geom_point(aes(group=measure,col=measure),size=4)+
  geom_errorbar(aes(ymin=LL_p_non_mediated,ymax=UL_p_non_mediated,col=measure),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, hjust=1,size=9),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c("proportion non-mediated for high educ",
                            "proportion non-mediated for quintile 3 vs 1 educ",
                            "proportion non-mediated for quintile 4 vs 1 educ",
                            "proportion non-mediated for quintile 5 vs 1 educ",
                            "proportion non-mediated for female sex at birth"))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_pnm_Norway_sensitivity.png")

# UK

## main analysis
all_mediation_output_UK_effects=all_mediation_output_UK%>%
  filter(effect!='de_emp_full_time_vs_part_time')%>%
  filter(effect!='ie_emp_full_time_vs_part_time')%>%
  filter(effect!='pm_emp_full_time_vs_all')%>%
  filter(effect!='pm_emp_full_time_vs_unemp')%>%
  filter(effect!='pm_emp_full_time_vs_part_time')%>%
  filter(effect!='pm_sex')

all_mediation_output_UK_effects$measure=c("NDE_full_time_all_others","NIE_full_time_all_others",
                                          "NDE_full_time_unemp","NIE_full_time_unemp",
                                          "NDE_sex","NIE_sex")

all_mediation_output_UK_effects$measure_group=c("SES_1","SES_1",
                                                    "SES_2","SES_2",
                                                    "Sex","Sex")


ggplot(all_mediation_output_UK_effects,aes(x=measure,y=est))+
  geom_point(aes(group=measure_group,col=measure_group),size=4)+
  geom_errorbar(aes(ymin=LL,ymax=UL,col=measure_group))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  scale_x_discrete(labels=c('NDE for full-time employment', 
                            'NDE for full-time vs unemployment', 
                            'NDE for female sex at birth',
                            'NIE for full-time employment',
                            'NIE for full-time vs unemployment',
                            'NDE for female sex at birth'))+
  ylab("effect size")+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_effects_UK.png")

all_mediation_output_UK_pnm=all_mediation_output_UK%>%
  filter(effect%in%c('pm_emp_full_time_vs_all','pm_emp_full_time_vs_unemp',
                     'pm_sex'))%>%
  mutate(LL_p_non_mediated=1-UL)%>%
  mutate(est_p_non_mediated=1-est)%>%
  mutate(UL_p_non_mediated=1-LL)

all_mediation_output_UK_pnm$measure=c("prop_non_mediated_full_time_all_others",
                                            "prop_non_mediated_full_time_unemp",
                                          "prop_non_mediated_sex")

ggplot(all_mediation_output_UK_pnm,aes(x=measure,y=est_p_non_mediated))+
  geom_point(aes(group=measure,col=measure),size=4)+
  geom_errorbar(aes(ymin=LL_p_non_mediated,ymax=UL_p_non_mediated,col=measure))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, hjust=1,size=9),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  scale_x_discrete(labels=c("proportion non-mediated for full-time employment",
                            "proportion non-mediated for full-time vs unemployment",
                            "proportion non-mediated for female sex at birth"))+
  ylab("effect size")+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_pnm_UK.png")

## sensitivity analysis
all_mediation_output_UK_effects_sensitivity=all_mediation_output_UK_sensitivity_analysis%>%
  filter(effect!='de_emp_full_time_vs_part_time')%>%
  filter(effect!='ie_emp_full_time_vs_part_time')%>%
  filter(effect!='pm_emp_full_time_vs_all')%>%
  filter(effect!='pm_emp_full_time_vs_unemp')%>%
  filter(effect!='pm_emp_full_time_vs_part_time')%>%
  filter(effect!='pm_sex')

all_mediation_output_UK_effects_sensitivity$measure=c("NDE_full_time_all_others","NIE_full_time_all_others",
                                                      "NDE_full_time_unemp","NIE_full_time_unemp",
                                                      "NDE_sex","NIE_sex")

all_mediation_output_UK_effects_sensitivity$measure_group=c("SES_1","SES_1",
                                                "SES_2","SES_2",
                                                "Sex","Sex")

ggplot(all_mediation_output_UK_effects_sensitivity,aes(x=measure,y=est))+
  geom_point(aes(group=measure_group,col=measure_group),size=4)+
  geom_errorbar(aes(ymin=LL,ymax=UL,col=measure_group))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=10),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c('NDE for full-time employment', 
                            'NDE for full-time vs unemployment', 
                            'NDE for female sex at birth',
                            'NIE for full-time employment',
                            'NIE for full-time vs unemployment',
                            'NDE for female sex at birth'))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_effects_UK_sensitivity.png")

all_mediation_output_UK_pnm_sensitivity=all_mediation_output_UK_sensitivity_analysis%>%
  filter(effect%in%c('pm_emp_full_time_vs_all','pm_emp_full_time_vs_unemp',
                     'pm_sex'))%>%
  mutate(LL_p_non_mediated=1-UL)%>%
  mutate(est_p_non_mediated=1-est)%>%
  mutate(UL_p_non_mediated=1-LL)

all_mediation_output_UK_pnm_sensitivity$measure=c("prop_non_mediated_full_time_all_others","prop_non_mediated_full_time_unemp",
  "prop_non_mediated_sex")

ggplot(all_mediation_output_UK_pnm_sensitivity,aes(x=measure,y=est_p_non_mediated))+
  geom_point(aes(group=measure,col=measure),size=4)+
  geom_errorbar(aes(ymin=LL_p_non_mediated,ymax=UL_p_non_mediated,col=measure))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, hjust=1,size=9),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c("proportion non-mediated for full-time employment",
                            "proportion non-mediated for full-time vs unemployment",
                            "proportion non-mediated for female sex at birth"))+
  theme(legend.position='none')
ggsave("./figures/mediation/all_mediation_output_pnm_UK_sensitivity.png")

# RUSSIA

all_mediation_output_Russia_effects=all_mediation_output_Russia%>%
  filter(effect!='pm_emp_full_time_vs_all')%>%
  filter(effect!='pm_sex')

all_mediation_output_Russia_effects$measure=c("NDE_full_time_all_others","NIE_full_time_all_others",
                                                       "NDE_sex","NIE_sex")

all_mediation_output_Russia_effects$measure_group=c("SES","SES","Sex","Sex")

ggplot(all_mediation_output_Russia_effects,aes(x=measure,y=est))+
  geom_point(aes(group=measure_group,col=measure_group),size=4)+
  geom_errorbar(aes(ymin=LL,ymax=UL,col=measure_group),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=12),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c('NDE for full-time employment', 
                            'NDE for female sex at birth',
                            'NIE for full-time employment',
                            'NIE for female sex at birth'))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_effects_Russia.png")

all_mediation_output_Russia_pnm=all_mediation_output_Russia%>%
  filter(effect%in%c('pm_emp_full_time_vs_all',
                     'pm_sex'))%>%
  mutate(LL_p_non_mediated=1-UL)%>%
  mutate(est_p_non_mediated=1-est)%>%
  mutate(UL_p_non_mediated=1-LL)

all_mediation_output_Russia_pnm$measure=c("prop_non_mediated_full_time_all_others",
                                                   "prop_non_mediated_sex")

ggplot(all_mediation_output_Russia_pnm,aes(x=measure,y=est_p_non_mediated))+
  geom_point(aes(group=measure,col=measure),size=4)+
  geom_errorbar(aes(ymin=LL_p_non_mediated,ymax=UL_p_non_mediated,col=measure),width=0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=11),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15))+
  ylab("effect size")+
  scale_x_discrete(labels=c("proportion non-mediated for full-time employment ",
                            "proportion non-mediated for female sex at birth"))+
  theme(legend.position='none')

ggsave("./figures/mediation/all_mediation_output_pnm_Russia.png")




