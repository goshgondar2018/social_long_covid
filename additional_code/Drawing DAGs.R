# ref: https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html

library(dagitty)
library(ggdag)
library(ggplot2)

# NORWAY

DAG1_Norway_plot <- DAG1_Norway %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG1_Norway_plot

ggsave("./figures/DAG_1_Norway.png")

DAG2_Norway <- dagitty('dag {
  "Psychological Disorder" [pos="-0.2,1.3"]
  "Type 2 Diabetes" [pos="-0.2,0.9"]
  "Type 1 Diabetes" [pos="-0.2,0.5"]
  "Chronic Pulmonary Disease (not asthma)" [pos="-0.2,0.1"]
   Hypertension [pos="-0.2,-0.3"]
  "Chronic Cardiac Disease (not htn)" [pos="-0.2,-0.70"]
   Asthma [pos="-0.2,-1.1"]
  "Diabetes (type not specified)" [pos="-0.2,-1.5"]
  Smoking [pos="-0.2,-1.9"]
  "COVID Vaccination Status" [pos="-0.2,-2.3"]
  "SES indicator" [pos="-2,-0.2"]
  "Sex" [exposure,pos="-1.25,-0.2"]
  "Long COVID QALDs" [outcome,pos="1.192,-0.2"]
  "Chronic Cardiac Disease (not htn)" -> "Long COVID QALDs"
  "Chronic Pulmonary Disease (not asthma)" -> "Long COVID QALDs"
  "Diabetes (type not specified)" -> "Long COVID QALDs"
  "Psychological Disorder" -> "Long COVID QALDs"
  "Sex" -> "Chronic Cardiac Disease (not htn)"
  "Sex" -> "Chronic Pulmonary Disease (not asthma)"
  "Sex" -> "Diabetes (type not specified)"
  "Sex" -> "Psychological Disorder"
  "Sex" -> "Type 1 Diabetes"
  "Sex" -> "Type 2 Diabetes"
  "Sex" -> Asthma
  "Sex" -> Hypertension
  "Sex" -> Smoking
  "Sex" -> "COVID Vaccination Status"
  "Type 1 Diabetes" -> "Long COVID QALDs"
  "Type 2 Diabetes" -> "Long COVID QALDs"
  Asthma -> "Long COVID QALDs"
  Hypertension -> "Long COVID QALDs"
  "SES indicator" -> "Chronic Cardiac Disease (not htn)"
  "SES indicator" -> "Chronic Pulmonary Disease (not asthma)"
  "SES indicator" -> "Diabetes (type not specified)"
  "SES indicator" -> "Long COVID QALDs"
  "SES indicator" -> "Psychological Disorder"
  "SES indicator" -> "Sex"
  "SES indicator" -> "Type 1 Diabetes"
  "SES indicator" -> "Type 2 Diabetes"
  "SES indicator" -> Asthma
  "SES indicator" -> Hypertension
  "SES indicator" -> Smoking
  "SES indicator" -> "COVID Vaccination Status"
  Smoking -> "Long COVID QALDs"
}')

DAG2_Norway_plot <- DAG2_Norway %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG2_Norway_plot

ggsave("./figures/DAG_2_Norway.png")

# UK

DAG1_UK <- dagitty('dag {
  "Chronic Cardiac Disease (not htn)" [pos="-0.2,-0.704"]
  "Chronic Pulmonary Disease (not asthma)" [pos="-0.2,-0.005"]
  "Diabetes (type not specified)" [pos="-0.2,-1.385"]
  "Ischaemic Heart Disease" [pos="-0.2,0.357"]
  "Long COVID QALDs" [outcome,pos="1.5,0.143"]
  "Psychological Disorder" [pos="-0.2,1.419"]
  "SES indicator" [exposure,pos="-1.4,0.179"]
  "Type 2 Diabetes" [pos="-0.2,0.701"]
  "Diabetes (type not specified)" [pos="-0.2,1.069"]
  Age [pos="-2.240,-0.428"]
  Asthma [pos="-0.2,-1.048"]
  Hypertension [pos="-0.2,-0.361"]
  Obesity [pos="-0.2,1.781"]
  Sex [pos="-2.240,0.615"]
  Smoking [pos="-0.2,-1.383"]
  "Antiviral treatment" [pos="-0.2,-1.718"]
  "Chronic Cardiac Disease (not htn)" -> "Long COVID QALDs"
  "Chronic Pulmonary Disease (not asthma)" -> "Long COVID QALDs"
  "Diabetes (type not specified)" -> "Long COVID QALDs"
  "Ischaemic Heart Disease" -> "Long COVID QALDs"
  "Psychological Disorder" -> "Long COVID QALDs"
  "SES indicator" -> "Chronic Cardiac Disease (not htn)"
  "SES indicator" -> "Chronic Pulmonary Disease (not asthma)"
  "SES indicator" -> "Diabetes (type not specified)"
  "SES indicator" -> "Ischaemic Heart Disease"
  "SES indicator" -> "Psychological Disorder"
  "SES indicator" -> "Type 2 Diabetes"
  "SES indicator" -> Asthma
  "SES indicator" -> Hypertension
  "SES indicator" -> Obesity
  "SES indicator" -> Smoking
  "SES indicator" -> "Antiviral treatment"
  "Type 2 Diabetes" -> "Long COVID QALDs"
  Age -> "Chronic Cardiac Disease (not htn)"
  Age -> "Chronic Pulmonary Disease (not asthma)"
  Age -> "Diabetes (type not specified)"
  Age -> "Ischaemic Heart Disease"
  Age -> "Long COVID QALDs"
  Age -> "Psychological Disorder"
  Age -> "SES indicator"
  Age -> "Type 2 Diabetes"
  Age -> Asthma
  Age -> Hypertension
  Age -> Obesity
  Age -> Smoking
  Age -> "Antiviral treatment"
  Asthma -> "Long COVID QALDs"
  Hypertension -> "Long COVID QALDs"
  Obesity -> "Long COVID QALDs"
  Sex -> "Chronic Cardiac Disease (not htn)"
  Sex -> "Chronic Pulmonary Disease (not asthma)"
  Sex -> "Diabetes (type not specified)"
  Sex -> "Ischaemic Heart Disease"
  Sex -> "Long COVID QALDs"
  Sex -> "Psychological Disorder"
  Sex -> "SES indicator"
  Sex -> "Type 2 Diabetes"
  Sex -> Asthma
  Sex -> Hypertension
  Sex -> Obesity
  Sex -> Smoking
  Sex -> "Antiviral treatment"
  Smoking -> "Long COVID QALDs"
  "Antiviral treatment" -> "Long COVID QALDs"
}')

DAG1_UK_plot <- DAG1_UK %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG1_UK_plot

ggsave("./figures/DAG_1_UK.png")

DAG2_UK <- dagitty('dag {
  "Chronic Cardiac Disease (not htn)" [pos="-0.2,-0.704"]
  "Chronic Pulmonary Disease (not asthma)" [pos="-0.2,-0.005"]
  "Diabetes (type not specified)" [pos="-0.2,-1.385"]
  "Ischaemic Heart Disease" [pos="-0.2,0.357"]
  "Long COVID QALDs" [outcome,pos="1.5,0.143"]
  "Psychological Disorder" [pos="-0.2,1.419"]
  "Sex" [exposure,pos="-1.25,0.179"]
  "Type 2 Diabetes" [pos="-0.2,0.701"]
  "Diabetes (type not specified)" [pos="-0.2,1.069"]
  Asthma [pos="-0.2,-1.048"]
  Hypertension [pos="-0.2,-0.361"]
  Obesity [pos="-0.2,1.781"]
  "SES indicator" [pos="-2,0.179"]
  Smoking [pos="-0.2,-1.383"]
  "Antiviral treatment" [pos="-0.2,-1.718"]
  "Chronic Cardiac Disease (not htn)" -> "Long COVID QALDs"
  "Chronic Pulmonary Disease (not asthma)" -> "Long COVID QALDs"
  "Diabetes (type not specified)" -> "Long COVID QALDs"
  "Ischaemic Heart Disease" -> "Long COVID QALDs"
  "Psychological Disorder" -> "Long COVID QALDs"
  "Sex" -> "Chronic Cardiac Disease (not htn)"
  "Sex" -> "Chronic Pulmonary Disease (not asthma)"
  "Sex" -> "Diabetes (type not specified)"
  "Sex" -> "Ischaemic Heart Disease"
  "Sex" -> "Psychological Disorder"
  "Sex" -> "Type 2 Diabetes"
  "Sex" -> Asthma
  "Sex" -> Hypertension
  "Sex" -> Obesity
  "Sex" -> Smoking
  "Sex" -> "Antiviral treatment"
  "Type 2 Diabetes" -> "Long COVID QALDs"
  Asthma -> "Long COVID QALDs"
  Hypertension -> "Long COVID QALDs"
  Obesity -> "Long COVID QALDs"
  "SES indicator" -> "Chronic Cardiac Disease (not htn)"
  "SES indicator" -> "Chronic Pulmonary Disease (not asthma)"
  "SES indicator" -> "Diabetes (type not specified)"
  "SES indicator" -> "Ischaemic Heart Disease"
  "SES indicator" -> "Long COVID QALDs"
  "SES indicator" -> "Psychological Disorder"
  "SES indicator" -> "SES indicator"
  "SES indicator" -> "Type 2 Diabetes"
  "SES indicator" -> Asthma
  "SES indicator" -> Hypertension
  "SES indicator" -> Obesity
  "SES indicator" -> Smoking
  "SES indicator" -> "Antiviral treatment"
  Smoking -> "Long COVID QALDs"
}')

DAG2_UK_plot <- DAG2_UK %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG2_UK_plot

ggsave("./figures/DAG_2_UK.png")

# RUSSIA

DAG1_Russia <- dagitty('dag {
  "Chronic Cardiac Disease (not htn)" [pos="-0.2,-0.581"]
  "Chronic Pulmonary Disease (not asthma)" [pos="-0.2,0.219"]
  "Diabetes (type not specified)" [pos="-0.2,-1.385"]
  "Long COVID QALDs" [outcome,pos="1.192,0.143"]
  "SES indicator" [exposure,pos="-1.4,0.179"]
  "Type 2 Diabetes" [pos="-0.2,0.619"]
  "Diabetes (type not specified)" [pos="-0.2,1.019"]
  Age [pos="-2.240,-0.428"]
  Asthma [pos="-0.2,-0.981"]
  Hypertension [pos="-0.2,-0.181"]
  Obesity [pos="-0.2,1.419"]
  Sex [pos="-2.240,0.615"]
  Smoking [pos="-0.2,-1.381"]
  "Chronic Cardiac Disease (not htn)" -> "Long COVID QALDs"
  "Chronic Pulmonary Disease (not asthma)" -> "Long COVID QALDs"
  "Diabetes (type not specified)" -> "Long COVID QALDs"
  "SES indicator" -> "Chronic Cardiac Disease (not htn)"
  "SES indicator" -> "Chronic Pulmonary Disease (not asthma)"
  "SES indicator" -> "Diabetes (type not specified)"
  "SES indicator" -> "Type 2 Diabetes"
  "SES indicator" -> Asthma
  "SES indicator" -> Hypertension
  "SES indicator" -> Obesity
  "SES indicator" -> Smoking
  "Type 2 Diabetes" -> "Long COVID QALDs"
  Age -> "Chronic Cardiac Disease (not htn)"
  Age -> "Chronic Pulmonary Disease (not asthma)"
  Age -> "Diabetes (type not specified)"
  Age -> "Long COVID QALDs"
  Age -> "SES indicator"
  Age -> "Type 2 Diabetes"
  Age -> Asthma
  Age -> Hypertension
  Age -> Obesity
  Age -> Smoking
  Asthma -> "Long COVID QALDs"
  Hypertension -> "Long COVID QALDs"
  Obesity -> "Long COVID QALDs"
  Sex -> "Chronic Cardiac Disease (not htn)"
  Sex -> "Chronic Pulmonary Disease (not asthma)"
  Sex -> "Diabetes (type not specified)"
  Sex -> "Long COVID QALDs"
  Sex -> "SES indicator"
  Sex -> "Type 2 Diabetes"
  Sex -> Asthma
  Sex -> Hypertension
  Sex -> Obesity
  Sex -> Smoking
  Smoking -> "Long COVID QALDs"
}')

DAG1_Russia_plot <- DAG1_Russia %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG1_Russia_plot

ggsave("./figures/DAG_1_Russia.png")

DAG2_Russia <- dagitty('dag {
  "Chronic Cardiac Disease (not htn)" [pos="-0.2,-0.581"]
  "Chronic Pulmonary Disease (not asthma)" [pos="-0.2,0.219"]
  "Diabetes (type not specified)" [pos="-0.2,-1.385"]
  "Long COVID QALDs" [outcome,pos="1.192,0.143"]
  "Type 2 Diabetes" [pos="-0.2,0.619"]
  "Diabetes (type not specified)" [pos="-0.2,1.019"]
  "Sex" [exposure,pos="-1.2,0.179"]
  Asthma [pos="-0.2,-0.981"]
  Hypertension [pos="-0.2,-0.181"]
  Obesity [pos="-0.2,1.419"]
  "SES indicator" [pos="-2,0.179"]
  Smoking [pos="-0.2,-1.381"]
  Country -> "Long COVID QALDs"
  "Chronic Cardiac Disease (not htn)" -> "Long COVID QALDs"
  "Chronic Pulmonary Disease (not asthma)" -> "Long COVID QALDs"
  "Diabetes (type not specified)" -> "Long COVID QALDs"
  "Type 1 Diabetes" -> "Long COVID QALDs"
  Country -> "Chronic Cardiac Disease (not htn)"
  Country -> "Chronic Pulmonary Disease (not asthma)"
  Country -> "Diabetes (type not specified)"
  Country -> "Type 1 Diabetes"
  Country -> "Type 2 Diabetes"
  Country -> Asthma
  Country -> Hypertension
  Country -> Obesity
  Country -> Smoking
  "Sex" -> "Chronic Cardiac Disease (not htn)"
  "Sex" -> "Chronic Pulmonary Disease (not asthma)"
  "Sex" -> "Diabetes (type not specified)"
  "Sex" -> "Type 1 Diabetes"
  "Sex" -> "Type 2 Diabetes"
  "Sex" -> Asthma
  "Sex" -> Hypertension
  "Sex" -> Obesity
  "Sex" -> Smoking
  "Type 2 Diabetes" -> "Long COVID QALDs"
  Asthma -> "Long COVID QALDs"
  Hypertension -> "Long COVID QALDs"
  Obesity -> "Long COVID QALDs"
  "SES indicator" -> "Chronic Cardiac Disease (not htn)"
  "SES indicator" -> "Chronic Pulmonary Disease (not asthma)"
  "SES indicator" -> "Diabetes (type not specified)"
  "SES indicator" -> "Type 1 Diabetes"
  "SES indicator" -> "Long COVID QALDs"
  "SES indicator" -> "SES indicator"
  "SES indicator" -> "Type 2 Diabetes"
  "SES indicator" -> Asthma
  "SES indicator" -> Hypertension
  "SES indicator" -> Obesity
  "SES indicator" -> Smoking
  Smoking -> "Long COVID QALDs"
}')


DAG2_Russia_plot <- DAG2_Russia %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour='grey') +
  theme_dag()+
  geom_dag_node(col='orange') +
  geom_dag_text(col='black',size=3)
DAG2_Russia_plot

ggsave("./figures/DAG_2_Russia.png")




