#Master thesis: Eco Anxiety & Greenwashing Perception
#author: Thomas Kirchmair
#descriptive statistics

#load libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(hrbrthemes)
library(stats)
library(gridExtra)
library(car)
library(misty)
library(ggfortify)
library(psych)
library(lavaan)

#########################
#read data

df <- read_excel("data_mase.xlsx")
View(df)



df <- df[-c(1:2)]

nrow(df)

df_germ <- df[df$country == 'Germany',]
df_southaf <- df[df$country =="South Africa",]

##########################
#age

df$Age <- df$Age_

#plot_age <-
 # ggplot(df, aes(x=Age, fill= country)) +
#  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
#  scale_fill_manual(values=c("#69b3a2", "#404080")) +
#  labs(fill="")
#plot_age

plot_Ageviolin <-
  ggplot(data=df, aes(x=country, y=Age, fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Altersverteilung in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Alter") +
  theme_light()
plot_Ageviolin


df %>% group_by(country) %>%
  dplyr::summarise(n = n(),
            mean(Age),
            sd(Age),
            median(Age))


df_germ$Age <- as.numeric(df_germ$Age)
df_southaf$Age <- as.numeric(df_southaf$Age)
wilcox.test(Age~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

#######################################################
#gender

#plot_gend <-
#  ggplot(data = df,  aes(x = country, fill=Gender))+
#  geom_bar(alpha=0.4)+
#  scale_fill_manual(values = c("orange", "blue", "green"), name = "Gender", 
#                    labels=c('Female', 'Male', 'Other')) +
#  ggtitle("Geschlechterverteilung in Stichprobe pro Land (N = 1035)\n", )+
#  theme_light()
#plot_gend

per_gend <-
df %>% group_by(country, Gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n) * 100, 2)))
per_gend

per_gend$freq <- as.numeric(per_gend$freq)

plot_gend <-
  ggplot(data = per_gend,  aes(x = country, y= freq, fill=Gender))+
  geom_col(alpha=0.4)+
  scale_fill_manual(values = c("orange", "blue", "green"), name = "Gender", 
                    labels=c('Weiblich', 'Männlich', 'Anderes')) +
  ggtitle("Geschlechterverteilung in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("relativer Anteil (%)") +
  theme_light()
plot_gend

per_gend

tab_gend <-
table(per_gend$Gender, per_gend$country)

chisq.test(tab_gend, correct = FALSE)

####################################################################
#education

#Germany
per_germ <- 
  df_germ %>% group_by(Education) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n) * 100, 2)))
per_germ

per_germ <- per_germ %>%
  mutate(edu = dplyr::recode(Education,
                      "1" = "nicht abgeschlossen",
                      "2" = "Grundschulbildung",
                      "3" = "Sekundarstufe I",
                      "4" = "Obere Sekundarstufe",
                      "5" = "Postsekundäre nicht-tertiäre Bildung",
                      "6" = "Tertiäre Kurzausbildungen",
                      "7" = "Bachelor-Abschluss",
                      "8" = "Master-Abschluss",
                      "9" = "Promotion"))


per_germ$edu <- factor(per_germ$edu,
                      levels = c("nicht abgeschlossen", "Grundschulbildung", "Sekundarstufe I",
                                 "Obere Sekundarstufe", "Postsekundäre nicht-tertiäre Bildung",
                                 "Tertiäre Kurzausbildungen", "Bachelor-Abschluss",
                                 "Master-Abschluss", "Promotion"))

per_germ$freq <- as.numeric(per_germ$freq)

plot_edu_germ <-
  ggplot(data = per_germ, aes(x=edu, y= freq)) +
  geom_col(alpha= 0.5, fill ="#69b3a2") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bildungsniveau Deutschland (N = 531)\n", ) +
  xlab("Bildung") +
  ylab("relativer Anteil (%)")
plot_edu_germ

#south africa

per_southaf <- 
  df_southaf %>% group_by(Education) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n) * 100, 2)))
per_southaf

per_southaf <- per_southaf %>%
  mutate(edu = dplyr::recode(Education,
                      "1" = "Grade Ten",
                      "2" = "Grade Four to grade Nine",
                      "3" = "Grade Eleven",
                      "4" = "Grade Twelve",
                      "5" = "National Certificate National Diploma",
                      "6" = "Trade Certificate",
                      "7" = "Occupational Certificate",
                      "8" = "Higher Diploma",
                      "9" = "Honours Degree",
                      "10" = "Bachelors Degree",
                      "11" = "Post Graduate Certificate",
                      "12" = "Masters",
                      "13" = "Doctorate"))

per_southaf$edu <- factor(per_southaf$edu,
                      levels = c("Grade Ten", "Grade Four to grade Nine", "Grade Eleven",
                                 "Grade Twelve", "National Certificate National Diploma",
                                 "Trade Certificate", "Occupational Certificate",
                                 "Higher Diploma", "Honours Degree", "Bachelors Degree",
                                 "Post Graduate Certificate", "Masters",
                                 "Doctorate"))


per_southaf$freq <- as.numeric(per_southaf$freq)

plot_edu_southaf <-
  ggplot(data = per_southaf, aes(x=edu, y = freq)) +
  geom_col(alpha= 0.5, fill ="#404080") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bildungsniveau Südafrika (N = 504)\n", ) +
  xlab("Bildung") +
  ylab("relativer Anteil (%)") +
  ylim(0, 30)
plot_edu_southaf

####germany % south africa



df$edu <- as.character(df$Education)

df[df$country == "Germany", ] <- df[df$country == "Germany", ] %>%
  mutate(edu = dplyr::recode(edu, "1" = "low",
                      "2" = "low",
                      "3" = "middle",
                      "4" = "middle",
                      "5" = "middle",
                      "6" = "middle",
                      "7" = "high",
                      "8" = "high"))


df[df$country == "South Africa", ] <- df[df$country == "South Africa", ] %>%
  mutate(edu = dplyr::recode(edu, "1" = "low",
                      "2" = "low",
                      "3" = "low",
                      "4" = "middle",
                      "5" = "middle",
                      "6" = "middle",
                      "7" = "middle",
                      "8" = "middle",
                      "9" = "middle",
                      "10" = "high",
                      "11" = "high",
                      "12" = "high",
                      "13" = "high"))



df$edu <- as.factor(df$edu)


per_edu <- 
  df %>% group_by(country, edu) %>%
  summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n) * 100, 2)))
per_edu

per_edu$freq <- as.numeric(per_edu$freq)

per_edu$edu<- factor(per_edu$edu, levels = c("low", "middle", "high"))


plot_edu_per <-
  ggplot(data = per_edu,  aes(x = country, y= freq, fill=edu))+
  geom_col(alpha=0.4)+
  scale_fill_manual(values = c("orange", "blue", "green"), name = "Education Level", 
                    labels=c('low', 'middle', 'high')) +
  ggtitle("Bildungslevel in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("relativer Anteil (%)") +
  ylim(0, 100) +
  theme_light()
plot_edu_per

per_edu


table_edu <- table(df$edu, df$country)
  
table_edu
  
chisq.test(table_edu, correct = FALSE)
cohenW(df$edu)
##############################################################
#income
df$Income_1 <- as.character(df$Income_1)

df_inc <- df %>%
  group_by(country, Income_1) %>%
  summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n) * 100, 2)))
df_inc


df_inc$freq <- as.numeric(df_inc$freq)
df_inc$Income_1 <- as.character(df_inc$Income_1)


#plot_income <-
# ggplot(df_inc, aes(x=country, y= n, fill= Income_1,)) +
 # geom_bar(alpha=0.5, position = 'dodge', stat = "identity") +
#  scale_fill_manual(values=c("papayawhip", "lightpink", "pink3", "salmon", "orchid",
#                             "mediumorchid3", "mediumpurple1", "slateblue1", "slateblue4", "navy")) +
#  ggtitle("Einkommen pro Land \n") +
#  xlab("Country") +
#  ylab("relativer Anteil (%)")
#plot_income

df_inc$Income_1 <- factor(df_inc$Income_1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

##
plot_income <-
  ggplot(df_inc, aes(x=country, y= freq, fill= Income_1,)) +
  geom_bar(alpha=0.5, position = 'dodge', stat = "identity") +
  scale_fill_manual(values=c("wheat1", "lightpink", "pink3", "salmon", "orchid",
                             "mediumorchid3", "mediumpurple1", "slateblue1", "slateblue4", "navy"), name = "Einkommen") +
  ggtitle("Einkommensverteilung pro Land (Selbsteinschätzung) \n") +
  xlab("Country") +
  ylab("relativer Anteil (%)") +
  theme_light()
plot_income


table_inc <- table(df$Income_1, df$country)
chisq.test(table_inc, correct = FALSE)

#inc <- as.numeric(df$Income_1)
#wilcox.test(inc~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
####################################################
#social media use

df$Active_Passive_SM_1 <- as.numeric(df$Active_Passive_SM_1)
df$Active_Passive_SM_2 <- as.numeric(df$Active_Passive_SM_2)
df$Active_Passive_SM_3 <- as.numeric(df$Active_Passive_SM_3)
df$Active_Passive_SM_4 <- as.numeric(df$Active_Passive_SM_4)
df$Active_Passive_SM_5 <- as.numeric(df$Active_Passive_SM_5)
df$Active_Passive_SM_6 <- as.numeric(df$Active_Passive_SM_6)


###########
#confirmatory factor analysis: SM Use
conf_sm_model <- 'act =~ Active_Passive_SM_1 + Active_Passive_SM_2 + Active_Passive_SM_3
            pass=~ Active_Passive_SM_4 + Active_Passive_SM_5 + Active_Passive_SM_6'

conf_sm <- cfa(conf_sm_model, data=df) 
summary(conf_sm,fit.measures = TRUE, standardized = TRUE)


conf_sm_model_1 <- 'sm =~ Active_Passive_SM_1 + Active_Passive_SM_2 + Active_Passive_SM_3
            + Active_Passive_SM_4 + Active_Passive_SM_5 + Active_Passive_SM_6'

conf_sm_1 <- cfa(conf_sm_model_1, data=df) 
summary(conf_sm_1, fit.measures = TRUE, standardized = TRUE)


anova(conf_sm, conf_sm_1)

#################
df$SM_use_mean <- rowMeans(df[24:29])

sm <- df %>% group_by(country) %>%
  summarise(mean_sm_mean = mean(SM_use_mean),
            median_sm_mean = median(SM_use_mean),
            sd_sm_mean = sd(SM_use_mean))
sm

wilcox.test(SM_use_mean~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

#############
df$SM_use_act <- rowMeans(df[24:26])


sm_act <- df %>% group_by(country) %>%
  summarise(mean_sm_act = mean(SM_use_act),
            median_sm_act = median(SM_use_act),
            sd_sm_act = sd(SM_use_act))
sm_act

wilcox.test(SM_use_act~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

psych::alpha(df[24:26])
############
df$SM_use_pass <- rowMeans(df[27:29])
sm_pass <- df %>% group_by(country) %>%
  summarise(mean_sm_pass = mean(SM_use_pass),
            median_sm_pass = median(SM_use_pass),
            sd_sm_pass = sd(SM_use_pass))
sm_pass

wilcox.test(SM_use_pass~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
psych::alpha(df[27:29])
######



plot_SM_act_violin <-
  ggplot(data=df, aes(x=country, y=SM_use_act, fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Aktive Social Media Nutzung für Umweltthemen in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Active Social Media Use") +
  theme_light()
plot_SM_act_violin

plot_SM_pass_violin <-
  ggplot(data=df, aes(x=country, y=SM_use_pass, fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Passive Social Media Nutzung für Umweltthemen in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Passive Social Media Use") +
  theme_light()
plot_SM_pass_violin


#plots_SM <- grid.arrange(plot_SM_act_violin, plot_SM_pass_violin, ncol = 2) 

plot_SM_mean_violin <-
  ggplot(data=df, aes(x=country, y=SM_use_mean, fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Generelle Social Media Nutzung für Umweltthemen in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Generel Social Media Use") +
  theme_light()
plot_SM_mean_violin

#######################################
#Eco Anxiety


df$Eco_Anxiety___3_neg_1 <- as.numeric(df$Eco_Anxiety___3_neg_1)
df$Eco_Anxiety___3_neg_2 <- as.numeric(df$Eco_Anxiety___3_neg_2)
df$Eco_Anxiety___3_neg_3 <- as.numeric(df$Eco_Anxiety___3_neg_3)
df$Eco_Anxiety___3_neg_4 <- as.numeric(df$Eco_Anxiety___3_neg_4)
df$Eco_Anxiety___3_neg_5 <- as.numeric(df$Eco_Anxiety___3_neg_5)
df$Eco_Anxiety___3_neg_6 <- as.numeric(df$Eco_Anxiety___3_neg_6)
df$Eco_Anxiety___3_neg_7 <- as.numeric(df$Eco_Anxiety___3_neg_7)
df$Eco_Anxiety___3_neg_8 <- as.numeric(df$Eco_Anxiety___3_neg_8)
df$Eco_Anxiety___3_neg_9 <- as.numeric(df$Eco_Anxiety___3_neg_9)
df$Eco_Anxiety___3_neg_10 <- as.numeric(df$Eco_Anxiety___3_neg_10)
df$Eco_Anxiety___3_neg_11 <- as.numeric(df$Eco_Anxiety___3_neg_11)
df$Eco_Anxiety___3_neg_12 <- as.numeric(df$Eco_Anxiety___3_neg_12)
df$Eco_Anxiety___3_neg_13 <- as.numeric(df$Eco_Anxiety___3_neg_13)



df$Anx_mean <- rowMeans(df[109:121])
df$Anx_aff <- rowMeans(df[109:112])
df$Anx_rum <- rowMeans(df[113:115])
df$Anx_behav <- rowMeans(df[116:118])
df$Anx_impact <- rowMeans(df[119:121])


psych::alpha(df[109:112])
psych::alpha(df[113:115])
psych::alpha(df[116:118])
psych::alpha(df[119:121])
######################################################

conf_eco_anx_model <- 'aff =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      rum =~ Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      behav =~ Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      impact =~ Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco <- cfa(conf_eco_anx_model, data=df) 
summary(conf_eco, standardized = TRUE)



conf_eco_anx_model1 <- 'eco_anx =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      + Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      + Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      + Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco_1 <- cfa(conf_eco_anx_model1, data=df)
summary(conf_eco_1, fit.measures = TRUE, standardized = TRUE)

anova(conf_eco, conf_eco_1)

#################################Germany

conf_eco_anx_model_germ <- 'aff =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      rum =~ Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      behav =~ Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      impact =~ Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco_germ <- cfa(conf_eco_anx_model_germ, data=df_germ) 
summary(conf_eco_germ, fit.measures = TRUE, standardized = TRUE)



conf_eco_anx_model_germ_1 <- 'eco_anx =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      + Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      + Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      + Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco_germ_1 <- cfa(conf_eco_anx_model_germ_1, data=df_germ)
summary(conf_eco_germ_1, fit.measures = TRUE, standardized = TRUE)

anova(conf_eco_germ, conf_eco_germ_1)

###################south africa


conf_eco_anx_model_sa <- 'aff =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      rum =~ Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      behav =~ Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      impact =~ Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco_sa <- cfa(conf_eco_anx_model_sa, data=df_southaf) 
summary(conf_eco_sa, fit.measures = TRUE, standardized = TRUE)



conf_eco_anx_model_sa_1 <- 'eco_anx =~ Eco_Anxiety___3_neg_1 + Eco_Anxiety___3_neg_2 + Eco_Anxiety___3_neg_3 + Eco_Anxiety___3_neg_4
                      + Eco_Anxiety___3_neg_5 + Eco_Anxiety___3_neg_6 + Eco_Anxiety___3_neg_7
                      + Eco_Anxiety___3_neg_8 + Eco_Anxiety___3_neg_9 + Eco_Anxiety___3_neg_10
                      + Eco_Anxiety___3_neg_11 + Eco_Anxiety___3_neg_12 + Eco_Anxiety___3_neg_13 '

conf_eco_sa_1 <- cfa(conf_eco_anx_model_sa_1, data=df_southaf)
summary(conf_eco_sa_1, fit.measures = TRUE, standardized = TRUE)

anova(conf_eco_sa, conf_eco_sa_1)

#####################
plot_Anx_aff <-
  ggplot(data=df, aes(x=country, y=Anx_aff,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Affektive Symptome in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Affektive Symptome") +
  theme_light()
plot_Anx_aff

desc_anx_aff <- df %>% group_by(country) %>%
  summarise(mean_anx_aff = mean(Anx_aff),
            median_anx_aff = median(Anx_aff),
            sd_anx_aff = sd(Anx_aff))
desc_anx_aff

wilcox.test(Anx_aff~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
##################

plot_Anx_rum <-
  ggplot(data=df, aes(x=country, y=Anx_rum,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Rumination in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Rumination") +
  theme_light()
plot_Anx_rum

desc_anx_rum <- df %>% group_by(country) %>%
  summarise(mean_anx_rum = mean(Anx_rum),
            median_anx_rum = median(Anx_rum),
            sd_anx_rum = sd(Anx_rum))
desc_anx_rum

wilcox.test(Anx_rum~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

#################
plot_Anx_behav <-
  ggplot(data=df, aes(x=country, y=Anx_behav,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Verhaltenssymptome in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Verhaltenssymptome") +
  theme_light()
plot_Anx_behav

desc_anx_behav <- df %>% group_by(country) %>%
  summarise(mean_anx_behav = mean(Anx_behav),
            median_anx_behav = median(Anx_behav),
            sd_anx_behav = sd(Anx_behav))
desc_anx_behav

wilcox.test(Anx_behav~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

#############
plot_Anx_impact <-
  ggplot(data=df, aes(x=country, y=Anx_impact,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Eco Anxiety aufgrund des persönlichen Einflusses in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Persönlicher Einfluss") +
  theme_light()
plot_Anx_impact

desc_anx_impact <- df %>% group_by(country) %>%
  summarise(mean_anx_impact = mean(Anx_impact),
            median_anx_impact = median(Anx_impact),
            sd_anx_impact = sd(Anx_impact))
desc_anx_impact

wilcox.test(Anx_impact~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

#################
plot_Anx_mean <-
  ggplot(data=df, aes(x=country, y=Anx_mean,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Eco Anxiety in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Eco Anxiety") +
  theme_light()
plot_Anx_mean

desc_anx <- df %>% group_by(country) %>%
  summarise(mean_anx_ = mean(Anx_mean),
            median_anx = median(Anx_mean),
            sd_anx = sd(Anx_mean))
desc_anx

wilcox.test(Anx_mean~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
########################################################################################
#Psychological Distance

df$Psychological_Dist_1 <- as.numeric(df$Psychological_Dist_1)
df$Psychological_Dist_2 <- as.numeric(df$Psychological_Dist_2)
df$Psychological_Dist_3 <- as.numeric(df$Psychological_Dist_3)

df$Dist_mean <- rowMeans(df[142:144])

plot_dist <-
  ggplot(data=df, aes(x=country, y=Dist_mean,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("Psychologische Distanz in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("Psychologische Distanz") +
  theme_light()
plot_dist

desc_dist <- df %>% group_by(country) %>%
  summarise(mean_dist = mean(Dist_mean),
            median_dist = median(Dist_mean),
            sd_dist = sd(Dist_mean))
desc_dist

wilcox.test(Dist_mean~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)


psych::alpha(subset(df, select = c(Psychological_Dist_1, Psychological_Dist_2, Psychological_Dist_3)))
###############################################################################################
#Umweltbewusstsein

df$NEP_1 <- as.numeric(df$NEP_1)
df$NEP_2 <- as.numeric(df$NEP_2)
df$NEP_3 <- as.numeric(df$NEP_3)
df$NEP_4 <- as.numeric(df$NEP_4)

df$NEP_5 <- as.numeric(df$NEP_5)
df$NEP_6 <- as.numeric(df$NEP_6)
df$NEP_7 <- as.numeric(df$NEP_7)
df$NEP_8 <- as.numeric(df$NEP_8)


df$NEP_1_rec <- item.reverse(df$NEP_1, min = 1, max = 8)
df$NEP_2_rec <- item.reverse(df$NEP_2, min = 1, max = 8)
df$NEP_3_rec <- item.reverse(df$NEP_3, min = 1, max = 8)
df$NEP_4_rec <- item.reverse(df$NEP_4, min = 1, max = 8)

df$DSP <- rowMeans(df[128:131])
df$NEP <- rowMeans(df[132:135])

#####
conf_nep_model <- 'dsp =~ NEP_1_rec + NEP_2_rec + NEP_3_rec + NEP_4_rec
                      nep =~ NEP_5 + NEP_6 + NEP_7 + NEP_8 '


conf_nep <- cfa(conf_nep_model, data=df) 
summary(conf_nep, fit.measures = TRUE, standardized = TRUE)



conf_nep_model_1 <- 'fact =~ NEP_1_rec + NEP_2_rec + NEP_3_rec + NEP_4_rec +
                      NEP_5 + NEP_6 + NEP_7 + NEP_8 '


conf_nep_1 <- cfa(conf_nep_model_1, data=df)
summary(conf_nep_1, fit.measures = TRUE, standardized = TRUE)

anova(conf_nep, conf_nep_1)

psych::alpha(subset(df, select = c(NEP_1_rec, NEP_2_rec, NEP_3_rec, NEP_4_rec)))
#psych::alpha(subset(df, select = c(NEP_1_rec, NEP_2_rec, NEP_3_rec)))
psych::alpha(subset(df, select = c(NEP_5, NEP_6, NEP_7, NEP_8)))
###############



plot_dsp <-
  ggplot(data=df, aes(x=country, y=DSP,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("DSP in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("DSP") +
  theme_light()
plot_dsp

desc_dsp <- df %>% group_by(country) %>%
  summarise(mean_dsp = mean(DSP),
            median_dsp = median(DSP),
            sd_dsp = sd(DSP))
desc_dsp

wilcox.test(DSP~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
######
plot_nep <-
  ggplot(data=df, aes(x=country, y=NEP,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("NEP in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("NEP") +
  theme_light()
plot_nep

desc_nep <- df %>% group_by(country) %>%
  summarise(mean_nep = mean(NEP),
            median_nep = median(NEP),
            sd_nep = sd(NEP))
desc_nep

wilcox.test(NEP~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
#############################################
#greenwashing literacy

#substantial claims

which(colnames(df)=="GL1_1")
which(colnames(df)=="G24_1")


df$GL1_1 <- as.numeric(df$GL1_1)
df$G5_1 <- as.numeric(df$G5_1)
df$G9_1 <- as.numeric(df$G9_1)
df$G13_1 <- as.numeric(df$G13_1)
df$G17_1 <- as.numeric(df$G17_1)
df$G21_1 <- as.numeric(df$G21_1)



df$gw_sub <- rowMeans(subset(df, select = c(GL1_1, G5_1, G9_1, G13_1, G17_1, G21_1), na.rm = TRUE))
psych::alpha(subset(df, select = c(GL1_1, G5_1, G9_1, G13_1, G17_1, G21_1)))

#vague

df$GL2_1 <- as.numeric(df$GL2_1)
df$G6_1 <- as.numeric(df$G6_1)
df$G10_1 <- as.numeric(df$G10_1)
df$G14_1 <- as.numeric(df$G14_1)
df$G18_1 <- as.numeric(df$G18_1)
df$G22_1 <- as.numeric(df$G22_1)



df$gw_vag <- rowMeans(subset(df, select = c(GL2_1, G6_1, G10_1, G14_1, G18_1, G22_1), na.rm = TRUE))
psych::alpha(subset(df, select = c(GL2_1, G6_1, G10_1, G14_1, G18_1, G22_1)))
#false

df$GL3_1 <- as.numeric(df$GL3_1)
df$G7_1 <- as.numeric(df$G7_1)
df$G11_1 <- as.numeric(df$G11_1)
df$G15_1 <- as.numeric(df$G15_1)
df$G19_1 <- as.numeric(df$G19_1)
df$G23_1 <- as.numeric(df$G23_1)



df$gw_false <- rowMeans(subset(df, select = c(GL3_1, G7_1, G11_1, G15_1, G19_1, G23_1), na.rm = TRUE))
psych::alpha(subset(df, select = c(GL3_1, G7_1, G11_1, G15_1, G19_1, G23_1)))
#omission

df$GL4_1 <- as.numeric(df$GL4_1)
df$G8_1 <- as.numeric(df$G8_1)
df$G12_1 <- as.numeric(df$G12_1)
df$G16_1 <- as.numeric(df$G16_1)
df$G20_1 <- as.numeric(df$G20_1)
df$G24_1 <- as.numeric(df$G24_1)



df$gw_om <- rowMeans(subset(df, select = c(GL4_1, G8_1, G12_1, G16_1, G20_1, G24_1), na.rm = TRUE))
psych::alpha(subset(df, select = c(GL4_1, G8_1, G12_1, G16_1, G20_1, G24_1)))
#################
#sub
plot_gw_sub <-
  ggplot(data=df, aes(x=country, y=gw_sub,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("GW substantial in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("GW substantial") +
  theme_light()
plot_gw_sub


desc_gw_sub <- df %>% dplyr::group_by(country) %>%
  summarise(mean_gw_sub = mean(gw_sub),
            median_gw_sub = median(gw_sub),
            sd_gw_sub = sd(gw_sub))
desc_gw_sub

wilcox.test(gw_sub~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
####

#vague

plot_gw_vag <-
  ggplot(data=df, aes(x=country, y=gw_vag,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("GW vague in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("GW vague") +
  theme_light()
plot_gw_vag

desc_gw_vag <- df %>% group_by(country) %>%
  summarise(mean_gw_vag = mean(gw_vag),
            median_gw_vag = median(gw_vag),
            sd_gw_vag = sd(gw_vag))
desc_gw_vag


wilcox.test(gw_vag~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
################
#false
plot_gw_false <-
  ggplot(data=df, aes(x=country, y=gw_false,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("GW false in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("GW false") +
  theme_light()
plot_gw_false

desc_gw_false <- df %>% group_by(country) %>%
  summarise(mean_gw_false = mean(gw_false),
            median_gw_false = median(gw_false),
            sd_gw_false = sd(gw_false))
desc_gw_false

wilcox.test(gw_false~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
################
#omission
plot_gw_om <-
  ggplot(data=df, aes(x=country, y=gw_om,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("GW omission in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("GW omission") +
  theme_light()
plot_gw_om

desc_gw_om <- df %>% group_by(country) %>%
  summarise(mean_gw_om = mean(gw_om),
            median_gw_om = median(gw_om),
            sd_gw_om = sd(gw_om))
desc_gw_om

wilcox.test(gw_om~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)
#####################################

df$gw_gesamt <- rowMeans(subset(df, select =c(GL3_1, G7_1, G11_1, G15_1, G19_1, G23_1, GL2_1, G6_1, G10_1, G14_1, G18_1, 
                      G22_1, GL4_1, G8_1, G12_1, G16_1, G20_1, G24_1)))

plot_gw_gesamt <-
  ggplot(data=df, aes(x=country, y=gw_gesamt,  fill = country)) +
  geom_violin(trim = FALSE, alpha=0.5) +
  geom_boxplot(width =0.1, alpha = 0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), guide = "none") +
  ggtitle("GW Perception in Stichprobe pro Land (N = 1035)\n", )+
  xlab("Country") +
  ylab("GW Perception") +
  theme_light()
plot_gw_gesamt

desc_gw_gesamt <- df %>% group_by(country) %>%
  summarise(mean_gw_gesamt = mean(gw_gesamt),
            median_gw_gesamt = median(gw_gesamt),
            sd_gw_gesamt = sd(gw_gesamt))
desc_gw_gesamt


wilcox.test(gw_gesamt~country, data = df, exact = FALSE, correct = FALSE, conf.int = FALSE)

###################


df_gw <- df[74:97]

summary(df_gw)

gw_factor <- stats::factanal(df_gw, factors = 4, rotation = "varimax")
gw_factor
###############

conf_gw_model <- 'subst =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
            vag=~ GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
            fals =~ GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
            omis =~ GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'
  
conf_gw <- cfa(conf_gw_model, data=df) 
summary(conf_gw,fit.measures = TRUE, standardized = TRUE)


conf_gw_model_1 <- 'gw =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
            + GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
            + GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
            + GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'

conf_gw_1 <- cfa(conf_gw_model_1, data=df) 
summary(conf_gw_1, fit.measures = TRUE, standardized = TRUE)

anova(conf_gw, conf_gw_1)


###############germany


#conf_gw_model_germ <- 'subst =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
#            vag=~ GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
#            fals =~ GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
#            omis =~ GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'

#conf_gw_germ <- cfa(conf_gw_model_germ, data=df_germ) 
#summary(conf_gw_germ, fit.measures = TRUE, standardized = TRUE)



#RMconf_gw_model_germ_1 <- 'gw =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
#            + GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
#            + GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
#           + GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'

#conf_gw_germ_1 <- cfa(conf_gw_model_germ_1, data=df_germ)
#summary(conf_gw_germ_1, fit.measures = TRUE, standardized = TRUE)

#anova(conf_gw_germ, conf_gw_germ_1)

##########################south africa

#conf_gw_model_sa <- 'subst =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
#            vag=~ GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
#            fals =~ GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
#            omis =~ GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'

#conf_gw_sa <- cfa(conf_gw_model_sa, data=df_southaf) 
#summary(conf_gw_sa, fit.measures = TRUE, standardized = TRUE)



#conf_gw_model_sa_1 <- 'gw =~ GL1_1 + G5_1 + G9_1 + G13_1 + G17_1 + G21_1
#            + GL2_1 + G6_1 + G10_1 + G14_1 + G18_1 + G22_1
#            + GL3_1 + G7_1 + G11_1 + G15_1 + G19_1 + G23_1
#            + GL4_1 + G8_1 + G12_1 + G16_1 + G20_1 + G24_1'

#conf_gw_sa_1 <- cfa(conf_gw_model_sa_1, data=df_southaf)
#summary(conf_gw_sa_1, fit.measures = TRUE, standardized = TRUE)

#anova(conf_gw_sa, conf_gw_sa_1)


##########################