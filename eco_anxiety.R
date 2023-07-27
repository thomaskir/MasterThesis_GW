#Master thesis: Eco Anxiety & Greenwashing Perception
#author: Thomas Kirchmair
#Eco Anxiety


library(haven)
library(dplyr)
library(plyr)
library(ggplot2)
library(misty)
library(jtools)
library(ggplot2)
#library(lsr)
library(interactions)
library(rstatix)
library(coin)
library(readxl)
library(lmtest)

data_mase <- read_excel("data_mase.xlsx")

df <- data_mase[-c(1:2)]

View(df)

#########
df$Age <- df$Age_
###############################################
#Mean Anxiety


anxiety <-  df %>%
  select(Eco_Anxiety___3_neg_1, Eco_Anxiety___3_neg_2, Eco_Anxiety___3_neg_3, Eco_Anxiety___3_neg_4, Eco_Anxiety___3_neg_5,
         Eco_Anxiety___3_neg_6, Eco_Anxiety___3_neg_7, Eco_Anxiety___3_neg_8, Eco_Anxiety___3_neg_9, Eco_Anxiety___3_neg_10, Eco_Anxiety___3_neg_11,
         Eco_Anxiety___3_neg_12, Eco_Anxiety___3_neg_13) #Eco_Anxiety___3_neg_14, Eco_Anxiety___3_neg_15, Eco_Anxiety___3_neg_16)


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



df$Anxiety <- rowMeans(df[109:121])


###############################################
#Mean Anxiety

violin_anx <- ggplot(df, aes(x= country, y = Anxiety)) +
  geom_violin() +
  geom_boxplot(width=0.1) + 
  theme_classic()
violin_anx



ggplot(df, aes(x=Anxiety)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
#######################
#Social Media Use

df$Active_Passive_SM_1 <- as.numeric(df$Active_Passive_SM_1)
df$Active_Passive_SM_2 <- as.numeric(df$Active_Passive_SM_2)
df$Active_Passive_SM_3 <- as.numeric(df$Active_Passive_SM_3)
df$Active_Passive_SM_4 <- as.numeric(df$Active_Passive_SM_4)
df$Active_Passive_SM_5 <- as.numeric(df$Active_Passive_SM_5)
df$Active_Passive_SM_6 <- as.numeric(df$Active_Passive_SM_6)


df$Social_Media_Use <- rowMeans(df[24:29])


############################
#Psychological Distance

df$Psychological_Dist_1 <- as.numeric(df$Psychological_Dist_1)
df$Psychological_Dist_2 <- as.numeric(df$Psychological_Dist_2)
df$Psychological_Dist_3 <- as.numeric(df$Psychological_Dist_3)

df$Psychological_Distance <- rowMeans(df[142:144])


#violin_dist <- ggplot(df, aes(x= country, y = Psychological_Distance)) +
#  geom_violin() +
#  geom_boxplot(width=0.1) + 
#  theme_classic()

#violin_dist

#summary(df$Psychological_Distance)

#ggplot(df, aes(x=Psychological_Distance)) + 
 # geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
  #               binwidth=.3,
   #              colour="black", fill="white") +
#  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot


#######################################
df$Country <- as.factor(df$country)

#mdl4 <- lm(Anxiety ~ Psychological_Distance + Psychological_Distance*Social_Media_Use + Country, data = df)
#summary(mdl4)

#########################################################


#mdl5 <- lm(Anxiety ~ Psychological_Distance + Psychological_Distance*Social_Media_Use + Age + Income_1 + Country, data = df)
#summary(mdl5)


##############

df$edu <- as.character(df$Education)

df[df$Country == "Germany", ] <- df[df$Country == "Germany", ] %>%
  mutate(edu = dplyr::recode(edu, "1" = "low",
                      "2" = "low",
                      "3" = "middle",
                      "4" = "middle",
                      "5" = "middle",
                      "6" = "middle",
                      "7" = "high",
                      "8" = "high"))


df[df$Country == "South Africa", ] <- df[df$Country == "South Africa", ] %>%
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



df$Education <- as.factor(df$edu)
#####################################
df$Income <- as.numeric(df$Income_1)
######################################

#mdl6 <- lm(Anxiety ~ 0+Psychological_Distance + Psychological_Distance*Social_Media_Use + Age + Income + Education + Country + Psychological_Distance*country + 
#             Psychological_Distance*Social_Media_Use*Country + Age*country + Income*Country + edu*Country, data = df)
#summary(mdl6)
#jtools::plot_coefs(mdl6, colors = "plum4")
##############################################
#Umweltbewusstsein

df$NEP_1 <- as.numeric(df$NEP_1)
df$NEP_2 <- as.numeric(df$NEP_2)
df$NEP_3 <- as.numeric(df$NEP_3)
df$NEP_4 <- as.numeric(df$NEP_4)

df$NEP_5 <- as.numeric(df$NEP_5)
df$NEP_6 <- as.numeric(df$NEP_6)
df$NEP_7 <- as.numeric(df$NEP_7)
df$NEP_8 <- as.numeric(df$NEP_8)


df$DSP <- rowMeans(df[128:131])
df$NEP <- rowMeans(df[132:135])



#df$NEP_1_rec <- item.reverse(df$NEP_1, min = 1, max = 8)
#df$NEP_2_rec <- item.reverse(df$NEP_2, min = 1, max = 8)
#df$NEP_3_rec <- item.reverse(df$NEP_3, min = 1, max = 8)
#df$NEP_4_rec <- item.reverse(df$NEP_4, min = 1, max = 8)

#df$DSP_rec <- rowMeans(df[128:131])

#df$NEP_mean <- df$NEP - df$DSP

#df$NEP_mean


#df$NEP_mean <- rowMeans(subset(df, select = c(NEP_1_rec, NEP_2_rec, NEP_3_rec, NEP_4_rec,
#                                              NEP_5, NEP_6, NEP_7, NEP_8), na.rm = TRUE))

######################
#mdl7 <- lm(Anxiety ~ 0+Psychological_Distance + Psychological_Distance*Social_Media_Use + Age + Income_1 + Education + Country + Psychological_Distance*Country + 
#             Psychological_Distance*Social_Media_Use*Country + Age*Country + Income_1*Country + edu*country + NEP + DSP, data = df)
#summary(mdl7)
#jtools::plot_coefs(mdl7, colors = "plum4")

#######################

df <- df %>%
  mutate(Gender = dplyr::recode(Gender, "1" = "female",
                         "2" = "male",
                         "3" = "other"))


df$Gender <- as.factor(df$Gender)

################################

#mdl8 <- lm(Anxiety ~ 0+Psychological_Distance + Psychological_Distance*Social_Media_Use + Age + Income_1 + edu + country + Psychological_Distance*country + 
#             Psychological_Distance*Social_Media_Use*country + Age*country + Income_1*country + edu*country + NEP*country + DSP*country + NEP + DSP +
#             Gender + Gender*country, data = df)
#summary(mdl8)
#jtools::plot_coefs(mdl8, colors = "plum4")
#
#
#mdl8$coefficients

############################################

######################################
#Recode Psychological Distance

df$Psychological_Dist_1_rec <- car::recode(df$Psychological_Dist_1, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
df$Psychological_Dist_2_rec <- car::recode(df$Psychological_Dist_2, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
df$Psychological_Dist_3_rec <- car::recode(df$Psychological_Dist_3, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

df$Psychological_Distance <- rowMeans(subset(df, select = c(Psychological_Dist_1_rec, Psychological_Dist_2_rec, Psychological_Dist_3_rec), na.rm = TRUE))


df$Psychological_Distance
##################
#fit model

mdl9 <- lm(Anxiety ~ Psychological_Distance + Social_Media_Use + Age +
             Income + Education + Country + NEP + DSP + Gender + Country*(Psychological_Distance + Social_Media_Use + Age +
                                                Income + Education + NEP + DSP + Gender + 
                                                  Psychological_Distance*Social_Media_Use), data = df)
summary(mdl9, center = TRUE)
summ(mdl9, center = TRUE)

plot_coefs_mdl9 <-
jtools::plot_coefs(mdl9, colors = "plum4", scale = TRUE) + 
ggtitle("estimates")
plot_coefs_mdl9

mdl9_x <- center_mod(mdl9)

plot_summs(mdl9_x, ci_level = 0.95, colors = "plum4") +
  ggtitle("Mean centered Regression Coefficients \n")

summ(mdl9_x, center = TRUE, digits = 5)




plot(mdl9$residuals)

qqnorm(mdl9$residuals)
qqline(mdl9$residuals) 

plot(density(mdl9$residuals))
hist(mdl9$residuals)

ggplot(data = df, aes(x=mdl9_x$residuals)) +
  geom_histogram(binwidth = 0.3,  fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals (Eco Anxiety)', x = 'Residuals', y = 'Frequency') +
  theme_light()

shapiro.test(mdl9_x$residuals)
ks.test(mdl9_x$residuals, "pnorm", mean=mean(mdl9_x$residuals), sd=sd(mdl9_x$residuals))

bptest(mdl9_x)

plot(mdl9, 1)
plot(fitted.values(mdl9), rstandard(mdl9))
plot(mdl9, which = 3)

library(sandwich)
mdl_xyz <- coeftest(mdl9, vcov = vcovHC(mdl9, type = "HC3"))

plot(mdl9)



car::durbinWatsonTest(mdl9_x)

qqnorm(mdl9_x$residuals)
qqline(mdl9_x$residuals)




###########################################################
#interactions

interact_plot(mdl9_x, pred = Income, modx = Country, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Income and country" )


catplot_gend <-
  cat_plot(mdl9_x, pred = Gender, modx = Country, geom = "line", colors = c("#69b3a2", "#404080"),
           pred.values = c("male", "female", "other"),
           line.thickness = 0.8, point.alpha = 0.7,
           geom.alpha = 0.7) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Gender and country" ) +
  ylim(2.5, 4)
catplot_gend

df_gend <- df[df$Gender != 'other',]

summary(df$Gender)
  

psych::describe.by(df_gend$Anxiety, df_gend$Gender)

oneway.test(df_gend$Anxiety ~ df_gend$Gender)

pairwise.t.test(df_gend$Anxiety, df_gend$Gender, p.adjust="bonferroni")



catplot_edu <-
cat_plot(mdl9_x, pred = Education, modx = Country, geom = "line", colors = c("#69b3a2", "#404080"),
         pred.values = c("low", "middle", "high"),
         line.thickness = 0.8, point.alpha = 0.7,
         geom.alpha = 0.7) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Education and country" ) +
  ylim(2, 4.5)



psych::describe.by(df$Anxiety, df$edu)

oneway.test(df$Anxiety ~ df$edu)
#levene_test(data = df, Anxiety ~ edu)

pairwise.t.test(df$Anxiety, df$edu, p.adjust="bonferroni")


interact_plot(mdl9_x, pred = NEP, modx = Country, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot NEP and country" )

interact_plot(mdl9_x, pred = DSP, modx = Country, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot DSP and country" )

interact_plot(mdl9_x, pred = Psychological_Distance, modx = Country, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Psychological Distance and country" )

interact_plot(mdl9_x, pred = Psychological_Distance, modx = Country, mod2 =Social_Media_Use, colors = c("#69b3a2", "#404080"),
          ) +
  ggtitle("Interaction plot Psychological Distance by Social Media Use and Country" )


interact_plot(mdl9_x, pred = Social_Media_Use , modx = Psychological_Distance, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Social Media Use and Psychological Distance" )
#############

#mdl <- lm(gw_mean ~ Anxiety + country + 0+Psychological_Distance + Social_Media_Use + Age +
#            Income_1 + edu + NEP + DSP + Gender + Anxiety*country + Anxiety*country* (Psychological_Distance + Social_Media_Use), data = df)
#summary(mdl)


#plot_summs(mdl, ci_level = 0.95, colors = "plum4") +
#  ggtitle("Mean centered Regression Coefficients \n")




#stats::cor.test(df$Income, df$Psychological_Distance, method = "spearman")
#stats::cor.test(df$Income, df$Anxiety, method = "spearman")

#stats::cor.test(df$Social_Media_Use, df$Anxiety, method = "spearman")

#scatter.smooth(x = df$Income_1, y = df$Anxiety)
#scatter.smooth(x = df$Social_Media_Use, y = df$Anxiety)



#ggplot(df, aes(x = Anxiety, y = GW, color = country)) +
 # geom_smooth() +
  #geom_point()
####################################


#levene_test(data = df, Anxiety ~ Gender)
#kruskal.test(data = df, Anxiety ~ Gender)
#rstatix::wilcox_effsize(data = df, Anxiety ~ Gender)

#levene_test(data = df, Anxiety ~ edu)
#kruskal.test(data = df, Anxiety ~ edu)
#dunn_test(data = df, Anxiety ~ edu, p.adjust.method = "bonferroni")
#rstatix::wilcox_effsize(data = df, Anxiety ~ edu)


plot(mdl9_x$fitted.values, mdl9_x$residuals)
car::durbinWatsonTest(mdl9_x)
######################
#robust standard errors

x_0 <- coeftest(mdl9_x, vcov = vcovHC(mdl9_x, type = "HC0"))
x_1 <- coeftest(mdl9_x, vcov = vcovHC(mdl9_x, type = "HC1"))
x_2 <- coeftest(mdl9_x, vcov = vcovHC(mdl9_x, type = "HC2"))
x_3 <- coeftest(mdl9_x, vcov = vcovHC(mdl9_x, type = "HC3"))

################

plot_summs(summ(mdl9_x, robust = "HC3", digits = 5), colors = "plum4") +
  ggtitle("Mean centered Regression Coefficients (Eco Anxiety) \n")


plot(mdl9_x, 3)

##############################################################################################
#correlation matrix
library(PerformanceAnalytics)


df_subset <- subset(df, select = c(Greenwashing_Perception, Anxiety, Psychological_Distance, Social_Media_Use, Age,  
                                     Income, NEP, DSP), na.rm = TRUE)


chart.Correlation(df_subset, histogram=FALSE, pch=19, method = "spearman")
mtext("Correlation Matrix (Spearman)", side=3, line=3)
####################################################################################
#Variable Inflation Fctor

mdl9_vif <- lm(Anxiety ~ Psychological_Distance + Social_Media_Use + Age +
             Income + Education + Country + NEP + DSP + Gender, data = df)

mdl9_vif <- center_mod(mdl9_vif)

vif_values_ea <- car::vif(mdl9_vif)
vif_values_ea
#barplot(vif_values_ea, main = "VIF Values", horiz = TRUE, col = "steelblue")
#abline(v = 5, lwd = 3, lty = 2)