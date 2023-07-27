#Master thesis: Eco Anxiety & Greenwashing Perception
#author: Thomas Kirchmair
#Greenwashing Perception


library(haven)
library(dplyr)
library(ggplot2)
library(misty)
library(jtools)
library(ggplot2)
library(interactions)

library(readxl)

data_mase <- read_excel("data_mase.xlsx")

df <- data_mase[-c(1:2)]



########################

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
############

#vague

df$GL2_1 <- as.numeric(df$GL2_1)
df$G6_1 <- as.numeric(df$G6_1)
df$G10_1 <- as.numeric(df$G10_1)
df$G14_1 <- as.numeric(df$G14_1)
df$G18_1 <- as.numeric(df$G18_1)
df$G22_1 <- as.numeric(df$G22_1)



df$gw_vag <- rowMeans(subset(df, select = c(GL2_1, G6_1, G10_1, G14_1, G18_1, G22_1), na.rm = TRUE))

#false

df$GL3_1 <- as.numeric(df$GL3_1)
df$G7_1 <- as.numeric(df$G7_1)
df$G11_1 <- as.numeric(df$G11_1)
df$G15_1 <- as.numeric(df$G15_1)
df$G19_1 <- as.numeric(df$G19_1)
df$G23_1 <- as.numeric(df$G23_1)



df$gw_false <- rowMeans(subset(df, select = c(GL3_1, G7_1, G11_1, G15_1, G19_1, G23_1), na.rm = TRUE))

#omission

df$GL4_1 <- as.numeric(df$GL4_1)
df$G8_1 <- as.numeric(df$G8_1)
df$G12_1 <- as.numeric(df$G12_1)
df$G16_1 <- as.numeric(df$G16_1)
df$G20_1 <- as.numeric(df$G20_1)
df$G24_1 <- as.numeric(df$G24_1)



df$gw_om <- rowMeans(subset(df, select = c(GL4_1, G8_1, G12_1, G16_1, G20_1, G24_1), na.rm = TRUE))



#Greenwashing Perception
df$Greenwashing_Perception <- rowMeans(subset(df, select =c(GL3_1, G7_1, G11_1, G15_1, G19_1, G23_1, GL2_1, G6_1,
                                                            G10_1, G14_1, G18_1, G22_1, GL4_1, G8_1, G12_1, G16_1, G20_1, G24_1)))
########
#country
df$Country <- as.factor(df$country)
####################
#Psychological Distance

df$Psychological_Dist_1 <- as.numeric(df$Psychological_Dist_1)
df$Psychological_Dist_2 <- as.numeric(df$Psychological_Dist_2)
df$Psychological_Dist_3 <- as.numeric(df$Psychological_Dist_3)

df$Dist_mean <- rowMeans(df[142:144])

#Social Media Use
df$Active_Passive_SM_1 <- as.numeric(df$Active_Passive_SM_1)
df$Active_Passive_SM_2 <- as.numeric(df$Active_Passive_SM_2)
df$Active_Passive_SM_3 <- as.numeric(df$Active_Passive_SM_3)
df$Active_Passive_SM_4 <- as.numeric(df$Active_Passive_SM_4)
df$Active_Passive_SM_5 <- as.numeric(df$Active_Passive_SM_5)
df$Active_Passive_SM_6 <- as.numeric(df$Active_Passive_SM_6)


df$Social_Media_Use <- rowMeans(df[24:29])


#Education
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

#Income
df$Income <- as.numeric(df$Income_1)

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



df$NEP_1_rec <- item.reverse(df$NEP_1, min = 1, max = 8)
df$NEP_2_rec <- item.reverse(df$NEP_2, min = 1, max = 8)
df$NEP_3_rec <- item.reverse(df$NEP_3, min = 1, max = 8)
df$NEP_4_rec <- item.reverse(df$NEP_4, min = 1, max = 8)



df$DSP_rec <- rowMeans(df[128:131])



df$NEP_mean <- df$NEP - df$DSP

df$NEP_mean

#Gender
df <- df %>%
  mutate(Gender = dplyr::recode(Gender, "1" = "female",
                                "2" = "male",
                                "3" = "other"))

df$Gender <- as.factor(df$Gender)

#############
#Age
df$Age <- df$Age_
########################

#mdl_hyp <- lm(Greenwashing_Perception ~ Anxiety + Country + Anxiety*Country, data = df)
#summ(mdl_hyp, center = TRUE)

#plot_summs(mdl_hyp, ci_level = 0.95, colors = "plum4") +
#  ggtitle("Mean centered Regression Coefficients for GW perception \n")

#interact_plot(mdl, pred = Anxiety, modx = Country)


############################################
#model original (psychological distance not recoded)

mdl <- lm(Greenwashing_Perception ~ Anxiety + Country + Dist_mean + Social_Media_Use + Age +
            Income + Education + NEP + DSP + Gender + Anxiety*Country + Anxiety*Country*(Dist_mean + Social_Media_Use), data = df)
summary(mdl)


plot_summs(mdl, colors = "plum4", center=TRUE) +
  ggtitle("Mean centered Regression Coefficients \n")

summ(mdl, center = TRUE, digits = 3)

#Interactions
interact_plot(mdl, pred = Anxiety, modx = Country, colors = c("#69b3a2", "#404080"), centered = "all") +
  ggtitle("Interaction plot Anxiety and Country" )

interact_plot(mdl, pred = Anxiety, modx = Country, mod2 = Dist_mean, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Anxiety by Country and Social Distance" )

interact_plot(mdl, pred = Anxiety, modx = Country, mod2 = Social_Media_Use, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Anxiety by Country and Social Media Use" )

#df_germ <- df[df$Country == "Germany", ]
#cor.test(x = df_germ$Greenwashing_Perception, y = df_germ$Anxiety, method = "spearman")

#df_sa<- df[df$Country == "South Africa", ]
#cor.test(x = df_sa$Greenwashing_Perception, y = df_sa$Anxiety)

  
#######################################




#scatter_eco_gw <-
#  ggplot(data = df, aes(y = Greenwashing_Perception, x= Anxiety, color = Country)) +
#  geom_point() +
#  geom_smooth(method = lm)
#scatter_eco_gw

#ggplot(df, aes(x = Anxiety, y = Greenwashing_Perception, color = Country)) +
#  geom_smooth() +
#  geom_point()




####################################
#recode pychological distance

#summary(df$Psychological_Dist_1)
#summary(df$Psychological_Dist_2)
#summary(df$Psychological_Dist_3)

#df$Psychological_Dist_1_rec <- misty::item.reverse(df$Psychological_Dist_1, min = 1, max = 7)
#df$Psychological_Dist_2_rec <- item.reverse(df$Psychological_Dist_2, min = 1, max = 7)
#df$Psychological_Dist_3_rec <- item.reverse(df$Psychological_Dist_3, min = 1, max = 7)


df$Psychological_Dist_1_rec <- car::recode(df$Psychological_Dist_1, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
df$Psychological_Dist_2_rec <- car::recode(df$Psychological_Dist_2, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
df$Psychological_Dist_3_rec <- car::recode(df$Psychological_Dist_3, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")


df$Psychological_Distance <- rowMeans(subset(df, select = c(Psychological_Dist_1_rec, Psychological_Dist_2_rec, Psychological_Dist_3_rec), na.rm = TRUE))

#summary(df$Psychological_Distance)
#summary(df$Dist_mean)

#model recoded (psychological distance recoded)
mdl_rev <- lm(Greenwashing_Perception ~ Anxiety + Country + Psychological_Distance + Social_Media_Use + Age +
            Income + Education + NEP + DSP + Gender + Anxiety*Country + Anxiety*Country*(Psychological_Distance + Social_Media_Use), data = df)


plot_summs(mdl_rev, colors = "plum4", center=TRUE) +
  ggtitle("Mean centered Regression Coefficients (Greenwashing Perception) \n")

summ(mdl_rev, center = TRUE, digits = 3)


########################################
interact_rev <-
interact_plot(mdl_rev, pred = Anxiety, modx = Country, mod2 = Psychological_Distance, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Anxiety by Country and Psychological Distance" )
interact_rev

interact_orig <-
interact_plot(mdl, pred = Anxiety, modx = Country, mod2 = Dist_mean, colors = c("#69b3a2", "#404080")) +
  ggtitle("Interaction plot Anxiety by Country and Psychological Distance" )
interact_orig

###########
shapiro.test(mdl_rev$residuals)
bptest(mdl_rev_cent)
#############


plot(mdl_rev, 1)
plot(fitted.values(mdl_rev), rstandard(mdl_rev))


mdl_rev_cent <- center_mod(mdl_rev)
summ(mdl_rev_cent, digits = 5)
############
#calculate robust standard errors

library(sandwich)
coeftest(mdl_rev_cent, vcov = vcovHC(mdl_rev_cent, type = "HC0"))
coeftest(mdl_rev_cent, vcov = vcovHC(mdl_rev_cent, type = "HC1"))
#coeftest(mdl_rev, vcov = vcovHC(mdl_rev, type = "HC2"))

#library(car)

#mdl_rev_cent

#fit_b <- car::Boot(mdl_rev_cent, R = 5000)


#summary(fit_b)
#confint(fit_b, level = .95)

##############
#check requirements
ggplot(data = df, aes(x=mdl_rev_cent$residuals)) +
  geom_histogram(binwidth = 0.3,  fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals (Greenwashing Perception)', x = 'Residuals', y = 'Frequency') +
  theme_light()

ks.test(mdl_rev_cent$residuals, "pnorm", mean=mean(mdl_rev_cent$residuals), sd=sd(mdl_rev_cent$residuals))

plot(mdl_rev_cent, 1)
plot(fitted.values(mdl_rev_cent), rstandard(mdl_rev_cent))



qqnorm(mdl_rev_cent$residuals)
qqline(mdl_rev_cent$residuals) 

plot(mdl_rev_cent, 3)


car::durbinWatsonTest(mdl_rev_cent)
#############################
#plot robust standard errors

plot_summs(summ(mdl_rev_cent, robust = "HC1", digits = 5), colors = "plum4") +
  ggtitle("Mean centered Regression Coefficients (Greenwashing Perception) \n")

#############################
#calculate Variable Inflation Factor

mdl_gw_vif <- lm(data = df, Greenwashing_Perception ~ Anxiety + Country + Psychological_Distance + Social_Media_Use + Age +
                   Income + Education + NEP + DSP + Gender)

mdl_gw_vif <- center_mod(mdl9_vif)

vif_values_ea <- car::vif(mdl9_vif)
vif_values_ea

