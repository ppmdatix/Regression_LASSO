setwd("~/Desktop/M2 DS/reg_lin/Regression_LASSO")
rm (list=ls())
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
library(pls)
library(prospectr)
library(MASS)
library(L1pack)
library(lqa)
library(penalized)


exo1 = read.table("pharynx.dat")
exo2 = read.table('betaplasma.txt')

colnames(exo1) <- c('CASE','INST','SEX','TX','GRADE','AGE','COND','SITE','T_STAGE','N_STAGE','ENTRY-DT','STATUS','TIME')

colnames(exo2)<- c('AGE','SEXE',
                  'SMOKSTAT',
                  'QUETELET',
                  'VITUSE',
                  'CALORIES',
                  'FAT',
                  'FIBER',
                  'ALCOHOL',
                  'CHOLESTEROL',
                  'BETADIET',
                  'RETDIET',
                  'BETAPLASMA',
                  'RETPLASMA')

# Code to build factor_survey_vector
survey_vector <- c("INST", "SEX", "COND", "SITE", "T_STAGE", "N_STAGE")
factor_survey_vector <- factor(survey_vector)

# Specify the levels of factor_survey_vector
levels(factor_survey_vector) <-factor_survey_vector

exo1$INST <- as.factor(exo1$INST)
exo1$SEX <- as.factor(exo1$SEX)
exo1$COND <- as.factor(exo1$COND)
exo1$T_STAGE <- as.factor(exo1$T_STAGE)
exo1$N_STAGE <- as.factor(exo1$N_STAGE)



regression_logistique_exo1 = glm(STATUS ~ AGE, data = exo1, family = quasibinomial)
regression_logistique_exo12 = glm(STATUS ~ TX, data = exo1, family = binomial)
regression_logistique_exo13 = glm(STATUS ~ ., data = exo1, family = binomial)
regression_logistique_exo13 = glm(STATUS ~ ., data = exo1, family = binomial)
summary(regression_logistique_exo1)
summary(regression_logistique_exo12)
summary(regression_logistique_exo13)




### plot(regression_logistique_exo1)

exo2$loge = log(exo2$BETAPLASMA + 0.2)
library(plyr)
exo2 = subset(exo2,loge>3.105666 & loge< 6.7555543533)
##exo2 = exo2[exo2$loge>3.105666 & exo2$loge< 6.7555543533]


exo2$SEXE <- as.factor(exo2$SEXE)
exo2$SMOKSTAT<- as.factor(exo2$SMOKSTAT)
exo2$VITUSE <- as.factor(exo2$VITUSE)

BETAPLASMA =  log(exo2$BETAPLASMA + 0.2)
BETAPLASMA <- BETAPLASMA[BETAPLASMA >3.105666 & BETAPLASMA < 6.7555543533]

normal_ = shapiro.test(BETAPLASMA)
hist(BETAPLASMA)
hist(log(exo2$BETAPLASMA))
boxplot((log(exo2$BETAPLASMA + 0.2)))



exo2$BETAPLASMA[257]

drops <- c("BETAPLASMA")
exo2 = exo2[ , !(names(exo2) %in% drops)]









