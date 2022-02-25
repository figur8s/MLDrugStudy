
############################
#   Variable Definitions   #
############################
 
# X1 is ID. 
#            Cannot be related to participant. Used for reference only

# X2 is Age: 
#            18-24 yrs (34.11%) = -0.95197, 25-34 yrs (25.52%) = -0.07854, 
#            35-44 yrs (18.89%) = 0.49788, 45-54 yrs (15.60%) = 1.09449, 
#            55-64 yrs (4.93%) = 1.82213, 65+ yrs (0.95%) = 2.59171

# X3 is Gender: 
#            Female (49.97%) = 0.48246, Male (50.03%) = -0.48246

# X4 is Education:
# Dropped out <16 yrs old (1.49%) = -2.43591
# Dropped out at 16 yrs old (5.25%) = -1.73790
# Dropped out at 17 yrs old (1.59%) = -1.43719
# Dropped out at 18 yrs old (5.31%) = -1.22751
# Some college, no degree (26.84%) = -0.61113
# Professional certificate/ diploma (14.32%) = -0.05921
# University degree (25.46%) = 0.45468
# Masters degree (15.01%) = 1.16365
# Doctorate degree (4.72%) = 1.98437

# X5 is Country of current residence of participant and has one value
# USA (29.55%) = -0.57009
# New Zealand (0.27%) = -0.46841
# Other (6.26%) = -0.28519
# Australia (2.86%) = -0.09765
# Republic of Ireland (1.06%) = 0.21128
# Canada (4.62%) = 0.24923
# UK (55.38%) = 0.96082

# X6 is ethnicity and has one value
# Black (1.75%) = -1.10702
# Asian (1.38%) = -0.50212
# White (91.25%) = -0.31685   closest value to mean
# Mixed-White/Black (1.06%) = -0.22166
# Other (3.34%) = 0.11440
# Mixed-White/Asian (1.06%) = 0.12600
# Mixed-Black/Asian (0.16%) = 1.90725

# Ordinal: X2 Age, X4 Education
# Nominal: X3 Gender, X5 Country, X6 Ethnicity

##################################
#   Big-5 Personality variables  #
##################################

# X7 Nscore is NEO-FFI-R Neuroticism, scale:
# raw score: 12-60, re-scaled so it's from -3.5 to 3.3 with mean 0 and sd 1

# X8 Escore is NEO-FFI-R Extraversion, scale:
# raw score: 16-59, re-scaled so it's from -3.3 to 3.3 with mean 0 and sd 1

# X9 Oscore is NEO-FFI-R Openness to experience, scale:
# raw score: 24-60, re-scaled so it's from -3.3 to 2.9 with mean 0 and sd 1

# X10 Ascore is NEO-FFI-R Agreeableness, scale:
# raw score: 12-60, re-scaled so it's from -3.5 to 3.5 with mean 0 and sd 1

# X11 Cscore is NEO-FFI-R Conscientiousness, scale:
# raw score: 17-59, re-scaled so it's from -3.5 to 3.5 with mean 0 and sd 1

##################################
#   Other Personality variables  #
##################################

# X12 Impulsive is measured by BIS-11, scale:
# already scaled, ranging from -2.6 to 2.9 with mean 0.01 and sd 0.95

# X13 Sensation Seeking measured by ImpSS
# already scaled, ranging from -2.1 to 1.9 with mean 0 and sd 0.96

#########################
#   Drug Use variables  #
#########################

# X14 Alcohol consumption
# CL0 Never Used (1.80%)
# CL1 Used over a Decade Ago (1.80%)
# CL2 Used in Last Decade (3.61%)
# CL3 Used in Last Year (10.50%)
# CL4 Used in Last Month (15.23%)
# CL5 Used in Last Week (40.27%)
# CL6 Used in Last Day (26.79%)

# X15 Amphetamine consumption

# X16 Amyl Nitrite consumption

# X17 Benzodiazepine consumption

# X18 Caffeine consumption

# X19 Cannabis consumption

# X20 Chocolate consumption

# X21 Cocaine consumption

# X22 Crack consumption

# X23 Ecstasy consumption

# X24 Heroin consumption

# X25 Ketamine consumption

# X26 Legal highs consumption

# X27 LSD consumption

# X28 Methadone consumption

# X29 Mushroom consumption

# X30 Nicotine consumption

# X31 Semeron (fictitious) consumption

# X32 Volatile substance abuse consumption

#############################
##  Features of the Data   ##
#############################
library(ggplot2)
library(factoextra)
library(dplyr)
library(magrittr)
library(purrr)
library(tidyverse)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)
library(readr)

drug_consumption <- read_csv("C:/Users/Kelsey/Downloads/drug_consumption.data",
                             col_names = FALSE)


data <- as.data.frame(drug_consumption)
data <- na.omit(data)

# scale data before clustering

t <- as_tibble(data[,c(14:30,32)])

Semeron <- as_tibble(data[,31]) # explored Semeron

for(i in 1:18){
  t[,i]<- ifelse(t[,i]== "CL0", 0, ifelse(t[,i]== "CL1", 0,
          ifelse(t[,i]== "CL2", 1, ifelse(t[,i]=="CL3", 1,
          ifelse(t[,i]== "CL4", 1, ifelse(t[,i]=="CL5", 1,
          ifelse(t[,i]== "CL6", 1, 99)))))))
}

for(i in 1:length(Semeron)){
  Semeron[i]<- ifelse(Semeron[i]== "CL0", 0, ifelse(Semeron[i]== "CL1", 1,
          ifelse(Semeron[i]== "CL2", 1, ifelse(Semeron[i]=="CL3", 1,
          ifelse(Semeron[i]== "CL4", 1, ifelse(Semeron[i]=="CL5", 1,                                                                                                        ifelse(t[,i]== "CL6", 1, 99)))))))
}

SemeronIndex <- which(Semeron$value>0)

t<-as.data.frame(cbind(t, Semeron))

data[,14:32]<-t #now the location of Semeron and VSA is switched.

# label Heroin drugs: crack, cocaine, methadone, heroin

data['HeroinClass']<- data$X21 + data$X22  + data$X24 + data$X28


# label Ecstasy drugs: amphetamines, cannabis, cocaine, ketamine,
# LSD, magic mushrooms, legal highs, and ecstasy.

data['EcstasyClass']<- data$X15 + data$X19 + data$X21 + 
                       data$X23 + data$X25 +
                       data$X26 + data$X27 + data$X29

# label Benzodiazepine drugs: methadone, amphetamines, and cocaine.

data['BenzodiazepineClass']<- data$X17 + data$X21 + data$X15 + data$X28 

newdata <- cbind(data[,2:13], data$HeroinClass, 
                 data$EcstasyClass, 
                 data$BenzodiazepineClass)

head(newdata)

# de-standardize AGE


n<-1885

for(i in 1:n){
  newdata$X2[i]<- ifelse(newdata$X2[i]== (-0.95197), 0, 
                ifelse(newdata$X2[i]== (-0.07854), 1,
                ifelse(newdata$X2[i]== (0.49788), 2, 
                ifelse(newdata$X2[i]== (1.09449), 3,
                ifelse(newdata$X2[i]== (1.82213), 4, 
                ifelse(newdata$X2[i]== (2.59171), 5,
                       99))))))
}

newdata['Age']<- newdata$X2

head(newdata)

# de-standardize GENDER

for(i in 1:n){
  newdata$X3[i]<- ifelse(newdata$X3[i]== (0.48246), 0, 
                         ifelse(newdata$X3[i]== (-0.48246), 1,
                                99))
}


newdata['Gender']<- newdata$X3

# de-standardize EDUCATION

for(i in 1:n){
  newdata$X4[i]<- ifelse(newdata$X4[i]== (-2.43591), 0, 
                  ifelse(newdata$X4[i]== (-1.73790), 1,
                  ifelse(newdata$X4[i]== (-1.43719), 2,
                  ifelse(newdata$X4[i]== (-1.22751), 3,
                  ifelse(newdata$X4[i]== (-0.61113), 4,
                  ifelse(newdata$X4[i]== (-0.05921), 5,
                  ifelse(newdata$X4[i]== (0.45468), 6,
                  ifelse(newdata$X4[i]== (1.16365), 7,
                  ifelse(newdata$X4[i]== (1.98437), 8,
                                99)))))))))
}


newdata['Education']<- newdata$X4


# de-standardize COUNTRY

for(i in 1:n){
  newdata$X5[i]<- ifelse(newdata$X5[i]== (-0.09765), 0, 
                  ifelse(newdata$X5[i]== (0.24923), 1,
                  ifelse(newdata$X5[i]== (-0.46841), 2,
                  ifelse(newdata$X5[i]== (-0.28519), 3,
                  ifelse(newdata$X5[i]== (0.21128), 4,
                  ifelse(newdata$X5[i]== (0.96082), 5,
                  ifelse(newdata$X5[i]== (-0.57009), 6,
                         99)))))))
}


newdata['Country']<- newdata$X5

# de-standardize ETHNICITY

for(i in 1:n){
  newdata$X6[i]<- ifelse(newdata$X6[i]== (-0.50212), 0, 
                  ifelse(newdata$X6[i]== (-1.10702), 1,
                  ifelse(newdata$X6[i]== (1.90725), 2,
                   ifelse(newdata$X6[i]== (0.12600), 3,
                    ifelse(newdata$X6[i]== (-0.22166), 4,
                    ifelse(newdata$X6[i]== (0.11440), 5,
                   ifelse(newdata$X6[i]== (-0.31685), 6,
                    99)))))))
}


newdata['Ethnicity']<- newdata$X6


newdata['Neuroticism']<- newdata$X7
newdata['Extraversion']<- newdata$X8
newdata['Openness']<- newdata$X9
newdata['Agreeableness']<- newdata$X10
newdata['Conscientiousness']<- newdata$X11
newdata['Impulsiveness']<- newdata$X12
newdata['Sensation_Seeking']<- newdata$X13

head(newdata)

newdata2 <- newdata[,-c(1:12)]

head(newdata2)

##################
#   Goal  3     ##
##################

# label Alcohol

data['Alcohol']<- data$X14

# label Caffeine 

data['Caffeine']<- data$X18 

# label Nicotine

data['Nicotine']<- data$X30

newdata.goal3 <- cbind(newdata2[,-c(1:3)], data$Alcohol, 
                       data$Caffeine, data$Nicotine)

head(newdata.goal3)

# code below to explore semeron
#Sem.newdata2 <- cbind(newdata2, data$X32)

#Sem.newdata2<-Sem.newdata2[SemeronIndex,]

#mean(Sem.newdata2$`data$EcstasyClass`)

#mean(Sem.newdata2$`data$HeroinClass`)

#mean(Sem.newdata2$`data$BenzodiazepineClass`)

Egraph <- ggplot(newdata2, aes(x = `data$EcstasyClass`, 
                               fill = `data$EcstasyClass`)) +
  geom_bar(width = 0.5, fill = "palegreen3") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(x = "Number of Ecstasy Pleiade Drugs")+
  #coord_flip() +
  geom_vline(xintercept = mean(newdata2$`data$EcstasyClass`), 
                            linetype="dotted", 
             color = c("palegreen3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Ecstasy Pleiade Use",
       fill = c("Sample Mean", "Semeron User Mean"))+
  xlab("Number of Ecstasy Pleiade Drugs (Pale Line = Mean)")

Hgraph <- ggplot(newdata2, aes(x = `data$HeroinClass`, 
                               fill = `data$HeroinClass`)) +
  geom_bar(width = 0.5, fill = "skyblue3") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(x = "Number of Ecstasy Pleiade Drugs")+
  #coord_flip() +
  geom_vline(xintercept = mean(newdata2$`data$HeroinClass`), 
             linetype="dotted", 
             color = c("skyblue3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Heroin Pleiade Use",
       fill = c("Sample Mean", "Semeron User Mean"))+
  xlab("Number of Heroin Pleiade Drugs (Pale Line = Mean)")

Bgraph <- ggplot(newdata2, aes(x = `data$BenzodiazepineClass`, 
                               fill = `data$BenzodiazepineClass`)) +
  geom_bar(width = 0.5, fill = "mediumpurple3") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(x = "Number of Ecstasy Pleiade Drugs")+
  #coord_flip() +
  geom_vline(xintercept = mean(newdata2$`data$BenzodiazepineClass`), 
             linetype="dotted", 
             color = c("mediumpurple3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Benzodiazepine Pleiade Use",
       fill = c("Sample Mean", "Semeron User Mean"))+
  xlab("Number of Benzodiazepine Pleiade Drugs (Pale Line = Mean)")

library(ggpubr)
theme_set(theme_pubr())

figure1 <- ggarrange(Egraph, Hgraph, Bgraph,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
figure1

Agraph <- ggplot(newdata.goal3, aes(x = `data$Alcohol`, 
                               fill = `data$Alcohol`)) +
  geom_bar(width = 0.5, fill = "goldenrod2") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_vline(xintercept = mean(newdata.goal3$`data$Alcohol`), 
  #           linetype="dotted", 
  #           color = c("lightgoldenrod3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Alcohol Use",
       fill = c("Sample Mean"))+
  xlab("Alcohol User within past decade (Mean=0.964)")

Ngraph <- ggplot(newdata.goal3, aes(x = `data$Nicotine`, 
                                    fill = `data$Nicotine`)) +
  geom_bar(width = 0.5, fill = "darkorange3") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_vline(xintercept = mean(newdata.goal3$`data$Nicotine`), 
  #           linetype="dotted", 
  #           color = c("lightsalmon3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Nicotine Use",
       fill = c("Sample Mean"))+
  xlab("Nicotine User within past decade (Mean=0.67)")

Cgraph <- ggplot(newdata.goal3, aes(x = `data$Caffeine`, 
                                    fill = `data$Caffeine`)) +
  geom_bar(width = 0.5, fill = "firebrick3") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_vline(xintercept = mean(newdata.goal3$`data$Caffeine`), 
  #           linetype="dotted", 
  #           color = c("indianred3"), size=1.5)+
  theme_classic()+
  labs(title = "Representation of Caffeine Use",
       fill = c("Sample Mean"))+
  xlab("Caffeine User within past decade (Mean=0.98)")

figure2 <- ggarrange(Agraph, Cgraph, Ngraph, 
                     labels = c("A", "B", "C"),
                     ncol = 1, nrow = 3)
figure2

######################################
#    Exploring Age, Education        #
######################################

# AGE

#'18-24' age -> 0
#'25-34' age -> 1
#'35-44' age -> 2
#'45-54' age -> 3
#'55-64' age -> 4
#'65+'   age -> 5

Agelabels <- vector(length=n)

for(i in 1:n){
  Agelabels[i]<- ifelse(newdata2$Age[i]== 0, "18-24", 
                       ifelse(newdata2$Age[i]== 1, "25-34",
                       ifelse(newdata2$Age[i]== 2, "35-44",
                       ifelse(newdata2$Age[i]== 3, "45-54",
                       ifelse(newdata2$Age[i]== 4, "55-64",
                       ifelse(newdata2$Age[i]== 5, "65+",
                       99))))))
}


Agelabels <- as.data.frame(Agelabels)

Agegraph <- ggplot(Agelabels, aes(x = Agelabels, fill = Agelabels)) +
  geom_bar(width = 0.5) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(y = Countrylabels)+
  #coord_flip() +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Age",
       title = "Representation of Age Groups")

Agegraph

#####################################

# EDUCATION

# Left school before 16 years                          -> 0
# Left school at 16 years                              -> 1
# Left school at 17 years                              -> 2
# Left school at 18 years                              -> 3
# Some college or university, no certificate or degree -> 4
# Professional certificate/ diploma                    -> 5
# University degree                                    -> 6
# Masters degree                                       -> 7
# Doctorate degree                                     -> 8

Edulabels <- vector(length=n)

for(i in 1:n){
  Edulabels[i]<- ifelse(newdata2$Education[i]== 0, "<16 yrs", 
                  ifelse(newdata2$Education[i]== 1, "16 yrs",
                  ifelse(newdata2$Education[i]== 2, "17 yrs",
                 ifelse(newdata2$Education[i]== 3, "18 yrs",
                 ifelse(newdata2$Education[i]== 4, "Some college",
                  ifelse(newdata2$Education[i]== 5, "Certificate/Diploma",
                 ifelse(newdata2$Education[i]== 6, "Undergraduate Degree",
                 ifelse(newdata2$Education[i]== 7, "Master's Degree",
                 ifelse(newdata2$Education[i]== 8, "Doctorate degree",
                      99)))))))))
}


Edulabels <- as.data.frame(Edulabels)

table(Edulabels$Edulabels)

Edugraph <- ggplot(Edulabels, aes(x = Edulabels, fill = Edulabels)) +
  geom_bar(width = 0.5) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(y = Countrylabels)+
  #coord_flip() +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Education",
       title = "Representation of Education Groups")

Edugraph


############################################
#    Exploring Ethnicity, Country          #
############################################

###########

# ETHNICITY

#Asian  -> 0
#Black  -> 1
#Mixed-Black/Asian -> 2
#Mixed-White/Asian -> 3
#Mixed-White/Black -> 4
#Other             -> 5
#White             -> 6

Ethnicitylabels <- vector(length=n)

for(i in 1:n){
  Ethnicitylabels[i]<- ifelse(newdata2$Ethnicity[i]== 0, "Asian", 
                       ifelse(newdata2$Ethnicity[i]== 1, "Black",
                       ifelse(newdata2$Ethnicity[i]== 2, "Black/Asian",
                       ifelse(newdata2$Ethnicity[i]== 3, "White/Asian",
                       ifelse(newdata2$Ethnicity[i]== 4, "White/Black",
                       ifelse(newdata2$Ethnicity[i]== 5, "Other",
                       ifelse(newdata2$Ethnicity[i]== 6, "White",
                        99)))))))
  }


Ethnicitylabels <- as.data.frame(Ethnicitylabels)

Ethnicitygraph <- ggplot(Ethnicitylabels, aes(x = Ethnicitylabels, fill = Ethnicitylabels)) +
  geom_bar(width = 0.5) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(y = Countrylabels)+
  #coord_flip() +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Ethnicity",
       title = "Representation of Ethnicity")

Ethnicitygraph

#########

# COUNTRY

#Australia -> 0
#Canada    -> 1
#New Zealand->2
#Other     -> 3
#Republic of Ireland ->4
#UK         ->5
#USA        ->6

Countrylabels <-vector(length=n)
for(i in 1:n){
  Countrylabels[i]<- ifelse(newdata2$Country[i]== 0, "Australia", 
                  ifelse(newdata2$Country[i]== 1, "Canada",
                  ifelse(newdata2$Country[i]== 2, "New Zealand",
                    ifelse(newdata2$Country[i]== 3, "Other",
                   ifelse(newdata2$Country[i]== 4, "Republic of Ireland",
                    ifelse(newdata2$Country[i]== 5, "United Kingdom",
                    ifelse(newdata2$Country[i]== 6, "USA",
                   99)))))))
}


Countrylabels <- as.data.frame(Countrylabels)

Countrygraph <- ggplot(Countrylabels, aes(x = Countrylabels, fill = Countrylabels)) +
  geom_bar(width = 0.5) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  #geom_text(y = Countrylabels)+
  #coord_flip() +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Country",
       title = "Representation of Country")

Countrygraph

#################################################
#    Labeling Each Personality Attribute         #
#################################################

dens.Big5 <- as.data.frame(cbind(newdata2$Neuroticism, newdata2$Extraversion,
               newdata2$Openness, newdata2$Agreeableness,
               newdata2$Conscientiousness))

lines <- c("Neuroticism", "Extraversion", "Openness", "Agreeableness",
           "Conscientiousness")

names(dens.Big5)[1:5]<-lines

head(dens.Big5)

#plot(density(dens.Big5$Neuroticism), main = "Normal Curve for NEOAC Traits")
#lines(density(dens.Big5$Extraversion))
#lines(density(dens.Big5$Openness))
#lines(density(dens.Big5$Agreeableness))
#lines(density(dens.Big5$Conscientiousness))

#library(vioplot)
#vioplot(dens.Big5$Neuroticism, newdata2$Impulsiveness, newdata2$Sensation_Seeking,
#        names=c("NEOAC Traits", "Impulsiveness", "Sensation Seeking"),
#        col = "gold")
#title("Normalized Personality Traits in Study")
#abline(h=0)


###################################################
#    Correlation Data for Each Drug Class         #
###################################################

dataHeroin <- newdata2[,-c(2:3, 7:8)]
dataEcstasy <- newdata2[,-c(1,3, 7:8)]
dataBenzodi <- newdata2[, -c(1:2, 7:8)]

xlabels <- c("Age", "Gender", "Education",
             "Neuroticism", "Extraversion", "Openness", "Agreeableness",
             "Conscientiousness", "Impulsiveness", "Sensation_Seeking")

###############################
#        Heroin Drugs         #
###############################

corrplot(cor(dataHeroin))
corrH <- cor(dataHeroin)

HeroinCorrX <- as.data.frame(corrH[-1,1])

HeroinX <- HeroinCorrX$`corrH[-1, 1]`

corrdataH<-cbind(xlabels, HeroinX)

as.numeric(corrdataH[,2])

as.character(corrdataH[,1])

library(ggplot2)

corrdataH <- as.data.frame(corrdataH)

attach(corrdataH)

corrdataH$HeroinX <- as.numeric(corrdataH$HeroinX)

corrdataH$HeroinX

corrdataH2 <- corrdataH[order(-corrdataH$HeroinX),]

corrdataH2

#corrdataH2 <- as.data.frame(corrdataH2)

Hgraph <- ggplot(corrdataH2, aes(x = reorder(xlabels, HeroinX), y = HeroinX,
                               fill = HeroinX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  #geom_text(aes(x = "Correlation with Heroin Group", y = "Trait"))+
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation: Traits and Heroin Pleiade Use")

Hgraph

#####################
#   Ecstasy Group   #
#####################

corrplot(cor(dataEcstasy))
corrE <- cor(dataEcstasy)

EcstasyCorrX <- as.data.frame(corrE[-1,1])

EcstasyX <- EcstasyCorrX$`corrE[-1, 1]`

corrdataE<-cbind(xlabels, EcstasyX)

as.numeric(corrdataE[,2])

as.character(corrdataE[,1])

library(ggplot2)

corrdataE <- as.data.frame(corrdataE)

attach(corrdataE)

corrdataE$EcstasyX <- as.numeric(corrdataE$EcstasyX)

corrdataE$EcstasyX

corrdataE2 <- corrdataE[order(-corrdataE$EcstasyX),]

corrdataE2


Egraph <- ggplot(corrdataE2, aes(x = reorder(xlabels, EcstasyX), y = EcstasyX,
                                 fill = EcstasyX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation: Traits and Ecstasy Pleiade Use")

Egraph


#####################
#   Benzodi Group   #
#####################

corrplot(cor(dataBenzodi))
corrB <- cor(dataBenzodi)

BenzodiCorrX <- as.data.frame(corrB[-1,1])

BenzodiX <- BenzodiCorrX$`corrB[-1, 1]`

corrdataB<-cbind(xlabels, BenzodiX)

as.numeric(corrdataB[,2])

as.character(corrdataB[,1])

library(ggplot2)

corrdataB <- as.data.frame(corrdataB)

attach(corrdataB)

corrdataB$BenzodiX <- as.numeric(corrdataB$BenzodiX)

corrdataB$BenzodiX

corrdataB2 <- corrdataB[order(-corrdataB$BenzodiX),]

corrdataB2


Bgraph <- ggplot(corrdataB2, aes(x = reorder(xlabels, BenzodiX), y = BenzodiX,
                                 fill = BenzodiX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation Traits and Benzodiazepine Pleiade Use")

Bgraph

#####################
#   Goal 3         ##
#####################

dataAlcohol <- newdata.goal3[,-c(4:5, 14:15)]
dataCaffeine <- newdata.goal3[,-c(4:5, 13,15)]
dataNicotine <- newdata.goal3[, -c(4:5, 13:14)]

#####################
#     Alcohol      ##
#####################

corrplot(cor(dataAlcohol))
corrA <- cor(dataAlcohol)

AlcoholCorrX <- as.data.frame(corrA[-1,1])

AlcoholX <- AlcoholCorrX$`corrA[-1, 1]`

corrdataA<-cbind(xlabels, AlcoholX)

as.numeric(corrdataA[,2])

as.character(corrdataA[,1])

library(ggplot2)

corrdataA <- as.data.frame(corrdataA)

attach(corrdataA)

corrdataA$AlcoholX <- as.numeric(corrdataA$AlcoholX)

corrdataA$AlcoholX

corrdataA2 <- corrdataA[order(-corrdataA$AlcoholX),]

corrdataA2


Agraph <- ggplot(corrdataA2, aes(x = reorder(xlabels, AlcoholX), y = AlcoholX,
                                 fill = AlcoholX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation Traits and Alcohol Use")

Agraph

#####################
#     Cafffeine    ##
#####################

corrplot(cor(dataCaffeine))
corrC <- cor(dataCaffeine)

CaffeineCorrX <- as.data.frame(corrC[-1,1])

CaffeineX <- CaffeineCorrX$`corrC[-1, 1]`

corrdataC<-cbind(xlabels, CaffeineX)

as.numeric(corrdataC[,2])

as.character(corrdataC[,1])

library(ggplot2)

corrdataC <- as.data.frame(corrdataC)

attach(corrdataC)

corrdataC$CaffeineX <- as.numeric(corrdataC$CaffeineX)

corrdataC$CaffeineX

corrdataC2 <- corrdataC[order(-corrdataC$CaffeineX),]

corrdataC2


Cgraph <- ggplot(corrdataC2, aes(x = reorder(xlabels, CaffeineX), y = CaffeineX,
                                 fill = CaffeineX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation Traits and Caffeine Use")

Cgraph

#####################
#     Nicotine     ##
#####################

corrplot(cor(dataNicotine))
corrN <- cor(dataNicotine)

NicotineCorrX <- as.data.frame(corrN[-1,1])

NicotineX <- NicotineCorrX$`corrN[-1, 1]`

corrdataN<-cbind(xlabels, NicotineX)

as.numeric(corrdataN[,2])

as.character(corrdataN[,1])

library(ggplot2)

corrdataN <- as.data.frame(corrdataN)

attach(corrdataN)

corrdataN$NicotineX <- as.numeric(corrdataN$NicotineX)

corrdataN$NicotineX

corrdataN2 <- corrdataN[order(-corrdataN$NicotineX),]

corrdataN2


Ngraph <- ggplot(corrdataN2, aes(x = reorder(xlabels, NicotineX), y = NicotineX,
                                 fill = NicotineX)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() +
  theme_classic()+
  labs(y = "Correlation", x="Traits", fill = "Corr",
       title = "Correlation Traits and Nicotine Use")

Ngraph



################################
##       Character Aspects     #
################################

newdata3 <- as.data.frame(cbind(newdata$X7,newdata$X8, newdata$X9,
                                newdata$X10,newdata$X11,
                                newdata$X12, newdata$X13,
                                Agelabels,
                                newdata2$Gender, 
                                Edulabels))

AGE_SS <- ggplot(data=newdata3, aes(x=newdata$Sensation_Seeking, 
                                     group=Agelabels, 
                                     fill=Agelabels)) +
  geom_density(adjust=1.5, position="fill") +
  theme_classic()+
  labs(x="Normalized Sensation Seeking", y="Density",
       fill = "Age Groups",
       title = "Densities for Sensation Seeking by Age") 

AGE_SS

cor(newdata$Sensation_Seeking, newdata2$Age)
  
mean(newdata$Sensation_Seeking[newdata2$Age==0])

mean(newdata$Sensation_Seeking[newdata2$Age==1])

mean(newdata$Sensation_Seeking[newdata2$Age==2])

mean(newdata$Sensation_Seeking[newdata2$Age==3])

mean(newdata$Sensation_Seeking[newdata2$Age==4])

mean(newdata$Sensation_Seeking[newdata2$Age==5])

# Mean Sensation Seeking has a steady negative decline as the age increases


AGE_Imp <-ggplot(data=newdata3, aes(x=newdata$Impulsiveness, 
                                      group=Agelabels, 
                                      fill=Agelabels)) +
  geom_density(adjust=1.5, position="fill") +
  theme_classic()+
  labs(x="Normalized Impulsiveness", y="Density",
       fill = "Age Groups",
       title = "Densities for Impulsiveness by Age") 

AGE_Imp

cor(newdata$Impulsiveness, newdata2$Age)

mean(newdata$Impulsiveness[newdata2$Age==0])

mean(newdata$Impulsiveness[newdata2$Age==1])

mean(newdata$Impulsiveness[newdata2$Age==2])

mean(newdata$Impulsiveness[newdata2$Age==3])

mean(newdata$Impulsiveness[newdata2$Age==4])

mean(newdata$Impulsiveness[newdata2$Age==5])

# Mean Impulsiveness has a steady decline as age increases, with a slight jump
# up for age 55-64


# this shows a negative relationship between increasing age and
# the measure of Sensation Seeking

cor(newdata2$Age, newdata2$Neuroticism)          #X7
#cor(newdata2$Age, newdata2$Extraversion)        #X8
cor(newdata2$Age, newdata2$Openness)             #X9
#cor(newdata2$Age, newdata2$Agreeableness)       #X10
cor(newdata2$Age, newdata2$Conscientiousness)    #X11

AGE_N <- ggplot(data=newdata3, aes(x=newdata$X7, 
                                    group=Agelabels, 
                                    fill=Agelabels)) +
  geom_density(adjust=1.5, position="fill") +
  theme_classic()+
  labs(x="Normalized Neuroticism", y="Density",
       fill = "Age Groups",
       title = "Densities for Neuroticism by Age") 

AGE_N


AGE_O <- ggplot(data=newdata3, aes(x=newdata$X9, 
                                   group=Agelabels, 
                                   fill=Agelabels)) +
  geom_density(adjust=1.5, position="fill") +
  theme_classic()+
  labs(x="Normalized Openness", y="Density",
       fill = "Age Groups",
       title = "Densities for Openness by Age") 

AGE_O


AGE_C <- ggplot(data=newdata3, aes(x=newdata$X11, 
                                   group=Agelabels, 
                                   fill=Agelabels)) +
  geom_density(adjust=1.5, position="fill") +
  theme_classic()+
  labs(x="Normalized Conscientiousness", y="Density",
       fill = "Age Groups",
       title = "Densities for Conscientiousness by Age") 

AGE_C

################################
#     AGE and Psych Traits     #
################################
cor(newdata2$Age, newdata2$Sensation_Seeking)    
cor(newdata2$Age, newdata2$Neuroticism)          
cor(newdata2$Age, newdata2$Extraversion)        
cor(newdata2$Age, newdata2$Openness)             
cor(newdata2$Age, newdata2$Agreeableness)       
cor(newdata2$Age, newdata2$Conscientiousness)    
cor(newdata2$Age, newdata2$Impulsiveness)

cor(newdata2$Age, newdata2$`data$HeroinClass`)
cor(newdata2$Age, newdata2$`data$EcstasyClass`)
cor(newdata2$Age, newdata2$`data$BenzodiazepineClass`)



################################
#     EDU and Psych Traits     #
################################
cor(newdata2$Education, newdata2$Sensation_Seeking)    
cor(newdata2$Education, newdata2$Neuroticism)          
cor(newdata2$Education, newdata2$Extraversion)        
cor(newdata2$Education, newdata2$Openness)             
cor(newdata2$Education, newdata2$Agreeableness)       
cor(newdata2$Education, newdata2$Conscientiousness)    
cor(newdata2$Education, newdata2$Impulsiveness)

cor(newdata2$Education, newdata2$`data$HeroinClass`)
cor(newdata2$Education, newdata2$`data$EcstasyClass`)
cor(newdata2$Education, newdata2$`data$BenzodiazepineClass`)
cor(newdata.goal3$Education, newdata.goal3$`data$Alcohol`)
cor(newdata.goal3$Education, newdata.goal3$`data$Caffeine`)
cor(newdata.goal3$Education, newdata.goal3$`data$Nicotine`)

###################################
#     Gender and Psych Traits     #
###################################

cor(newdata2$Gender, newdata2$Sensation_Seeking)    #x13
cor(newdata2$Gender, newdata2$Neuroticism)          #X7
cor(newdata2$Gender, newdata2$Extraversion)        #X8
cor(newdata2$Gender, newdata2$Openness)             #X9
cor(newdata2$Gender, newdata2$Agreeableness)       #X10
cor(newdata2$Gender, newdata2$Conscientiousness)    #X11
cor(newdata2$Gender, newdata2$Impulsiveness)

cor(newdata2$Gender, newdata2$`data$HeroinClass`)
cor(newdata2$Gender, newdata2$`data$EcstasyClass`)
cor(newdata2$Gender, newdata2$`data$BenzodiazepineClass`)

# being male is more strongly correlated with SS, Openness, ALL Drug classes,
# being male is most strongly correlated with use of Ecstasy Drugs, 
# 2nd to Sensation Seeking

# being female is more strongly corr with Agreeableness, Conscientiousness

############################
# Outline of PCA Steps   ###
############################

# clustering PCA: dimensionality reduction is done based on the amount of variance explained by a few variables
# certain variables might be able to be clustered together

# import packages
library(tidyverse)   # pipe operator
library(magrittr)    # set rownames, colnames
library(DT)          # print data tables
library(factoextra)  # pca biplot


# create dataset with just numeric measures of personality

numeric.personality.data <- newdata2[,c(9:15)]


# use built in function to see if analysis using pca loadings would be helpful

pca_result <- prcomp(numeric.personality.data, scale = TRUE)   # it can standardize data for us like the above
datatable(round(pca_result$x, 4), options = list(pageLength = 6)) 

fviz_pca_biplot(
  pca_result, 
  axes = c(1, 2),              # choose two PC's to plot
  geom = c("point"),           # plot points, no text here
  labelsize = 0.5,               # label font size 
  pointsize = 0.1,             # point size 
  col.ind = "grey80",          # point and label color 
  col.var = "contrib",         # use variable contributions to PC as legend 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")    # color for variable contrib
) +
  scale_y_continuous()

summary(pca_result) 

res.pca <- prcomp(numeric.personality.data, scale = TRUE)

fviz_eig(res.pca, main = "Scree Plot for the Numerical Psych Vars")

fviz_pca_var(res.pca, 
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#options(ggrepel.max.overlaps = Inf)


###

library("Gifi")

ordinalvec<- c(TRUE, FALSE, TRUE, FALSE, FALSE)

CatPCA <- princals(newdata2[,c(4:8)], ordinal = ordinalvec)

summary(CatPCA)

plot(CatPCA, "loadplot", main = "Loadings Plot for Categorical Vars")

# START K MEANS HERE

# unsupervised clustering
# for k-means we are iterating between the e and m steps of an EM algorithm
# with categorical variables, we can work with them and it's easier with ordinal
# there are functions in r that can help with that
# nominal categories, see what's going on inside each and then compare (?)
# k means and hierarchical clustering are "distance-based"

# two types of hierarchical clustering approaches
# agglomerative (bottom-up): start with one point per group, work up to one large cluster
# repeatedly merge the two groups that have the samllest distance (dissimilarity) divergence
# the most popular method

# divisive (top-down): start with one big cluster and work to having one per group
# divisive is NOT very popular

# dendragram ... ?

# Centroid linkage.. ?

# ideal number of clusters determined by different methods

# After you identify the clusters, describe what's happening
# Potentially use supervised statistics and make inference...

# some problems of k-means:
# 1 K means algorithm can depend on initial values
# 2 definition of dist. & divergence can be distorted if the spaces upon which
# the clusters are defined are non-homogeneous


newdata4 <- cbind(newdata.goal3[,c(13:15)], newdata2[,-c(4:8)])

head(newdata4)

#distance <- get_dist(newdata4)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

##### how would a clustering of size 2 look like?
k2 <- kmeans(newdata4, centers = 2, nstart = 100)
str(k2)
k2
fviz_cluster(k2, data = newdata4)

###### Now, let's trying clustering 
###### portfolios for k=2,3,4, and 5!

k3 <- kmeans(newdata4, centers = 3, nstart = 50)
k4 <- kmeans(newdata4, centers = 4, nstart = 50)
k5 <- kmeans(newdata4, centers = 5, nstart = 50)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

summary(k3)

clustered.data <- cbind(newdata4, cluster = k3$cluster)

k3$centers

cluster1<-clustered.data[clustered.data$cluster==1,]
cluster2<-clustered.data[clustered.data$cluster==2,]
cluster3<-clustered.data[clustered.data$cluster==3,]


par(mfrow=c(4,2))
hist(cluster1$Neuroticism)
abline(v=mean(cluster1$Neuroticism), col="red")
hist(cluster1$Extraversion)
abline(v=mean(cluster1$Extraversion), col="red")
hist(cluster1$Openness)
abline(v=mean(cluster1$Openness), col="red")
hist(cluster1$Agreeableness)
abline(v=mean(cluster1$Agreeableness), col="red")
hist(cluster1$Conscientiousness)
abline(v=mean(cluster1$Conscientiousness), col="red")
hist(cluster1$Sensation_Seeking)
abline(v=mean(cluster1$Sensation_Seeking), col="red")
hist(cluster1$Impulsiveness)
abline(v=mean(cluster1$Impulsiveness), col="red")

par(mfrow=c(4,2))
hist(cluster2$Neuroticism)
abline(v=mean(cluster2$Neuroticism), col="red")
hist(cluster2$Extraversion)
abline(v=mean(cluster2$Extraversion), col="red")
hist(cluster2$Openness)
abline(v=mean(cluster2$Openness), col="red")
hist(cluster2$Agreeableness)
abline(v=mean(cluster2$Agreeableness), col="red")
hist(cluster2$Conscientiousness)
abline(v=mean(cluster2$Conscientiousness), col="red")
hist(cluster2$Sensation_Seeking)
abline(v=mean(cluster2$Sensation_Seeking), col="red")
hist(cluster2$Impulsiveness)
abline(v=mean(cluster2$Impulsiveness), col="red")

par(mfrow=c(4,2))
hist(cluster3$Neuroticism)
abline(v=mean(cluster3$Neuroticism), col="red")
hist(cluster3$Extraversion)
abline(v=mean(cluster3$Extraversion), col="red")
hist(cluster3$Openness)
abline(v=mean(cluster3$Openness), col="red")
hist(cluster3$Agreeableness)
abline(v=mean(cluster3$Agreeableness), col="red")
hist(cluster3$Conscientiousness)
abline(v=mean(cluster3$Conscientiousness), col="red")
hist(cluster3$Sensation_Seeking)
abline(v=mean(cluster3$Sensation_Seeking), col="red")
hist(cluster3$Impulsiveness)
abline(v=mean(cluster3$Impulsiveness), col="red")


par(mfrow=c(3,2))
hist(cluster1$`data$HeroinClass`)
abline(v=mean(cluster1$`data$HeroinClass`), col="blue")
hist(cluster1$`data$EcstasyClass`)
abline(v=mean(cluster1$`data$EcstasyClass`), col="blue")
hist(cluster1$`data$BenzodiazepineClass`)
abline(v=mean(cluster1$`data$BenzodiazepineClass`), col="blue")
hist(cluster1$`data$Alcohol`)
abline(v=mean(cluster1$`data$Alcohol`), col="blue")
hist(cluster1$`data$Caffeine`)
abline(v=mean(cluster1$`data$Caffeine`), col="blue")
hist(cluster1$`data$Nicotine`)
abline(v=mean(cluster1$`data$Nicotine`), col="blue")

par(mfrow=c(3,2))
hist(cluster2$`data$HeroinClass`)
abline(v=mean(cluster2$`data$HeroinClass`), col="blue")
hist(cluster2$`data$EcstasyClass`)
abline(v=mean(cluster2$`data$EcstasyClass`), col="blue")
hist(cluster2$`data$BenzodiazepineClass`)
abline(v=mean(cluster2$`data$BenzodiazepineClass`), col="blue")
hist(cluster2$`data$Alcohol`)
abline(v=mean(cluster2$`data$Alcohol`), col="blue")
hist(cluster2$`data$Caffeine`)
abline(v=mean(cluster2$`data$Caffeine`), col="blue")
hist(cluster2$`data$Nicotine`)
abline(v=mean(cluster2$`data$Nicotine`), col="blue")


par(mfrow=c(3,2))
hist(cluster3$`data$HeroinClass`)
abline(v=mean(cluster3$`data$HeroinClass`), col="blue")
hist(cluster3$`data$EcstasyClass`)
abline(v=mean(cluster3$`data$EcstasyClass`), col="blue")
hist(cluster3$`data$BenzodiazepineClass`)
abline(v=mean(cluster3$`data$BenzodiazepineClass`), col="blue")
hist(cluster3$`data$Alcohol`)
abline(v=mean(cluster3$`data$Alcohol`), col="blue")
hist(cluster3$`data$Caffeine`)
abline(v=mean(cluster3$`data$Caffeine`), col="blue")
hist(cluster3$`data$Nicotine`)
abline(v=mean(cluster3$`data$Nicotine`), col="blue")


####################################
#    TEST  AND  TRAIN  SAMPLING    #
####################################


PerHeroinAny <- count(newdata2[newdata2$`data$HeroinClass`>0,])/n
PerHeroinNo <- count(newdata2[newdata2$`data$HeroinClass`==0,])/n

PerEcstasyAny <- count(newdata2[newdata2$`data$EcstasyClass`>0,])/n
PerEcstasyNo <- count(newdata2[newdata2$`data$EcstasyClass`==0,])/n

PerBenzodiAny <- count(newdata2[newdata2$`data$BenzodiazepineClass`>0,])/n
PerBenzodiNo <- count(newdata2[newdata2$`data$BenzodiazepineClass`==0,])/n

PerAlcoholAny <- count(newdata.goal3[newdata.goal3$`data$Alcohol`>0,])/n
PerAlcoholNo <- count(newdata.goal3[newdata.goal3$`data$Alcohol`==0,])/n

PerCaffeineAny <- count(newdata.goal3[newdata.goal3$`data$Caffeine`>0,])/n
PerCaffeineNo <- count(newdata.goal3[newdata.goal3$`data$Caffeine`==0,])/n

PerNicotineAny <- count(newdata.goal3[newdata.goal3$`data$Nicotine`>0,])/n
PerNicotineNo <- count(newdata.goal3[newdata.goal3$`data$Nicotine`==0,])/n

# % of subject who did ANY Heroin Class Drug:
PerHeroinAny$n
# % of subject who did No Heroin Class Drug:
PerHeroinNo$n
# % of subject who did ANY Ecstasy Class Drug:
PerEcstasyAny$n
# % of subject who did No Ecstasy Class Drug:
PerEcstasyNo$n
# % of subject who did ANY Benzodiazepine Class Drug:
PerBenzodiAny$n
# % of subject who did No Benzodiazepine Class Drug:
PerBenzodiNo$n
# % of subject who did No Alcohol in Past Decade:
PerAlcoholNo$n
# % of subject who did ANY Alcohol in Past Decade:
PerAlcoholAny$n
# % of subject who did No Caffeine in Past Decade:
PerCaffeineNo$n
# % of subject who did ANY Caffeine in Past Decade:
PerCaffeineAny$n
# % of subject who did No Nicotine in Past Decade:
PerNicotineNo$n
# % of subject who did ANY Nicotine in Past Decade:
PerNicotineAny$n



n.train <- n*.8
n.test <- n*.2

n.train
n.test

set.seed(999) 

index<- c(1:1885)

index.train <- sample(index, n.train, replace = TRUE)
  
data.train <- newdata2[index.train,]

data.train.goal3 <- newdata.goal3[index.train,]
  
count(data.train[data.train$`data$HeroinClass`>0,])/n
  # 35 / 65 split (User v Non-User). Should be 44/56.
  
count(data.train[data.train$`data$EcstasyClass` >0,])/n
  # 56 / 44 split (User v Non-User). Should be 70/30.
  
count(data.train[data.train$`data$BenzodiazepineClass` >0,])/n
  # 45 / 55 split (User v Non-User). Should be 56 / 44.
  
count(data.train.goal3[data.train.goal3$`data$Alcohol` >0,])/n
# 77 / 23 split (User v Non-User). Should be 96 / 4.

count(data.train.goal3[data.train.goal3$`data$Caffeine` >0,])/n
# 79 / 21 split (User v Non-User). Should be 98 / 2.

count(data.train.goal3[data.train.goal3$`data$Nicotine` >0,])/n
# 54 / 46 split (User v Non-User). Should be 67 / 33.

# we consistently under-sample the active user groups for all drug groups

# define test sample for goal 1-2

index.test0 <- index[-index.train]

index.test <-sample(index.test0, n.test, replace = TRUE)

data.test <- newdata2[index.test,]

#removes Country and Ethnicity:
data.train <- data.train[,-c(7:8)]
data.test <- data.test[,-c(7:8)]

dim(data.train)
dim(data.test)

# define test sample for goal 3
data.test.goal3 <- newdata.goal3[index.test,]
#removes Country and Ethnicity:
data.train.g3 <- data.train.goal3[,-c(4:5)]
data.test.g3 <- data.test.goal3[,-c(4:5)]

dim(data.train.g3)
dim(data.test.g3)

####################################
#    One-Hot Encode: Goal 1-2      #
####################################

data.train0 <- data.train[,-c(4:6)]

data.train02 <- newdata3[index.train,c(8:10)]

data.train.2.0 <- as.data.frame(cbind(data.train0, data.train02))

head(data.train.2.0)

# adequately one-hot encode Education, age, gender

library(caret)

dummy.train <- dummyVars(" ~ .", data=data.train.2.0)

final_train <- data.frame(predict(dummy.train, newdata=data.train.2.0))

#final_train <- final_train[,-11] # had to remove repeat of Sensation_seeking

head(final_train)

# Now do the same One-Hot Encoding for test

data.test0 <- data.test[,-c(4:6)]

data.test02 <- newdata3[index.test,c(8:10)]

data.test.2.0 <- as.data.frame(cbind(data.test0, data.test02))

head(data.test.2.0)

library(caret)

dummy.test <- dummyVars(" ~ .", data=data.test.2.0)

final_test <- data.frame(predict(dummy.test, newdata=data.test.2.0))

head(final_test)

y_train <- final_train[,c(1:3)]
x_train <- final_train[,-c(1:3)]

y_test <- final_test[,c(1:3)]
x_test <- final_test[,-c(1:3)]


 #classify y var as user / non-user 1/0

for(i in 1:n.train){
  for(j in 1:3){
    if(y_train[i,j] > 0){
      y_train[i,j]<-1
    }else{
      y_train[i,j]<-0
    }
  }
}

head(y_train)

for(i in 1:n.test){
  for(j in 1:3){
    if(y_test[i,j] > 0){
      y_test[i,j]<-1
    }else{
      y_test[i,j]<-0
    }
  }
}

head(y_test)

####################################
#    One-Hot Encode: Goal 3      #
####################################

data.train0.g3 <- data.train.g3[,-c(1:3)] # remove cat vars

#data.train02 <- newdata3[index.train,c(8:10)]

data.train.2.0.g3 <- as.data.frame(cbind(data.train0.g3, data.train02))

head(data.train.2.0.g3)

# adequately one-hot encode Education, age, gender

library(caret)

dummy.train.g3 <- dummyVars(" ~ .", data=data.train.2.0.g3)

final_train.g3 <- data.frame(predict(dummy.train.g3, newdata=data.train.2.0.g3))

#final_train <- final_train[,-11] # had to remove repeat of Sensation_seeking

head(final_train.g3)

# Now do the same One-Hot Encoding for test

data.test0.g3 <- data.test.g3[,-c(1:3)] # remove cat vars

#data.test02 <- newdata3[index.test,c(8:10)]

data.test.2.0.g3 <- as.data.frame(cbind(data.test0.g3, data.test02))

head(data.test.2.0.g3)

library(caret)

dummy.test.g3 <- dummyVars(" ~ .", data=data.test.2.0.g3)

final_test.g3 <- data.frame(predict(dummy.test.g3, newdata=data.test.2.0.g3))

head(final_test.g3)

y_train.g3 <- final_train.g3[,c(8:10)]
x_train.g3 <- final_train.g3[,-c(8:10)]

y_test.g3 <- final_test.g3[,c(8:10)]
x_test.g3 <- final_test.g3[,-c(8:10)]

head(x_train.g3)

head(y_test.g3)


#########################
# DENDRODIAGRAM

library(ggdendro)
library(dendextend)
library(ggplot2)
library(dplyr)

x_full = rbind(x_test, x_train)
y_full_H = c(y_test$X.data.HeroinClass., 
             y_train$X.data.HeroinClass.)

y_full = cbind(rbind(y_test, y_train), rbind(y_test.g3, y_train.g3))

data_full = cbind(y_full, x_full)

#data_full_H = cbind(y_full_H, x_full)

#hclust.Heroin.NU <- hclust(dist(newdata4[y_full_H==0,]),
#                           "ave")

#hclust.Heroin.User <- hclust(dist(data_full_H[y_full_H==1,]),
#                             "ave")
#par(mfrow=c(1,1))
#plot(hclust.Heroin.NU)

#plot(hclust.Heroin.User)

#cut.treeH.NU <- cutree(hclust.Heroin.NU, k = 4)
#cut.treeH.User <- cutree(hclust.Heroin.User, k=4)

#HNU_dend_obj <- as.dendrogram(hclust.Heroin.NU)
#HUser_dend_obj <- as.dendrogram(hclust.Heroin.User)

#HNU_col_dend <- color_branches(HNU_dend_obj, h = 4)
#HUser_col_dend <- color_branches(HUser_dend_obj, h = 4)

#plot(HNU_col_dend, main="Heroin Non-User Hierarchical Clustering")
#plot(HUser_col_dend, main="Heroin User Hierarchical Clustering")


#HNUdata_df_cl <- mutate(data_full_H[y_full_H==0,], 
#                        cluster = cut.treeH.NU)
#count(HNUdata_df_cl,cluster)

#HUserdata_df_cl <- mutate(data_full_H[y_full_H==1,], 
 #                         cluster = cut.treeH.User)
#count(HUserdata_df_cl,cluster)



#ggplot(HNUdata_df_cl, aes(x=Sensation_Seeking, y = Neuroticism, 
#                          color = factor(cluster)))+
#  geom_point()


#ggplot(HUserdata_df_cl, aes(x=Sensation_Seeking, y = Neuroticism, 
#                            color = factor(cluster)))+
#  geom_point()



data_full_scaled <- apply(data_full, 2, scale)

colnames(data_full_scaled) = colnames(data_full)


#hc.crime.scaled <- hclust(dist(US.scaled), "ave")


# Determine number of clusters
wss <- (nrow(data_full_scaled)-1)*sum(apply(data_full_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_full_scaled,
                                     centers=i)$withinss)
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fitk <- kmeans(data_full_scaled, 6) # 6 cluster solution
# get cluster means
aggregate(data_full_scaled,by=list(fitk$cluster),FUN=mean)
# append cluster assignment
data_full_scaledc <- data.frame(data_full_scaled, fitk$cluster)


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(data_full_scaled, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)





# Next we fit a lasso logistic regression model using glmnet() on the training data
# Evaluate the performance on training data
# Plot the accuracy "acclmv" as a function of the shrinkage parameter, lambda

# See Figure 10.11 for similar procedure

# This code should run quickly bc of sparse-matrix format
# accuracy() function written in 10.9.2 Lab is applied to every col of predic-
# tion matrix classlmv. Bc the matrix is a logical matrix of TRUE/FALSE, we
# supply the second argument truth as a logical vector

library(glmnet)

accuracy <- function(pred, truth){
  return(mean(drop(pred)==drop(truth)))
}

###########
# Heroin  #
###########

fitlm.Heroin <- glmnet(x_train, y_train$X.data.HeroinClass.,
                family = "binomial")

predict.H.lmtest <- predict(fitlm.Heroin, newx=as.matrix(x_test), type = "class", s = c(0.05, 0.01))

head(predict.H.lmtest)
accH.lmH <- accuracy(predict.H.lmtest, y_test$X.data.HeroinClass.)

accH.lmH


###########
# Ecstasy #
###########

fitlm.Ecstasy <- glmnet(x_train, y_train$X.data.EcstasyClass.,
                        family = "binomial")

predict.E.lmtest <- predict(fitlm.Ecstasy, newx=as.matrix(x_test), 
                            type = "class", s = c(0.05, 0.01))

head(predict.E.lmtest)
accH.lmE <- accuracy(predict.E.lmtest, y_test$X.data.EcstasyClass.)

accH.lmE

##################
# Benzodiazepine #
##################

fitlm.Benzodi <- glmnet(x_train, y_train$X.data.BenzodiazepineClass.,
                        family = "binomial")

predict.B.lmtest <- predict(fitlm.Benzodi, newx=as.matrix(x_test), 
                            type = "class", s = c(0.05, 0.01))

head(predict.B.lmtest)
accH.lmB <- accuracy(predict.B.lmtest, y_test$X.data.BenzodiazepineClass.)

accH.lmB


###########
# Alcohol #
###########

fitlm.Alcohol <- glmnet(x_train.g3, y_train.g3$X.data.Alcohol.,
                        family = "binomial")

predict.A.lmtest <- predict(fitlm.Alcohol, newx=as.matrix(x_test.g3), 
                            type = "class", s = c(0.05, 0.01))

head(predict.A.lmtest)
accH.lmA <- accuracy(predict.A.lmtest, y_test.g3$X.data.Alcohol.)

accH.lmA

############
# Caffeine #
############

fitlm.Caffeine <- glmnet(x_train.g3, y_train.g3$X.data.Caffeine.,
                        family = "binomial")

predict.C.lmtest <- predict(fitlm.Caffeine, newx=as.matrix(x_test.g3), 
                            type = "class", s = c(0.05, 0.01))

head(predict.C.lmtest)
accH.lmC <- accuracy(predict.C.lmtest, y_test.g3$X.data.Caffeine.)

accH.lmC


############
# Nicotine #
############

fitlm.Nicotine <- glmnet(x_train.g3, y_train.g3$X.data.Nicotine.,
                         family = "binomial")

predict.N.lmtest <- predict(fitlm.Nicotine, newx=as.matrix(x_test.g3), 
                            type = "class", s = c(0.05, 0.01))

head(predict.N.lmtest)
accH.lmN <- accuracy(predict.N.lmtest, y_test.g3$X.data.Nicotine.)

accH.lmN

############################################
# Look at the lambdas compared to accuracy #
############################################

classlmvH <- predict.glmnet(fitlm.Heroin, newx=as.matrix(x_test)) > 0
acclmvH <- apply(classlmvH , 2, accuracy, y_test$X.data.HeroinClass. > 0)


classlmvE<- predict.glmnet(fitlm.Ecstasy, newx=as.matrix(x_test)) > 0
acclmvE <- apply(classlmvE , 2, accuracy, y_test$X.data.EcstasyClass. > 0)


classlmvB <- predict.glmnet(fitlm.Benzodi, newx=as.matrix(x_test)) > 0
acclmvB <- apply(classlmvB , 2, accuracy, y_test$X.data.BenzodiazepineClass. > 0)

plot(x=-log(fitlm.Heroin$lambda), y=acclmvH, xlab="-log(lambda)",
     ylim = c(0.4, 1.0), xlim= c(1,10),
     ylab="Accuracy", main="Ridge Regression for User Classification",
     col="blue", type="l")

lines(x=-log(fitlm.Ecstasy$lambda), y=acclmvE,
     col="green", type="l")

lines(x=-log(fitlm.Benzodi$lambda), y=acclmvB,
     col="purple", type="l")
legend("bottomright", legend = c("Heroin", "Ecstasy", "Benzodiazepine"), 
       fill = c("blue", "green", "purple"), cex=0.6)

best.lambda.H<-fitlm.Heroin$lambda[match(max(acclmvH), acclmvH)]

best.lambda.B<-fitlm.Benzodi$lambda[match(max(acclmvB), acclmvB)]

best.lambda.E<-fitlm.Ecstasy$lambda[match(max(acclmvE), acclmvE)]

# Goal 3: Now let's do it for Alcohol, Caffeine, and Nicotine

classlmvA <- predict.glmnet(fitlm.Alcohol, newx=as.matrix(x_test.g3)) > 0
acclmvA <- apply(classlmvA , 2, accuracy, y_test.g3$X.data.Alcohol > 0)


classlmvC<- predict.glmnet(fitlm.Caffeine, newx=as.matrix(x_test.g3)) > 0
acclmvC <- apply(classlmvC , 2, accuracy, y_test.g3$X.data.Caffeine > 0)


classlmvN <- predict.glmnet(fitlm.Nicotine, newx=as.matrix(x_test.g3)) > 0
acclmvN <- apply(classlmvN , 2, accuracy, y_test.g3$X.data.Nicotine > 0)

plot(x=-log(fitlm.Alcohol$lambda), y=acclmvA, xlab="-log(lambda)",
     ylim = c(0.4, 1.0), xlim= c(1,10),
     ylab="Accuracy", main="Ridge Regression for User Classification",
     col="gold", type="l")

lines(x=-log(fitlm.Caffeine$lambda), y=acclmvC,
      col="darkorange", type="l")

lines(x=-log(fitlm.Nicotine$lambda), y=acclmvN,
      col="red", type="l")
legend("bottomright", legend = c("Alcohol", "Caffeine", "Nicotine"), 
       fill = c("yellow", "orange", "red"), cex=0.6)

best.lambda.A<-fitlm.Alcohol$lambda[match(max(acclmvA), acclmvA)]

best.lambda.C<-fitlm.Caffeine$lambda[match(max(acclmvC), acclmvC)]

best.lambda.N<-fitlm.Nicotine$lambda[match(max(acclmvN), acclmvN)]

###########
# Heroin  #
###########

predict.H.bestlamb <- predict(fitlm.Heroin, newx=as.matrix(x_test), 
                            type = "class", s = best.lambda.H)

head(predict.H.bestlamb)
accH.lmH.bestlamb <- accuracy(predict.H.bestlamb, y_test$X.data.HeroinClass.)

accH.lmH.bestlamb

table(predict.H.bestlamb, y_test$X.data.HeroinClass.)

###########
# Ecstasy #
###########

predict.E.bestlamb <- predict(fitlm.Ecstasy, newx=as.matrix(x_test), 
                            type = "class", s = best.lambda.E)

head(predict.E.bestlamb)
accH.lmE.bestlamb <- accuracy(predict.E.bestlamb, y_test$X.data.EcstasyClass.)

accH.lmE.bestlamb

table(predict.E.bestlamb, y_test$X.data.EcstasyClass.)

##################
# Benzodiazepine #
##################

predict.B.bestlamb<- predict(fitlm.Benzodi, newx=as.matrix(x_test), 
                            type = "class", s = best.lambda.B)

head(predict.B.bestlamb)
accH.lmB.bestlamb <- accuracy(predict.B.bestlamb, y_test$X.data.BenzodiazepineClass.)

accH.lmB.bestlamb

table(predict.B.bestlamb, y_test$X.data.BenzodiazepineClass.)


############
# Alcohol  #
############

predict.A.bestlamb <- predict(fitlm.Alcohol, newx=as.matrix(x_test.g3), 
                              type = "class", s = best.lambda.A)

head(predict.A.bestlamb)
accH.lmA.bestlamb <- accuracy(predict.A.bestlamb, y_test.g3$X.data.Alcohol.)

accH.lmA.bestlamb

table(predict.A.bestlamb, y_test.g3$X.data.Alcohol.)

############
# Caffeine  #
############

predict.C.bestlamb <- predict(fitlm.Caffeine, newx=as.matrix(x_test.g3), 
                              type = "class", s = best.lambda.C)

head(predict.C.bestlamb)
accH.lmC.bestlamb <- accuracy(predict.C.bestlamb, y_test.g3$X.data.Caffeine.)

accH.lmC.bestlamb

table(predict.C.bestlamb, y_test.g3$X.data.Caffeine.)

#############
# Nicotine  #
#############

predict.N.bestlamb <- predict(fitlm.Nicotine, newx=as.matrix(x_test.g3), 
                              type = "class", s = best.lambda.N)

head(predict.N.bestlamb)
accH.lmN.bestlamb <- accuracy(predict.N.bestlamb, y_test.g3$X.data.Nicotine.)

accH.lmN.bestlamb

table(predict.N.bestlamb, y_test.g3$X.data.Nicotine.)


#################################
# Neural Network Data-wrangling #
#################################

x.lrm.H <- scale(model.matrix(y_train$X.data.HeroinClass. ~. -1, data = x_train))
y.lrm.H <- y_train$X.data.HeroinClass.

x.lrm.H.test <- scale(model.matrix(y_test$X.data.HeroinClass. ~. -1, data = x_test))
y.lrm.H.test <- y_test$X.data.HeroinClass.

x.lrm.E <- scale(model.matrix(y_train$X.data.EcstasyClass. ~. -1, data = x_train))
y.lrm.E <- y_train$X.data.EcstasyClass.

x.lrm.E.test <- scale(model.matrix(y_test$X.data.EcstasyClass. ~. -1, data = x_test))
y.lrm.E.test <- y_test$X.data.EcstasyClass.

x.lrm.B <- scale(model.matrix(y_train$X.data.BenzodiazepineClass. ~. -1,
                              data = x_train))
y.lrm.B <- y_train$X.data.BenzodiazepineClass.

x.lrm.B.test <- scale(model.matrix(y_test$X.data.BenzodiazepineClass. ~. -1,
                                   data = x_test))
y.lrm.B.test <- y_test$X.data.BenzodiazepineClass.

#########################################
# GOAL 3: Neural Network Data-wrangling #
#########################################

x.lrm.A <- scale(model.matrix(y_train.g3$X.data.Alcohol. ~. -1, data = x_train.g3))
y.lrm.A <- y_train.g3$X.data.Alcohol.

x.lrm.A.test <- scale(model.matrix(y_test.g3$X.data.Alcohol. ~. -1, data = x_test.g3))
y.lrm.A.test <- y_test.g3$X.data.Alcohol.

x.lrm.C <- scale(model.matrix(y_train.g3$X.data.Caffeine. ~. -1, data = x_train.g3))
y.lrm.C <- y_train.g3$X.data.Caffeine.

x.lrm.C.test <- scale(model.matrix(y_test.g3$X.data.Caffeine. ~. -1, data = x_test.g3))
y.lrm.C.test <- y_test.g3$X.data.Caffeine.

x.lrm.N <- scale(model.matrix(y_train.g3$X.data.Nicotine. ~. -1,
                              data = x_train.g3))
y.lrm.N <- y_train.g3$X.data.Nicotine.

x.lrm.N.test <- scale(model.matrix(y_test.g3$X.data.Nicotine. ~. -1,
                                   data = x_test.g3))
y.lrm.N.test <- y_test.g3$X.data.Nicotine.


####################
#  3 Neural Nets   #
####################

library(keras)
#library(tensorflow)
#use_condaenv("tf", required=TRUE)
reticulate::conda_list()$name
use_condaenv("r-reticulate", required = TRUE)
reticulate::conda_install(packages="pillow")

####################
#  Neural Net  1   #
####################


###########
# Heroin  #
###########

modnn1.H <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.H)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

# use soft max for final activation function for 1,2,3,4 classification

modnn1.H %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                  metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.H <- modnn1.H %>% fit(
  x.lrm.H, y.lrm.H, epochs = 500, batch_size = 30,
  validation_data = list(x.lrm.H.test, y.lrm.H.test)
)

# If you re-run fit() command it will pick up where it left off

plot(x = c(1:500), y= history1.H$metrics$val_accuracy, type = "l", col = "blue",
     main = "Simple Neural Net 1 via ReLU Activation (Heroin Pleiades)",
     xlab = "epoch", ylab = "Accuracy")

accH.nn1<-history1.H$metrics$val_accuracy[500]

abline(h=accH.nn1, col="black")

###########
# Ecstasy #
###########

modnn1.E <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.E)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn1.E %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.E <- modnn1.E %>% fit(
  x.lrm.E, y.lrm.E, epochs = 500, batch_size = 30,
  validation_data = list(x.lrm.E.test, y.lrm.E.test)
)

# If you re-run fit() command it will pick up where it left off

plot(x = c(1:500), y= history1.E$metrics$val_accuracy, type = "l", col = "green",
     main = "Simple Neural Net 1 via ReLU Activation (Ecstasy Pleiades)",
     xlab = "epoch", ylab = "Accuracy")

accE.nn1<-history1.E$metrics$val_accuracy[500]

abline(h=accE.nn1, col="black")

##################
# Benzodiazepine #
##################

modnn1.B <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.B)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn1.B %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.B <- modnn1.B %>% fit(
  x.lrm.B, y.lrm.B, epochs = 500, batch_size = 30,
  validation_data = list(x.lrm.B.test, y.lrm.B.test)
)

# If you re-run fit() command it will pick up where it left off

plot(x = c(1:500), y= history1.B$metrics$val_accuracy, type = "l", col = "purple",
     main = "Simple Neural Net 1 via ReLU Activation (Benzodiazepine Pleiades)",
     xlab = "epoch", ylab = "Accuracy")

accB.nn1<-history1.B$metrics$val_accuracy[500]

abline(h=accB.nn1, col="black")

##########################
# Plot of all 3 Pleiades #
##########################

plot(x = c(1:500), y= history1.E$metrics$val_accuracy, type = "l", col = "green",
     main = "Simple Neural Net 1 via ReLU Activation (All Pleiades)",
     xlab = "epoch", ylab = "Accuracy")
abline(h=accE.nn1, col="black")

lines(x = c(1:500), y= history1.H$metrics$val_accuracy, type = "l", col = "blue")
abline(h=accH.nn1, col="black")

lines(x = c(1:500), y= history1.B$metrics$val_accuracy, type = "l", col = "purple")
abline(h=accB.nn1, col="black")


###########
# Alcohol #
###########

modnn1.A <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.A)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

# use soft max for final activation function for 1,2,3,4 classification

modnn1.A %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.A <- modnn1.A %>% fit(
  x.lrm.A, y.lrm.A, epochs = 500, batch_size = 20,
  validation_data = list(x.lrm.A.test, y.lrm.A.test)
)

accA.nn1<-history1.A$metrics$val_accuracy[500]

###########
# Caffeine #
###########

modnn1.C <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.C)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn1.C %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.C <- modnn1.C %>% fit(
  x.lrm.C, y.lrm.C, epochs = 500, batch_size = 20,
  validation_data = list(x.lrm.C.test, y.lrm.C.test)
)

accC.nn1<-history1.C$metrics$val_accuracy[500]

###########
# Nicotine #
###########

modnn1.N <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x.lrm.N)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

# use soft max for final activation function for 1,2,3,4 classification

modnn1.N %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

##################################
# TIME TO FIT THE MODEL

history1.N <- modnn1.N %>% fit(
  x.lrm.N, y.lrm.N, epochs = 500, batch_size = 20,
  validation_data = list(x.lrm.N.test, y.lrm.N.test)
)

accN.nn1<-history1.N$metrics$val_accuracy[500]

##########################
# Plot of all 6 Pleiades #
##########################

plot(x = c(1:500), y= history1.A$metrics$val_accuracy, type = "l", col = "gold",
     ylim = c(0.4,1),
     main = "Simple Neural Net 1 via ReLU Activation",
     xlab = "epoch", ylab = "Accuracy")
abline(h=accA.nn1, col="black")

lines(x = c(1:500), y= history1.C$metrics$val_accuracy, type = "l", col = "orange")
abline(h=accC.nn1, col="black")

lines(x = c(1:500), y= history1.N$metrics$val_accuracy, type = "l", col = "red")
abline(h=accN.nn1, col="black")

lines(x = c(1:500), y= history1.E$metrics$val_accuracy, type = "l", col = "green")
abline(h=accE.nn1, col="black")

lines(x = c(1:500), y= history1.H$metrics$val_accuracy, type = "l", col = "blue")
abline(h=accH.nn1, col="black")

lines(x = c(1:500), y= history1.B$metrics$val_accuracy, type = "l", col = "purple")
abline(h=accB.nn1, col="black")

legend("bottomright", legend = c("Heroin", "Ecstasy", "Benzodiazepine",
                                 "Alcohol", "Caffeine", "Nicotine"), 
       fill = c("blue", "green", "purple",
                "yellow", "orange", "red"), cex=0.6)

####################
#  Neural Net  2   #
####################

###########
# Heroin  #
###########

modnn2.H <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.H)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate
     
modnn2.H %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.H <- modnn2.H %>% fit(x.lrm.H, y_train$X.data.HeroinClass.,
      epochs = 500, batch_size = 30,
      validation_data = list(x.lrm.H.test, y_test$X.data.HeroinClass.))

# WARNING
# If you re-run fit() command it will pick up where it left off

#plot(x = c(1:500), y= history2.H$metrics$val_accuracy, type = "l", col = "blue",
#     main = "Neural Net 2 via ReLU Activation with 2 Dropout Layers (Heroin Pleiades)",
#     xlab = "epoch", ylab = "Accuracy")

accH.nn2<-history2.H$metrics$val_accuracy[500]

#abline(h=accH.nn2, col="black")

###########
# Ecstasy #
###########


modnn2.E <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.E)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate

modnn2.E %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.E <- modnn2.E %>% fit(x.lrm.E, y_train$X.data.EcstasyClass.,
                               epochs = 500, batch_size = 30,
                               validation_data = list(x.lrm.E.test, 
                                                      y_test$X.data.EcstasyClass.))

# WARNING
# If you re-run fit() command it will pick up where it left off

#plot(x = c(1:500), y= history2.E$metrics$val_accuracy, type = "l", col = "green",
#     main = "Neural Net 2 via ReLU Activation with 2 Dropout Layers (Ecstasy Pleiades)",
#     xlab = "epoch", ylab = "Accuracy")

accE.nn2<-history2.E$metrics$val_accuracy[500]

#abline(h=accE.nn2, col="black")

##################
# Benzodiazepine #
##################


modnn2.B <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.B)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate

modnn2.B %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.B <- modnn2.B %>% fit(x.lrm.B, y_train$X.data.BenzodiazepineClass.,
                               epochs = 500, batch_size = 30,
                               validation_data = list(x.lrm.B.test, 
                                                      y_test$X.data.BenzodiazepineClass.))

# WARNING
# If you re-run fit() command it will pick up where it left off

#plot(x = c(1:500), y= history2.B$metrics$val_accuracy, type = "l", col = "purple",
#     main = "Neural Net 2 via ReLU Activation with 2 Dropout Layers (Benzodiazepine Pleiades)",
#     xlab = "epoch", ylab = "Accuracy")

accB.nn2<-history2.B$metrics$val_accuracy[500]

#abline(h=accB.nn2, col="black")

##########################
# Plot of all 3 Pleiades #
##########################

#plot(x = c(1:500), y= history2.E$metrics$val_accuracy, type = "l", col = "green",
#     main = "NN 2 w/2 Dropout Layers via ReLU Act. (All Pleiades)",
#     xlab = "epoch", ylab = "Accuracy")
#abline(h=accE.nn2, col="black")

#lines(x = c(1:500), y= history2.H$metrics$val_accuracy, type = "l", col = "blue")
#abline(h=accH.nn2, col="black")

#lines(x = c(1:500), y= history2.B$metrics$val_accuracy, type = "l", col = "purple")
#abline(h=accB.nn2, col="black")

################
# use soft max for final activation function for 1,2,3,4 classification

###########
# Alcohol  #
###########

modnn2.A <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.A)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate

modnn2.A %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.A <- modnn2.A %>% fit(x.lrm.A, y_train$X.data.HeroinClass.,
                               epochs = 500, batch_size = 30,
                               validation_data = list(x.lrm.A.test, y_test$X.data.Alcohol.))

# WARNING
# If you re-run fit() command it will pick up where it left off

accA.nn2<-mean(history2.A$metrics$accuracy)


###########
# Caffeine  #
###########

modnn2.C <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.C)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate

modnn2.C %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.C <- modnn2.A %>% fit(x.lrm.C, y_train$X.data.HeroinClass.,
                               epochs = 500, batch_size = 30,
                               validation_data = list(x.lrm.C.test, 
                                                      y_test$X.data.Caffeine.))

# WARNING
# If you re-run fit() command it will pick up where it left off

accC.nn2<-mean(history2.C$metrics$accuracy)

###########
# Nicotine  #
###########

modnn2.N <- keras_model_sequential () %>%
  layer_dense(units = 10, activation = "relu",      # units = 20 causes overfit
              input_shape = ncol(x.lrm.N)) %>%
  layer_dropout(rate = 0.2) %>%                  #additional dropout .4 less acc
  layer_dense(units = 5, activation = "relu") %>%    # unit = 3 less acc
  #layer_dense(units = 5, activation = "relu") %>%   # more layers, less accurate
  #layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%                     # adding dropout improved it
  layer_dense(units = 1, activation = "relu")     # sigmoid less accurate

modnn2.N %>% compile(loss = "mse", optimizer = optimizer_rmsprop (),
                     metrics = c("accuracy"))

history2.N <- modnn2.N %>% fit(x.lrm.N, y_train$X.data.HeroinClass.,
                               epochs = 500, batch_size = 30,
                               validation_data = list(x.lrm.N.test, 
                                                      y_test$X.data.Nicotine.))

 # WARNING
# If you re-run fit() command it will pick up where it left off

accN.nn2<-mean(history2.N$metrics$accuracy)

## PLOT ALL 6

plot(x = c(1:500), y= history2.E$metrics$val_accuracy, type = "l", col = "green",
     ylim = c(0.4,1),
     main = "NN 2 w/2 Dropout Layers via ReLU Act. (All Pleiades)",
     xlab = "epoch", ylab = "Accuracy")
abline(h=accE.nn2, col="black")

lines(x = c(1:500), y= history2.H$metrics$val_accuracy, type = "l", col = "blue")
abline(h=accH.nn2, col="black")

lines(x = c(1:500), y= history2.B$metrics$val_accuracy, type = "l", col = "purple")
abline(h=accB.nn2, col="black")

lines(x = c(1:500), y= history2.A$metrics$accuracy, type = "l", col = "yellow")
abline(h=accA.nn2, col="black")

lines(x = c(1:500), y= history2.C$metrics$accuracy, type = "l", col = "orange")
abline(h=accC.nn2, col="black")

lines(x = c(1:500), y= history2.N$metrics$accuracy, type = "l", col = "red")
abline(h=accN.nn2, col="black")

legend("bottomright", legend = c("Heroin", "Ecstasy", "Benzodiazepine",
                                 "Alcohol", "Caffeine", "Nicotine"), 
       fill = c("blue", "green", "purple",
                "yellow", "orange", "red"), cex=0.6)
