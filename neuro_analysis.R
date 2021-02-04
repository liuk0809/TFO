# options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
# install.packages("rJava")
# install.packages("xlsx")
# library("xlsx")
library(dplyr)
library(ggplot2)

### longitudinal data with 
# dat <- read.xlsx("Masterdokument_1.xlsx",sheetIndex = 1)
dat <- read.csv("Masterdokument_1.csv",skip=1,header = TRUE) %>%
  select(IDpat,ifostatus,proteingroup,ga,Bayley,X.1,Date,Weight,ENTERAL..OVERALL...g.kg.,X.74,X.75) %>%
  rename(fat=ENTERAL..OVERALL...g.kg.,lactose=X.74,protein=X.75,full_ifo_days=X.1,ID=IDpat,IFO_STATUS=ifostatus) %>%
  slice(-c(1:2)) %>%
  arrange(ID) 
length(unique(dat$ID)) # there are 103 infants in this data set

y <- read.csv("ND Check_Infants for Analysis 2018-08-19.csv") %>%
  select(ID,IFO_STATUS,Incl,ProteinGroup,Scores.Received.,COG,LANG,MOT) %>%
  arrange(ID) 
length(unique(y$ID)) # there are 230 infants in this data set
# y1 <- filter(y,Incl=="Y")
# score <- filter(y, Scores.Received. == "YES")
ND <- filter(y, Incl=="Y" & Scores.Received. == "YES")

# analysis population with ND data
ptID <- intersect(unique(dat$ID),ND$ID) # there are 69 infants for ND analysis 

dat1 <- filter(dat, ID %in% ptID)
str(dat1)
dat11 <- type.convert(dat1,numerals="no.loss",as.is=TRUE)
str(dat11)

# the type of PROTEIN, fat, lactose is not converted yet.
levels(dat1$protein)
levels(dat1$fat)
levels(dat1$lactose)
dat11$protein <- as.numeric(dat11$protein)
dat11$fat <- as.numeric(dat11$fat)
dat11$lactose <- as.numeric(dat11$lactose)
str(dat11)
summary(dat11$protein)
# check the protein cell with non-numerical/"" in dat1 has been converted to NA in dat11 #
k <- which(dat1$protein %in% levels(dat1$protein)[c(1:3,356)]) # extract rows in dat1 with non-numerical/"" entries
dat1[k,]
dat11[k,] # checked 

# protein with value 0 should be excluded 
dat11$protein[which(dat11$protein==0.00)] <- NA
summary(dat11$protein)
summary(dat11$fat)
dat11$fat[which(dat11$fat==0)] <- NA
summary(dat11$lactose)
dat11$lactose[which(dat11$lactose==0)] <- NA
dat11$IFO_STATUS <- as.factor(dat11$IFO_STATUS)
str(dat11)
summary(dat11$IFO_STATUS)

### end of longitudinal data ###

### start of ND data ###

ND1 <- filter(ND, ID %in% ptID)

# base <- read.csv("peapodbodycomposition 280_plusminus_ 28 dyas.csv") %>%
#   select(IDstudy,IFO_STATUS,Incl,ProteinGroup,GA_at_Birth,bw) %>%
#   rename(ID=IDstudy) %>%
#   arrange(ID)
# base$ID # there are 73 infants in this data set
# base1 <- distinct(base)
# base2 <- filter(base1, ID %in% ptID)
# setdiff(ptID, base1$ID)

base <- read.csv("baseline characteristics.csv") %>%
  select(IDpat,DOB,ga,bwt) %>%
  rename(ID=IDpat) %>%
  arrange(ID)# 103 infants in this data set
base1 <- filter(base,ID %in% ptID)

dat2 <- full_join(ND1,base1,by="ID") 

dat2$IFO_STATUS <- as.factor(dat2$IFO_STATUS)

## END: dat11 is the longitudinal weight and protein; dat2 contains ND score,BW,GA ## 


########################################################
## plot the longitudinal curve for weight and protein ## 
########################################################
ggplot(data=dat11) + 
 #geom_line(aes(x=full_ifo_days,y=Weight,group=ID,color=as.factor(proteingroup)))
  geom_line(aes(x=full_ifo_days,y=Weight,group=ID,color=IFO_STATUS))

ggplot(data=dat11)+
  #geom_line(aes(x=full_ifo_days,y=protein,group=ID,color=as.factor(proteingroup)))
  geom_line(aes(x=full_ifo_days,y=protein,group=ID,color=IFO_STATUS))

ggplot(data=dat11)+
  #geom_line(aes(x=full_ifo_days,y=protein,group=ID,color=as.factor(proteingroup)))
  geom_line(aes(x=full_ifo_days,y=fat,group=ID,color=IFO_STATUS))

ggplot(data=dat11)+
  #geom_line(aes(x=full_ifo_days,y=protein,group=ID,color=as.factor(proteingroup)))
  geom_line(aes(x=full_ifo_days,y=lactose,group=ID,color=as.factor(IFO_STATUS)))

ggplot(data=dat2)+
  geom_point(aes(x=bwt,y=COG))
ggplot(data=dat2)+
  geom_point(aes(x=ga,y=COG))


dat_protein = dat11 %>% group_by(ID) %>%
  summarise( ave_protein = mean(protein,na.rm=TRUE))

dat3 <- left_join(dat2,dat_protein,by="ID")
### dat3 is the cross-sectional data for analysis ##



ggplot(data=dat3,aes(x=ave_protein,y=COG,col=IFO_STATUS))+geom_point()+
  facet_grid(~IFO_STATUS)


m <- lm(COG~IFO_STATUS*ga,data=dat3)
summary(m)
m <- lm(COG~IFO_STATUS*ga+bwt+IFO_STATUS*ave_protein,data=dat3)
 summary(m)
m <- lm(COG~ProteinGroup*ave_protein,data=dat3)
m <- lm(COG~IFO_STATUS,data=dat3)
summary(m)
plot(m)



m1 <- lm(COG~ga,data=dat3)
# m1 <- lm(COG ~ ave_protein,data=dat3)
 summary(m1)
 anova(m1,m)
 
 ldat <- data.frame(IFO_STATUS=as.factor(c(0,1)),intercepts =c(cf[1],cf[1]+cf[2]),slopes=c(cf[3],cf[3]+cf[4]))
 cf <- coef(m)
 plot(ga,COG)
 ggplot(data=dat3,aes(ga,y=COG,col=IFO_STATUS))+
   geom_point()+
   geom_abline(data=ldat,aes(intercept=intercepts,slope=slopes,col=IFO_STATUS))+
   facet_grid(~IFO_STATUS)
 abline(cf[1],cf[3],col="green") # SF group
 abline(cf[1]+cf[2],cf[3]+cf[4],col="red") # TPO group
 

# LANG 
 m <- lm(LANG~IFO_STATUS*ga,data=dat3)
 summary(m)
 m1 <- lm(LANG~ga,data=dat3)
 summary(m1)
 anova(m1,m)

 # MOT
 m <- lm(MOT~IFO_STATUS*ga,data=dat3)
 summary(m)
 m1 <- lm(MOT~ga,data=dat3)
 summary(m1)
 anova(m1,m)
 