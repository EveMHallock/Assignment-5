trout<- read.csv("data/siscowet.csv")



#example from slides
summary(trout)
head(trout)
str(trout)
xtabs(~age+locID, data=trout)
op<-par(mfrow=c(2,2), pch=19)
plot(len~age,data=trout,subset=locID=="Blind Sucker",main="Blind Sucker", ylab= "Length (mm)", xlab= "Age (years)")
plot(len~age,data=trout,subset=locID=="Grand Marais",main="Grand Marais", ylab= "Length (mm)", xlab= "Age (years)")
plot(len~age,data=trout,subset=locID=="Little Lake Harbor",main="Little Lake Harbor", ylab= "Length (mm)", xlab= "Age (years)")
par(op)


library(ggplot2)
library(tidyr)
install.packages("ggtext")
library(ggtext)


#trying ggplot 
trout %>%
ggplot(aes(x=age, y=wgt, color=locID)) +
  geom_boxplot(outlier.shape=NA) + 
  scale_y_continuous(limits = c(0, 3000)) +
  ylab("Weight (g)") +
  xlab("Age (years)") +
  labs(color="Locations", 
       title= "Siscowet Trout Weight by Age") +
  theme_dark() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5,size=14),
        axis.text = element_text(size=12)) 

trout %>%
  ggplot(aes(x=len, y=wgt, color=locID)) +
  geom_point(outlier.size=NA) +
  ylab("Weight (g)") +
  xlab("Length (mm)") +
  labs(color="Locations", 
       title= "Siscowet Trout Lengths by Weight") +
  theme_dark() +
  scale_color_brewer(palette="Paired")
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5,size=14),
        axis.text = element_text(size=12)) +
  geom_smooth(method = "lm", se = FALSE)

trout %>%
  ggplot(aes(x=age, y=len, color=locID)) +
  geom_point(outlier.size=NA) +
  ylab("Length(mm)") +
  xlab("Age (Years)") +
  labs(color="Locations", 
       title= "Siscowet Trout Lengths by Ages") +
  theme_dark() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5,size=14),
        axis.text = element_text(size=12)) +
  geom_smooth(method = "lm", se = FALSE)


#histograms


hist(trout$len, col= "gray", labels=TRUE, breaks=9, 
     xlab="Lengths (mm)", 
     main="Distribution of Siscowet Trout Lengths",
     ylim=c(0,250))

#glms
install.packages("jtools")
install.packages("broom")
install.packages("ggstance")
library(jtools)
library(broom)
library(ggstance)
install.packages("interactions")
library(interactions)

m1<-glm(wgt~locID, family=gaussian, data=trout)
summary(m1) 

m2<- glm(wgt~pnldep, data=trout, family=poisson)
summary(m2)

m3<-glm(len~age, data=trout, family= poisson)
summary(m3)


library(effects)
plot(allEffects(m3))




