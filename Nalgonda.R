mydata<-read.csv(file = "~/final_csv_nalgonda (2).csv")
View(mydata)
str(mydata)
dim(mydata)
#plot(mydata)
#install.packages("caTools")
library(caTools)
library(forecast)
library(AER)
library(dplyr)
anyNA(mydata)
View(mydata)
md.pattern(mydata)
#mydata=filter(mydata,TEH_NAME!= c('Atmakur(m)', 'Chendur', 'Chevemula', 'CHINTHAPALLE', 'Gundlapalle','Huzurnagar', 'KODAD', 'Marriguda', 'Mattampalle', 'Mothkur', 'Narayanapur', 'Neredcherla', 'Peddavoora', 'Thungathurthy', 'Tripuraram', 'TRIPURARAM', 'Vegulapalle'))
msplit<-sample.split(mydata,SplitRatio = 0.8)
mtraining<-subset(mydata,msplit=="TRUE")
mtesting<-subset(mydata,msplit=="FALSE")
pmodel2<-lm(formula=PREMON~POMRB+TEH_NAME+year+temp_pomrb+temp_premon+rain_premon, mtraining)
#plot(pmodel)$
dim(mtraining)
dim(mtesting)


pmodel1<-lm(formula=POMRB~TEH_NAME+year+temp_pomrb+rain_pomrb, mtraining)
#plot(pmodel)$
dim(mtraining)
dim(mtesting)
p1<-predict(pmodel1,mtesting,type="response")
dim(p1)
str(p1)
str(mtesting$POMRB)
accuracy(p1,mtesting$POMRB)

p2<-predict(pmodel2,mtesting,type="response")
dim(p2)
str(p2)
str(mtesting$PREMON)
#actuals_preds2 <- data.frame(cbind(actuals=mtesting$PREMON, predicteds=p2))
#correlation_accuracy2 <- cor(actuals_preds2)
#actuals_preds2
#correlation_accuracy2[2][1]
accuracy(p2,mtesting$PREMON)

pmodel3<-glm(formula=MONSOON~TEH_NAME+year+temp_premon+temp_monsoon+rain_premon+rain_monsoon,family = gaussian(), mtraining)
#plot(pmodel)$
dim(mtraining)
dim(mtesting)
p3<-predict(pmodel3,mtesting,type="response")
dim(p3)
str(p3)
str(mtesting$MONSOON)
accuracy(p3,mtesting$MONSOON)

pmodel4<-glm(formula=POMKH~TEH_NAME+year+temp_monsoon+temp_pomkh+rain_pomkh+rain_monsoon,family = gaussian(), mtraining)
#plot(pmodel)$
dim(mtraining)
dim(mtesting)
p4<-predict(pmodel4,mtesting,type="response")
dim(p4)
str(p4)
str(mtesting$POMKH)
accuracy(p4,mtesting$POMKH)

img<-readJPEG("~/blank-simple-map-of-nalgonda.jpg")

df <- data.frame(mtesting$LAT,mtesting$LON)
ggplot(df, aes(mtesting$LAT,mtesting$LON)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf,Inf) +
  stat_bin2d() +scale_fill_gradient(low="green",high="red")+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) 

