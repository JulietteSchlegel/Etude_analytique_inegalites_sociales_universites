#devoir du 7 decmbre
db_30_11 <- read.csv("C:/Users/schle/Desktop/MASTER_1/econometrie/db_30_11.txt")
View(db_30_11)
BDD<-db_30_11
library(tidyverse)

#on y rajoute la corrélation avec geom_smooth
bac_S<-db_30_11$bac_S
prof_sup<-db_30_11$prof_sup
hommes<-db_30_11$hommes
employe<-db_30_11$employe

ggplot(data=db_30_11, aes(x=prof_sup,y=bac_S))+
  geom_point(col="red")+
  geom_smooth(method="lm",col="orange")+
  geom_text(aes(x=prof_sup,y=bac_S,label=BDD$Libellé),size=3)

cor(bac_S,prof_sup)
cor(bac_S,db_30_11$employe)
cor(bac_S,db_30_11$ouvrier)

#modèle linéaire expliquant le taux de bacheliers S par la part des hommes 
#dans la population étudiante et les différentes origines sociales

#analyse bivariée
lm1<-lm(bac_S~hommes)
summary(lm1)
plot(lm1)
hunter
hunter

lm2<-(lm(bac_S~db_30_11$prof_sup))
summary(lm2)

lm3<-lm(bac_S~db_30_11$employe)
summary(lm3)

lm4<-lm(bac_S~db_30_11$ouvrier)
summary(lm4)
plot(lm4)
hunter

mg1<-lm(bac_S~hommes+prof_sup+employe)
summary(mg1)
plot(mg1)
hunter

mg2<-lm(bac_S~hommes+prof_sup+db_30_11$ouvrier)
summary(mg2)

#test de linéarité
raintest(mg1)
#test d'homoscédasticité
bptest(mg1)
#test de multicolinéarité
vif(mg1)
vif(lm(hommes~prof_sup+employe)) 
vif(lm(prof_sup~employe+hommes)) 
vif(lm(employe~hommes+prof_sup))

hist(residuals(mg1))
shapiro.test(residuals(mg1))
#la p-value est de 0,4> 0,05 donc la distribution normale des résidus est rejeée
bartlett.test(residuals(mg1),)

effectif<-db_30_11$effectif
#modèle logit
glm(data=db_30_11, ranked~effectif+bac_S, family = binomial(link="logit"))

glm1<-glm(data=db_30_11, ranked~effectif+bac_S, family = binomial(link="logit"))
summary(glm1)

library(tidyverse)
db_30_11<-db_30_11 %>% mutate(fitted1=glm1$fitted.values)


#trandformer ranked en variable numérique pour rajouter la courbe dans le graph
db_30_11<-db_30_11 %>% mutate(ranked2=as.integer(ranked))
db_30_11<-db_30_11 %>% mutate(fitted2=glm1$fitted.values)

ggplot(db_30_11)+
  geom_boxplot(aes(x=effectif,y=bac_S),
             col='red',pch='o') + facet_grid(db_30_11$ranked2)

ggplot(db_30_11) + geom_point((aes(x=effectif,y=bac_S)),col='red',pch='o') + 
  facet_grid(db_30_11$ranked2)

summary(db_30_11$)

#suite du devoir : analyse et classification
library(tidyverse)
db11janvier <- read.csv("C:/Users/schle/Desktop/MASTER_1/econometrie/db11janvier.txt", sep=";")
View(db11janvier)
library(FactoMineR)
names(db11janvier )
dbreduite<-db11janvier %>% select(chef_ent,prof_sup,art_comm,agriculteur,employe,ouvrier,prof_inter,bac_S,bac_L,bac_ES,femmes,hommes,licences,masters,doctorats,effectif)
names(dbreduite)

#figure 2
PCA(dbreduite[,1:15],quanti.sup=11:15,row.w = dbreduite$effectif) 
#ici axe horizontal = origine plus ou moins populaire ou aisée ; verticale = en haut bac scientifique et en bas plus littéraire
sortie1<-PCA(dbreduite[,1:15],quanti.sup=11:15,row.w = dbreduite$effectif)
dbreduite<-bind_cols(dbreduite,as_tibble(sortie1$ind$coord))

dimdesc(sortie1)

#figure 1 (pondérée)
barplot(sortie1$eig[,2],main="Valeurs propres",
        names.arg=1:nrow(sortie1$eig))

#rajouter les bacs techno et pro pour voir l'influence
dbreduite2<-db11janvier %>% select(chef_ent,prof_sup,art_comm,agriculteur,employe,ouvrier,prof_inter,femmes,hommes,bac_S,bac_L,bac_ES,bac_pro,bac_techno,effectif)
names(dbreduite2)

PCA(dbreduite2[,1:14],quanti.sup=8:9,row.w = dbreduite$effectif)
sortie2<-PCA(dbreduite2[,1:14],quanti.sup=8:9,row.w = dbreduite$effectif)

dimdesc(sortie2)

barplot(sortie2$eig[,2],main="Valeurs propres",
        names.arg=1:nrow(sortie2$eig))

#figure 3
ggplot(data=dbreduite,aes(x=Dim.1,y=Dim.2))+
  ggtitle("Universités 2019")+  
  geom_point(aes(color=ouvrier))+
  geom_text_repel(aes(color=ouvrier,label=db11janvier$Sigle),size=3)+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_color_gradient(low = "red", high = "green")+
  geom_vline(xintercept = 0,linetype="dashed")+
  theme(plot.title=element_text(hjust=0.5),legend.position=c(.95,.9),
        panel.background =element_rect(fill="white"),legend.title=element_blank())

#zoom sur les universités en vert
dbvar<-as_tibble(sortie1$var$coord)

ggplot(data=dbreduite,aes(x=Dim.1,y=Dim.2))+
  ggtitle("Universités 2019")+  
  geom_point(aes(color=ouvrier))+
  geom_text_repel(aes(color=ouvrier,label=db11janvier$Sigle),size=3)+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_color_gradient(low = "red", high = "green")+
  geom_vline(xintercept = 0,linetype="dashed")+
  geom_point(data=dbvar)+
  coord_cartesian(xlim = c(0, 2.5),ylim=c(-2.5, 2.5))+
  theme(plot.title=element_text(hjust=0.5),legend.position=c(.95,.9),
        panel.background =element_rect(fill="white"),legend.title=element_blank())

#classification

