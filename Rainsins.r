#Tache 1 : Importation des donnees:
raisen=read.table(file=file.choose(),header=TRUE,sep=",")
#Information Generale sur les donnees
attach(raisen)
dim(raisen)
names(raisen)
colnames(raisen)
rownames(raisen)
summary(raisen)
str(raisen)
#Tache 2 : Pre-traitement des donnees:
#valeurs dupliquées:
duplicated(raisen)
sum(duplicated(raisen))
#Valeurs aberantes:
library(ggplot2)
#Detection des outliers en utlisiant Boxplot:
boxplot(raisen$Area,ylab="Area",xlab="boxplot of the outliers in the area",col="orange")
boxplot(raisen$MajorAxisLength,ylab="MajorAxisLength",xlab="boxplot of the outliers MajorAxisLength",col="orange")
boxplot(raisen$MinorAxisLength,ylab="MinorAxisLength",xlab="boxplot of the outliers in the MinorAxisLength",col="blue")
boxplot(raisen$Eccentricity,ylab="Eccentricity",xlab="boxplot of the outliers in the Eccentricity",col="blue")
boxplot(raisen$ConvexArea,ylab="ConvexArea",xlab="boxplot of the outliers in the ConvexArea",col="red")
boxplot(raisen$Extent,ylab="Extent",xlab="boxplot of the outliers in the Extent",col="red")
boxplot(raisen$Perimeter,ylab="Perimeter",xlab="boxplot of the outliers in the Perimeter",col="green")

#Inter-quantile-Range:
myfun= function(x){
  
  
  Q1=quantile(x, probs=0.25,na.rm = TRUE)
  Q3=quantile(x, probs=0.75,na.rm = TRUE)
  Vmax=Q3+1.5*(Q3-Q1)
  Vmin=Q1-1.5*(Q3-Q1)
  x[x > Vmax|x< Vmin] = NA
  
  return(x)
}

boxplot(raisen$Area)
sum(is.na(raisen$Area))
raisen$Area=myfun(raisen$Area)
sum(is.na(raisen$Area))
boxplot(raisen$Area)

boxplot(raisen$MajorAxisLength)
sum(is.na(raisen$MajorAxisLength))
raisen$MajorAxisLength=myfun(raisen$MajorAxisLength)
sum(is.na(raisen$MajorAxisLength))
boxplot(raisen$MajorAxisLength)

boxplot(raisen$MinorAxisLength)
sum(is.na(raisen$MinorAxisLength))
raisen$MinorAxisLength=myfun(raisen$MinorAxisLength)
sum(is.na(raisen$MinorAxisLength))
boxplot(raisen$MinorAxisLength)

boxplot(raisen$Eccentricity)
sum(is.na(raisen$Eccentricity))
raisen$Eccentricity=myfun(raisen$Eccentricity)
sum(is.na(raisen$Eccentricity))
boxplot(raisen$Eccentricity)

boxplot(raisen$ConvexArea)
sum(is.na(raisen$ConvexArea))
raisen$ConvexArea=myfun(raisen$ConvexArea)
sum(is.na(raisen$ConvexArea))
boxplot(raisen$ConvexArea)

boxplot(raisen$Extent)
sum(is.na(raisen$Extent))
raisen$Extent=myfun(raisen$Extent)
sum(is.na(raisen$Extent))
boxplot(raisen$Extent)

boxplot(raisen$Perimeter)
sum(is.na(raisen$Perimeter))
raisen$Perimeter=myfun(raisen$Perimeter)
sum(is.na(raisen$Perimeter))
boxplot(raisen$Perimeter)
summary(raisen)
#On a Remplacé les valeurs abberantes par les NAs a l'aide de IQR

#Traitement des Valeurs Manquantes
#le taux des valeurs Manquantes
sum(is.na(raisen))/prod(dim(raisen))
summary(is.na(raisen))
#les valeurs manquantes representent 3%<5% alors on peut les supprimer
#imputation des valeurs manquantes en Eccentricity  par KNN
#les valeurs manquantes representent 8% des valeurs de  Eccentricity 8%>5% donc imputation
install.packages("VIM")
library(VIM)
datt=kNN(raisen,imp_var=FALSE)
summary(datt)
View(datt)
sum(is.na(datt))

#Tache3
#Analyse Univariée
#Etude de la normalité en utilisant le shapiro Test
#les hypothese 
#H0 : X suit une normale
#H1 : X ne suit pas une normale
#on va comparer les resultats du shapiro test par rapport a p-value=0.05
shapiro.test(datt$Area)
shapiro.test(datt$MajorAxisLength)
shapiro.test(datt$MinorAxisLength)
shapiro.test(datt$Eccentricity)
shapiro.test(datt$ConvexArea)
shapiro.test(datt$Extent)
shapiro.test(datt$Perimeter)

#les valeurs sont inferieur aux p-value=0.05 donc elles ne sont pas normale
#etude de la modalité des variables
table(datt$Class)
#les differentes valeurs prises par Class sont Besni et Kecimen
hist(datt$Area,col="Skyblue",main="Repartitions des valeurs prises par Area",xlab="Area")
hist(datt$MajorAxisLength,col="Skyblue",main="Repartitions des valeurs prises par MajorAxisLength",xlab="MajorAxisLength")
hist(datt$MinorAxisLength,col="Skyblue",main="Repartitions des valeurs prises par MinorAxisLength",xlab="MinorAxisLength")
hist(datt$Eccentricity,col="Skyblue",main="Repartitions des valeurs prises par Eccentricity",xlab="Eccentricity")
hist(datt$ConvexArea,col="Skyblue",main="Repartitions des valeurs prises par ConvexArea",xlab="ConvexArea")
hist(datt$Extent,col="Skyblue",main="Repartitions des valeurs prises par Extent",xlab="Extent")
hist(datt$Perimeter,col="Skyblue",main="Repartitions des valeurs prises par Perimeter",xlab="Perimeter")
#Tache4
#Analyse bivariee
#etude de la correlation des varaibles
#Vu l'abscence de normalité on va utiliser le coefficent de spearman pour etudier la correlation
plot(datt$Area,datt$MajorAxisLength)
Rho=cor(datt$Area,datt$MajorAxisLength,method="spearman")
Rho
#le coefficent de correlation est egale a 0.9 alors il existe une forte correlation entre les 2 variables

plot(datt$Area,datt$MinorAxisLength)
Rho1=cor(datt$Area,datt$MinorAxisLength,method="spearman")
Rho1

plot(datt$Area,datt$ConvexArea)
Rho2=cor(datt$Area,datt$ConvexArea,method="spearman")
Rho2

plot(datt$Area,datt$Eccentricity)
Rho3=cor(datt$Area,datt$Eccentricity,method="spearman")
Rho3

plot(datt$Area,datt$Extent)
Rho4=cor(datt$Area,datt$Extent,method="spearman")
Rho4


plot(datt$Area,datt$Perimeter)
Rho5=cor(datt$Area,datt$Perimeter,method="spearman")
Rho5



plot(datt$MinorAxisLength,datt$MajorAxisLength)
Rho6=cor(datt$MinorAxisLength,datt$MajorAxisLength,method="spearman")
Rho6

plot(datt$MinorAxisLength,datt$ConvexArea)
Rho8=cor(datt$MinorAxisLength,datt$ConvexArea,method="spearman")
Rho8

plot(datt$MinorAxisLength,datt$Extent)
Rho9=cor(datt$MinorAxisLength,datt$Extent,method="spearman")
Rho9


plot(datt$MinorAxisLength,datt$Perimeter)
Rho10=cor(datt$MinorAxisLength,datt$Perimeter,method="spearman")
Rho10

Rho11=cor(datt$MajorAxisLength, datt$Eccentricity, method = "spearman")
Rho11
plot(Raisin$MajorAxisLength,datt$Eccentricity)

Rho12=cor(Raisin$MajorAxisLength,datt$ConvexArea, method = "spearman")
Rho12
plot(Raisin$MajorAxisLength,datt$ConvexArea)

Rho13=cor(Raisin$MajorAxisLength,datt$Extent, method = "spearman")
Rho13
plot(Raisin$MajorAxisLength,datt$Extent)

Rho14=cor(Raisin$MajorAxisLength,datt$Perimeter, method = "spearman")
Rho14
plot(Raisin$MajorAxisLength,datt$Perimeter)

Rho15=cor(datt$Eccentricity,datt$ConvexArea, method = "spearman")
Rho15
plot(datt$Eccentricity,datt$ConvexArea)

Rho16=cor(datt$Eccentricity,datt$Extent, method = "spearman")
Rho16
plot(datt$Eccentricity,datt$Extents)

Rho17=cor(datt$Eccentricity,datt$Perimeter, method = "spearman")
Rho17
plot(datt$Eccentricity,datt$Perimeter)

Rho18=cor(datt$ConvexArea,datt$Extent, method = "spearman")
Rho18
plot(datt$ConvexArea,datt$Extent)

Rho19=cor(datt$ConvexArea,datt$Perimeter, method = "spearman")
Rho19
plot(datt$ConvexArea,datt$Perimeter)

Rho20=cor(datt$Extent,datt$Perimeter, method = "spearman")
Rho20
plot(datt$Extent,datt$Perimeter)


##test d'hypothese
cor.test(datt$Area,datt$MajorAxisLength, method = "spearman")
#p_value <0.05, donc on accepte H1, les deux variables sont corrolees
cor.test(datt$Area,datt$MinorAxisLength, method = "spearman")
#p_value <0.05, donc on accepte alors H1 ce qui implique les deux variables sont corrolees.
cor.test(datt$Area,datt$Eccentricity, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$Area,datt$ConvexArea, method = "spearman")
#p_value <0.05, donc on accepte H1 ce qui implique les deux variables sont corrolees
cor.test(datt$Area,datt$Extent, method = "spearman")
#p_value>0.05, donc on accepte H0 ce qui impplique les deux variables ne sont pas corrolees
cor.test(datt$Area,datt$Perimeter, method = "spearman")
#p_value <0.05, donc on accepte H1 ce qui implique les deux variables sont corrolees
cor.test(datt$MajorAxisLength,datt$MinorAxisLength, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$MajorAxisLength,datt$Eccentricity, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$MajorAxisLength,datt$ConvexArea, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont fortement corrolees selon le plot
cor.test(datt$MajorAxisLength,datt$Extent, method = "spearman")
#p_value >0.05, donc on accepte H0 ce qui implique les deux variables ne sont pas corrolees
cor.test(datt$MajorAxisLength,datt$Perimeter, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$MinorAxisLength,datt$Eccentricity, method = "spearman")
#p_value>0.05, donc on accepte H0 ce qui impplique les deux variables ne sont pas corrolees selon le plot
cor.test(datt$MinorAxisLength,datt$ConvexArea, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$MinorAxisLength,datt$Extent, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$MinorAxisLength,datt$Perimeter, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$Eccentricity, datt$ConvexArea, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$Eccentricity,datt$Extent, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$Eccentricity,datt$Perimeter, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$ConvexArea,datt$Extent, method = "spearman")
#p_value >0.05, donc on accepte H0 ce qui implique les deux variables ne sont pas corrolees
cor.test(datt$ConvexArea,datt$Perimeter, method = "spearman")
#p_value<0.05, donc on accepte H1 ce qui impplique les deux variables sont faiblement corrolees selon le plot
cor.test(datt$Extent,datt$Perimeter,method = "spearman")


##Correlation avec target class
kruskal.test(datt$Area,datt$Class)
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$MajorAxisLength,datt$Class) 
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$MinorAxisLength,datt$Class) 
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$Eccentricity,datt$Class) 
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$ConvexArea,datt$Class)
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$Extent,datt$Class) 
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables
kruskal.test(datt$Perimeter,datt$Class) 
#p_value<0.05, On accepte H1, il a une dependance entre les deux variables


#Tache5
#Regression Lineaire
RL1=lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+Extent+Class+ConvexArea+Perimeter,data=datt)
RL1
summary(RL1)
#on elemine Area car il possede la valeur de p-value la plus importantes
RL2=lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+Extent+ConvexArea+Perimeter,data=datt)
summary(RL2)
#on eleiminte extent et ConvexArea

RL3=lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+Perimeter,data=datt)
summary(RL3)

RL4=lm(MinorAxisLength~Area+MajorAxisLength+Perimeter,data=datt)
summary(RL4)
#on a egalité alors il faut verifier avec l'AIC

AIC(RL1)
AIC(RL2)
AIC(RL3)
AIC(RL4)
#le meilleur modele est RL2
#ACP
install.packages("devtools")
install.packages(c("FactoMineR", "factoextra"))
install.packages("Factoshiny")
install.packages("caret")
install.packages("tidyverse")
library(caret)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(devtools)
library(Factoshiny)
Raisin1=datt
View(Raisin1)
R=residuals(RL2)
plot(R)
qqnorm(R) ;qqline(R)
Raisin1$Class = NULL
View(Raisin1)
summary(Raisin1)

#PCA

prcomp(Raisin1,scale=TRUE)
pca<-prcomp(Raisin1,scale=TRUE)
#pca summary
summary(pca)
plot(pca,type="lines")






#dattX.pca <- prcomp(Raisin1, center=TRUE, scale=TRUE) 
#df_reduced =dattX.pca$pc1
#df_reduced = as.data.frame(df_reduced)
#df_reduced$MinorAxisLength=Raisin1$MinorAxisLength
# réaliser une régression en fonction de la nouvelle représentation des données
#model= lm(MinorAxisLength~df_reduced$MinorAxisLength, data = df_reduced)
#summary(model)

#Tache6
#regression généralisé
##### GLM
library(stats)
datamodel=datt
attach(datamodel)

#modele gaussien
modelgl <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent +
                 Perimeter + Class, data = datamodel, family = "gaussian")
summary(modelgl)
with(summary(modelgl), 1 - deviance/null.deviance)
AIC(modelgl)
residuals(modelgl)
## R*2=0.9756766

#modele Binomial
modelbnm <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent +
                  Perimeter + Class, data = datamodel, family = "binomial")
## Impossible : les valeurs de y doivent être 0 <= y <= 1
summary(modelbnm)

#modele gamma
modelgamma <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                    Extent + Perimeter + Class , data = datamodel, family = Gamma)
summary(modelgamma)
with(summary(modelgamma), 1 - deviance/null.deviance)
##R*2=0.9288202

## 2ème Gamma
library(mgcv)
mod_gam2 = gam(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                 Extent + Perimeter + Class , data = datamodel)
summary(mod_gam2)
AIC(mod_gam2)
##R*2 adjusted estimée = 0.975

#modele poisson
modelpss <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent +
                  Perimeter + Class , data = datamodel, family = "poisson")
summary(modelpss)
with(summary(modelpss), 1 - deviance/null.deviance)
##R*2=0.9574122