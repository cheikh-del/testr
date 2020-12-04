#### Devoir MLG
###Exo2
###On charge la base de donnée 
dt=read.table(file.choose() ,sep=" ",header =TRUE)
dt
attach(dt)
#1
###  le vecteur Y (cible ou sortie)
Y=c(dt$Y)
Y
####la matrice X contenant les 4 variables explicatives X1, . . . ,X4
X=as.matrix(dt[,2:5])
X
### Je Représente Y en fonction de chacune des autres variables
##### Y en fonction X1
plot(Y ~ X1, data = dt,
     xlab = "X1",
     ylab = "Y",
     main = "Y vs X1",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### Y en fonction X2
plot(Y ~ X2, data = dt,
     xlab = "X2",
     ylab = "Y",
     main = "Y vs X2",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### Y en fonction X3
plot(Y ~ X3, data = dt,
     xlab = "X3",
     ylab = "Y",
     main = "Y vs X3",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
##### Y en fonction X4
plot(Y ~ X4, data = dt,
     xlab = "X4",
     ylab = "Y",
     main = "Y vs X4",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

#"2
####Réalisation de la régression linéaire de Y sur l'ensemble des variables
regLm1=lm(Y~X, data = dt)
summary(regLm1)
# R2=0.7972
#On remarque tout les coefficients sont segnificatives

#"3
#####réalisation la régression de Y sur les variables {Xj j =1, . . . , 4, j /= i}
regLm2=lm(Y~X1+X2+X3+X4,data = dt)
summary(regLm2)
e=resid(regLm2)
#### la représentation  les résidus en fonction de Xi
##### e en fonction X1
plot(X1,e,
     xlab = "X1",
     ylab = "residus",
     main = "residus vs X1",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### e en fonction X2
plot(X2,e,
     xlab = "X2",
     ylab = "residus",
     main = "residus vs X2",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### e en fonction X3
plot(X3,e,
     xlab = "X3",
     ylab = "residus",
     main = "residus vs X3",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
##### e en fonction X4
plot(X4,e,
     xlab = "X4",
     ylab = "residus",
     main = " residus vs X4",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
###Commentaire de graphes sur les graphes

library(ggplot2)
qplot(data = dt, X1, fill = I("dodger blue"))
qplot(data = dt, X2, fill = I("dodger blue"))
qplot(data = dt, X3, fill = I("dodger blue"))
qplot(data = dt, X4, fill = I("dodger blue"))

#4
#### la matrice Z contenant les variables X1,X2,X3 et ln(X4)
Z=matrix(c(X1,X2,X3,log(X4)),ncol = 4)
Z

#### la régression de Y sur les variables de Z
regLm3=lm(Y~Z)
summary(regLm3)

##On retire X3
###### Comparaison entre premier modele et dernier modele
# On remarque que R2=0.9923 variables sont significatives sauf Z3 qui pas significatif
# alors que pour la premier modele R2=R2=0.7972 et que tout les variables sont significatives
Z1=matrix(c(X1,X2,log(X4)),ncol = 3)
regLm4=lm(Y~Z1)
summary(regLm4)
##### R2 ajuster n'a pas changer 

#5
X=matrix(c(X1,X2,X3,X4,log(X4)),ncol = 5)
X
regLm5=lm(Y~X)
summary(regLm5)
AIC(regLm4,regLm5)
?AIC
#6
###### on conserve le modele  de question 5
X=matrix(c(X1,X2,log(X4)),ncol = 3)
X
regLm=lm(Y~X)
summary(regLm)
e1=resid(regLm)

###Test de normalité avec R : Test de Shapiro-Wilk
  
shapiro.test(e1)

## On remarque que  p-value non significative. residus  suit donc une loi normale.
#.7
### l'hypothèse d'homoscédasticité
library(lmtest)
bptest(regLm)

?hmctest
hmctest(regLm)

#.8
###intervalle de confiance à 95%,
confint(regLm, level = 0.95)
###intervalle de confiance à 95%,
confint(regLm, level = 0.99)
####intervalle de confiance à 95% pour beta1

#### calcul de XX_bar
n = nrow(dt)
p = length(coef(regLm))
X = cbind(rep(1, n),X)
y = Y
X_X=solve(t(X) %*% X)
X_X
###Calcul de beta_chapeau
(beta_hat = solve(t(X) %*% X) %*% t(X) %*% y)
coef(regLm)
### Sigma 
summary(regLm)$sigma
#### degre de liberté =496
####beta1=2.013
#### sigma_chapeau=41.99517
y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% y
e = y - y_hat
sqrt(t(e) %*% e / (n - p))
#### statistique  student = 1.960
##### avec formule du cours 
c(2.013-1.960*41.99517*sqrt(2.120365e-07), 2.013+1.960*41.99517*sqrt(2.120365e-07))

dtt=cbind.data.frame(Y=dt$Y,X1=dt$X1,X2=dt$X2,X4=log(dt$X4))
dtt
regLm0=lm(Y~X1+X2+X4,data = dtt)
summary(regLm0)
#.9 prévision et un intervalle de confiance
X_pred = data.frame(X1=200,X2=200,X4=log(200))
X_pred

predict(regLm0, newdata = X_pred,interval = "prediction", level = 0.95)

### avec formule du cours 
Xn=as.matrix(c(1,200,200,200))
Xn
Y_n1=solve(t(Xn)%*%beta_hat)
Y_n1
c(Y_n1-1.960*41.99517*sqrt(1+t(Xn)%*%X_X%*%Xn),Y_n1+1.960*41.99517*sqrt(1+t(Xn)%*%X_X%*%Xn))

predict(regLm0, newdata = X_pred,interval = "confidence", level = 0.95)

##EXO03
##################
###On charge la base de donnée 
dataR=read.table(file.choose() ,sep=" ",header =TRUE)
dataR
attach(dataR)
dataR[-which(dataR$X1 < 2500),]
dataR


###1

cor(as.matrix(dataR[,2:6]))
W=as.matrix(dataR[,2:6])
W
cor(W)

install.packages("Hmisc")
library(Hmisc)
#####matrice de p-value 
rcorr(as.matrix(dataR[,2:6]),type=c("pearson"))
cor.test(dataR$X1,dataR$X2,method="pearson")
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrice des coefficients de corrélation
# pmat : matrice des p-valeurs de corrélation
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res<-rcorr(as.matrix(dataR[,2:6]))
flattenCorrMatrix(res$r, res$P)

####2 sélection de variables avec le critère BIC
Y_mod=lm(Y~.,data=dataR)
coef(Y_mod)
Y_mod_bic=step(Y_mod,direction = "backward",k = log(n))
coef(Y_mod_bic)
######################################################################
### les variables que ont doit conserver sont X1,X2,X3 et X5   #######
######################################################################

###3 Representation de Y en fonction des valeurs prédites par le modèle
##### Y en fonction X1
# Changer la couleur et la forme des points
# Changer le type de trait et la couleur
library(ggplot2)
ggplot(dataR, aes(x=X1, y=Y)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")

ggplot(dataR, aes(x=X2, y=Y)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")

ggplot(dataR, aes(x=X3, y=Y)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
ggplot(dataR, aes(x=X4, y=Y)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")

plot(Y ~ X1, data = dataR,
     xlab = "X1",
     ylab = "Y",
     main = "Y vs X1",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### Y en fonction X2
plot(Y ~ X2, data = dataR,
     xlab = "X2",
     ylab = "Y",
     main = "Y vs X2",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### Y en fonction X3
plot(Y ~ X3, data = dataR,
     xlab = "X3",
     ylab = "Y",
     main = "Y vs X3",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
##### Y en fonction X4
plot(Y ~ X5, data = dataR,
     xlab = "X5",
     ylab = "Y",
     main = "Y vs X5",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
######
Y_mod1=lm(Y~X1+X2+X3+X5,data = dataR)
summary(Y_mod1)
r=resid(Y_mod1)
#### la représentation  les résidus en fonction de Xi
##### residus en fonction X1
plot(X1,r,
     xlab = "X1",
     ylab = "residus",
     main = "residus vs X1",
     pch = 20,
     cex = 2,
     col = "dodgerblue")

##### residus en fonction X2
plot(X2,r,
     xlab = "X2",
     ylab = "residus",
     main = "residus vs X2",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
?rstudent
plot(rstudent(Y_mod1), main="Résidus studentisés")

ggplot (rstudent ( Y_mod1 ), pch = 23 , bg = 'blue' , cex = 3 )

ggplot(dataR, aes(x=rstudent ( Y_mod1 ))) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
##### residus en fonction X3
plot(X3,r,
     xlab = "X3",
     ylab = "residus",
     main = "residus vs X3",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
##### residus en fonction X4
plot(X5,r,
     xlab = "X5",
     ylab = "residus",
     main = " residus vs X5",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
plot(r,
     ylab = "residus",
     main = " residus vs X5",
     pch = 20,
     cex = 2,
     col = "dodgerblue")
boxplot(X1)
######Commentaires ici

#calcul du résidu studentisé
res.student <- rstudent(Y_mod1)
PA= subset(res.student,res.student>2|res.student<(-2))
PA
#risque alpha = 0.1
alpha <- 0.5
#calcul du seuil à partir de la loi de Student à (n-p-2) ddl
seuil.student <- qt(1-alpha/2,500-4-2)
#construction du graphique des résidus standardisés
plot(dataR$X1,res.student)
abline(h=-seuil.student)
abline(h=+seuil.student)
abline(h=0)
#détection des points en dehors des tuyaux
ab.student <- dataR[res.student < -seuil.student | res.student > +seuil.student,]
#mettre en évidence les points atypiques dans le graphique
for (i in 1:nrow(ab.student)){
  X <- row(ab.student)[i]
  text(dataR[X,"X1"],res.student[X],X)
}

###4 eventuelle anomalie
boxplot.stats(X1)$out
###5

dataR[-which(dataR$X1 < 2500),]
dataR[-which(dataR$X1 == 936.3827),]
dataR = dataR[-160,]  # On supprime la 160eme ligne



Y_mod1=lm(Y~X1+X2+X3+X5,data = dataR)
summary(Y_mod1)
r=resid(Y_mod1)

###4 eventuelle anomalie

###5

dataR = dataR[-160,]  # On supprime la 160eme ligne


Y_mod1=lm(Y~X1+X2+X3+X5,data = dataR)
summary(Y_mod1)
r=resid(Y_mod1)

