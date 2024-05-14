# Prétraitement :

library(readxl)

donnees <- read_excel("C:/Users/Victor/Downloads/Dataset-M.xlsx")
donnees_propres <- donnees[apply(donnees, 1, function(row) !any(row == -32768)), ] # nolint: line_length_linter.

library(dplyr)

donnees_propres <- donnees_propres %>%
  mutate(Temps = row_number())

off <- 0
donnees_propres_2 <- data.frame()
for (i in seq_len(nrow(donnees_propres))) {
    if (donnees_propres[i, "Ready"] == 0) {
        off <- 30
    }
    else if (donnees_propres[i, "Ready"] == 1 && off <= 0) {
        donnees_propres_2 <- rbind(donnees_propres_2, donnees_propres[i, ])
    }
    off <- off - 1
}

donnees_propres_2_Deg0 <- donnees_propres_2[donnees_propres_2$Deg == 0, ]
donnees_propres_2_Deg1 <- donnees_propres_2[donnees_propres_2$Deg == 1, ]
train_indices_Deg0 <- sample(nrow(donnees_propres_2_Deg0), floor(0.7 * nrow(donnees_propres_2_Deg0)))
train_indices_Deg1 <- sample(nrow(donnees_propres_2_Deg1), floor(0.7 * nrow(donnees_propres_2_Deg1)))


train_data_deg0 <- donnees_propres_2_Deg0[train_indices_Deg0, ]
test_data_deg0 <- donnees_propres_2_Deg0[-train_indices_Deg0, ]

train_data_deg1 <- donnees_propres_2_Deg1[train_indices_Deg1, ]
test_data_deg1 <- donnees_propres_2_Deg1[-train_indices_Deg1, ]


##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
par(mfrow = c(1, 1))
# modèle 1.1.P1 :

y = train_data_deg0$P1
x1 = train_data_deg0$Mo
x2 = train_data_deg0$CO
x3 = train_data_deg0$CR
x4 = train_data_deg0$T1
x5 = train_data_deg0$Temps

mat=data.frame(y,x1,x2,x3,x4,x5)
summary(mat)

library(leaps)
sv=regsubsets(y~x1+x2+x3+x4+x5,data=mat)
summary(sv) #Permet de voir les résultats

plot(sv,scale="adjr2")
plot(sv,scale="bic")
plot(sv,scale="Cp")

re2=lm(y~x1+x2+x3+x4+x5)
summary(re2)

# on prend pas x3 et x4

re2=lm(y~x1+x2+x5)
summary(re2) # R ajusté = 0.972
RMSE <- (mean((re2$resid)^2))^(1/2)
print(RMSE) # 0.0349189

plot(re2$fit,re2$resid,xlab="Prédictions", ylab="Résidus",col='red',main='Régression multiple') # régression multiple : on a pas de patate :(
acf(re2$resid,col='red',xlab="Délai", ylab="Corrélation",main='Régression multiple') # pas d'autocorrélation
Box.test(re2$resid,type="Box-Pierce") # pas d'autocorrélation
Box.test(re2$resid,type="Ljung-Box") # pas d'autocorrélation
hist(re2$resid,probab=TRUE,xlab="Résidus", ylab="Densité",col='red',main='Régression multiple', breaks=100) # régression multiple : on observe des résidus normaux
lines(density(re2$resid),col='green')

library(nortest)
lillie.test(re2$resid) # résidus non normaux

qqnorm(re2$resid) # pas normal


# Verifions la précision de la regression avec le jeux de données test :
y <- test_data_deg0$P1
x1 = test_data_deg0$Mo
x2 = test_data_deg0$CO
x5 = test_data_deg0$Temps

y_pred <- coef(re2)[1] + coef(re2)[2] * x1 + coef(re2)[3] * x2 + coef(re2)[4] * x5
RMSE1 <- (mean((y-y_pred)^2))^(1/2)
print(RMSE1) # 0.03543203
REQMN1 <- RMSE1 / mean(y)
print(REQMN1) #  0.06133496

y1 <- y
y_pred1 <- y_pred

# modèle 1.1.T3P :

y = train_data_deg0$T3P
x1 = train_data_deg0$Mo
x2 = train_data_deg0$CO
x3 = train_data_deg0$CR
x4 = train_data_deg0$T1
x5 = train_data_deg0$Temps

mat=data.frame(y,x1,x2,x3,x4,x5)
summary(mat)

sv=regsubsets(y~x1+x2+x3+x4+x5,data=mat)
summary(sv) #Permet de voir les résultats

plot(sv,scale="adjr2")
plot(sv,scale="bic")
plot(sv,scale="Cp")

re2=lm(y~x1+x2+x3+x4+x5)
summary(re2) # R ajusté = 0.9867
RMSE <- (mean((re2$resid)^2))^(1/2)
print(RMSE) # 4.498608

# on garde toutes les variables

plot(re2$fit,re2$resid,xlab="Prédictions", ylab="Résidus",col='red',main='Régression multiple') # régression multiple : on a pas de patate :(
acf(re2$resid,col='red',xlab="Délai", ylab="Corrélation",main='Régression multiple') # pas d'autocorrélation
Box.test(re2$resid,type="Box-Pierce") # pas d'autocorrélation
Box.test(re2$resid,type="Ljung-Box") # pas d'autocorrélation
hist(re2$resid,probab=TRUE,xlab="Résidus", ylab="Densité",col='red',main='Régression multiple', breaks=100) # régression multiple : on observe des résidus NON normaux
lines(density(re2$resid),col='green')
lillie.test(re2$resid) # résidus non normaux

qqnorm(re2$resid) # pas normal

# Verifions la précision de la regression avec le jeux de données test :
y = test_data_deg0$T3P
x1 = test_data_deg0$Mo
x2 = test_data_deg0$CO
x3 = test_data_deg0$CR
x4 = test_data_deg0$T1
x5 = test_data_deg0$Temps

y_pred <- coef(re2)[1] + coef(re2)[2] * x1 + coef(re2)[3] * x2 + coef(re2)[4] * x3 + coef(re2)[5] * x4 + coef(re2)[6] * x5
RMSE2 <- (mean((y-y_pred)^2))^(1/2)
print(RMSE2) # 4.536698
REQMN2 <- RMSE2 / mean(y)
print(REQMN2) # 0.01397822

y2 <- y
y_pred2 <- y_pred

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

# modèle 1.2.P1 :


y = train_data_deg1$P1
x1 = train_data_deg1$Mo
x2 = train_data_deg1$CO
x3 = train_data_deg1$CR
x4 = train_data_deg1$T1
x5 = train_data_deg1$Temps

mat=data.frame(y,x1,x2,x3,x4,x5)
summary(mat)

sv=regsubsets(y~x1+x2+x3+x4+x5,data=mat)
summary(sv) #Permet de voir les résultats

plot(sv,scale="adjr2")
plot(sv,scale="bic")
plot(sv,scale="Cp")

re2=lm(y~x1+x2+x3+x4+x5)
summary(re2) # R ajusté = 0.8725
RMSE <- (mean((re2$resid)^2))^(1/2)
print(RMSE) # 0.07221479

# on prend tout

plot(re2$fit,re2$resid,xlab="Prédictions", ylab="Résidus",col='red',main='Régression multiple') # PATATE
acf(re2$resid,col='red',xlab="Délai", ylab="Corrélation",main='Régression multiple') # pas d'autocorrélation
Box.test(re2$resid,type="Box-Pierce") # pas d'autocorrélation
Box.test(re2$resid,type="Ljung-Box") # pas d'autocorrélation
hist(re2$resid,probab=TRUE,xlab="Résidus", ylab="Densité",col='red',main='Régression multiple', breaks=100) # régression multiple : on observe des résidus NON normaux
lines(density(re2$resid),col='green')
lillie.test(re2$resid) # résidus non normaux

qqnorm(re2$resid) # pas normal


# Verifions la précision de la regression avec le jeux de données test :
y = test_data_deg1$P1
x1 = test_data_deg1$Mo
x2 = test_data_deg1$CO
x3 = test_data_deg1$CR
x4 = test_data_deg1$T1
x5 = test_data_deg1$Temps

y_pred <- coef(re2)[1] + coef(re2)[2] * x1 + coef(re2)[3] * x2 + coef(re2)[4] * x3 + coef(re2)[5] * x4 + coef(re2)[6] * x5
RMSE3 <- (mean((y-y_pred)^2))^(1/2)
print(RMSE3) # 0.07263966
REQMN3 <- RMSE3 / mean(y)
print(REQMN3) # 0.1419475

y3 <- y
y_pred3 <- y_pred

# modèle 1.2.T3P :

y = train_data_deg1$T3P
x1 = train_data_deg1$Mo
x2 = train_data_deg1$CO
x3 = train_data_deg1$CR
x4 = train_data_deg1$T1
x5 = train_data_deg1$Temps

mat=data.frame(y,x1,x2,x3,x4,x5)
summary(mat)

sv=regsubsets(y~x1+x2+x3+x4+x5,data=mat)
summary(sv) #Permet de voir les résultats

plot(sv,scale="adjr2")
plot(sv,scale="bic")
plot(sv,scale="Cp")

re2=lm(y~x1+x2+x3+x4+x5)
summary(re2) # R ajusté =  0.9684
RMSE <- (mean((re2$resid)^2))^(1/2)
print(RMSE) # 13.13002


plot(re2$fit,re2$resid,xlab="Prédictions", ylab="Résidus",col='red',main='Régression multiple') # pas de pata
acf(re2$resid,col='red',xlab="Délai", ylab="Corrélation",main='Régression multiple') # pas d'autocorrélation
Box.test(re2$resid,type="Box-Pierce") # pas d'autocorrélation
Box.test(re2$resid,type="Ljung-Box") # pas d'autocorrélation
hist(re2$resid,probab=TRUE,xlab="Résidus", ylab="Densité",col='red',main='Régression multiple', breaks=100) # régression multiple : on observe des résidus NON normaux
lines(density(re2$resid),col='green')
lillie.test(re2$resid) # résidus non normaux

qqnorm(re2$resid) # pas normal

y = test_data_deg1$T3P
x1 = test_data_deg1$Mo
x2 = test_data_deg1$CO
x3 = test_data_deg1$CR
x4 = test_data_deg1$T1
x5 = test_data_deg1$Temps

y_pred <- coef(re2)[1] + coef(re2)[2] * x1 + coef(re2)[3] * x2 + coef(re2)[4] * x3 + coef(re2)[5] * x4 + coef(re2)[6] * x5
RMSE4 <- (mean((y-y_pred)^2))^(1/2)
print(RMSE4) # 13.12208
REQMN4 <- RMSE4 / mean(y)
print(REQMN4) # 0.03198438

y4 <- y
y_pred4 <- y_pred

# modèle 1.1.P1  (deg0) : Adjusted R-squared:  0.9708
# modèle 1.1.T3P (deg0) : Adjusted R-squared:  0.9863
# modèle 1.2.P1  (deg1) : Adjusted R-squared:  0.8741
# modèle 1.2.T3P (deg1) : Adjusted R-squared:  0.9677
# On a de meilleur résultats avec T3P plutôt que P1.
# modèle 1.1.P1  (deg0) : RMSE: 0.03543203 ; REQMN: 0.06133496
# modèle 1.1.T3P (deg0) : RMSE: 4.536698   ; REQMN: 0.01397822
# modèle 1.2.P1  (deg1) : RMSE: 0.07263966 ; REQMN: 0.1419475
# modèle 1.2.T3P (deg1) : RMSE: 13.12208   ; REQMN: 0.03198438

par(mfrow = c(2, 2))

plot(y1, type = "l", col = "blue", main = "Modèle P1 (deg0)", xlab = "Indices", ylab = "Valeurs")
lines(y_pred1, type = "l", col = "green")
legend("topright", legend = c("y", "y_pred"), col = c("blue", "green"), lty = 1:1)

plot(y2, type = "l", col = "blue", main = "Modèle T3P (deg0)", xlab = "Indices", ylab = "Valeurs")
lines(y_pred2, type = "l", col = "green")
legend("topright", legend = c("y", "y_pred"), col = c("blue", "green"), lty = 1:1)

plot(y3, type = "l", col = "blue", main = "Modèle P1 (deg1)", xlab = "Indices", ylab = "Valeurs")
lines(y_pred3, type = "l", col = "green")
legend("topright", legend = c("y", "y_pred"), col = c("blue", "green"), lty = 1:1)

plot(y4, type = "l", col = "blue", main = "Modèle T3P (deg1)", xlab = "Indices", ylab = "Valeurs")
lines(y_pred4, type = "l", col = "green")
legend("topright", legend = c("y", "y_pred"), col = c("blue", "green"), lty = 1:1)
