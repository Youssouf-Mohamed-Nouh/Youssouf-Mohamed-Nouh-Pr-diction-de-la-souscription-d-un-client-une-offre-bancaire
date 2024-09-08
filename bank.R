#-----------------------------------------
# Debut Nettoyage des donnees (Importation)
#-----------------------------------------
data<- read.csv("bank.csv",sep = ";")
# 1- Modifier les noms des colone (Oki)
# 2- Pretraitement de la colone cible
levels(as.factor(data$y)) # (Oki)
# 3- Traitement de la valeur manquant
print(sum(is.na(data))) #(Oki)
#-----------------------------------------
# Fin Nettoyage des donnees (Importation)
#-----------------------------------------
#-----------------------------------------
# Debut Recoder le type des variables (quantitative,qualitative)
#-----------------------------------------
# 1- Variable qualitative(en factor)
library(dplyr)
data<-data%>%
  mutate_if(is.character,as.factor)

# 2- Variable quantitative(en Integer)
data<-data%>%
  mutate_if(is.character,as.integer)

# 3- Recoder les variables qualitative(oki)

# 4- Verifier les valeurs manquant et identifier la colone

print(sum(is.na(data)))# (Oki) Sinon 
apply(data, 2, anyNA)

#-----------------------------------------
# Fin Recoder le type des variables (quantitative,qualitative)
#-----------------------------------------


#-----------------------------------------
# Debut Test Statistique
#-----------------------------------------

# 1- calculter(Pourcentage) Pour les variables qualitative
round(prop.table(table(data$job)),4)*100 #job
round(prop.table(table(data$marital)),4)*100 #marital
round(prop.table(table(data$education)),4)*100 #education
round(prop.table(table(data$default)),4)*100 #default
round(prop.table(table(data$housing)),4)*100 #housing
round(prop.table(table(data$loan)),4)*100 #loan
round(prop.table(table(data$contact)),4)*100 #contact
round(prop.table(table(data$month)),4)*100 #month
round(prop.table(table(data$poutcome)),4)*100 #poutcome
round(prop.table(table(data$y)),4)*100 #y

# 2- calculer(statistique) Pour les variables quantitative
# moyenne,median,max,min
summary(data$age) # age
summary(data$balance) # balance
summary(data$day) # day
summary(data$duration) # duration
summary(data$campaign) # campaign
summary(data$pdays) # pdays
summary(data$previous) # previous

# Variance et ecarte type
var(data$age) # age
sd(data$age) # age
var(data$balance) # balance
sd(data$balance) # balance
var(data$day) # day
sd(data$day) # day
var(data$duration) # duration
sd(data$duration) # duration
var(data$campaign) # campaign
sd(data$campaign) # campaign
var(data$pdays) # pdays
sd(data$pdays) # pdays
var(data$previous) # previous
sd(data$previous) # previous

#-----------------------------------------
# Fin Test Statistique
#-----------------------------------------


#-----------------------------------------
# Debut Visualisation
#-----------------------------------------

library(ggplot2)
library(plotly)

# Les variables quantitative (Histogramme)
n <- nrow(data)
binwidh <- (max(data$age) - min(data$age)) / (log2(n) + 1)

g <- ggplot(data = data, aes(x = age)) + 
  geom_histogram(binwidth = binwidh, fill = "red", color = "blue") +
  labs(title = "Distribution d'âge dans les données",
       x = "Âge", y = "Effectif") +
  stat_bin(binwidth = binwidh, geom = "text", 
           aes(label = ..count..), vjust = -0.5)

g

# Les variables qualitative (bar)
g<-ggplot(data = data,aes(x=job))+
  geom_bar(fill="red",color="blue")+
  labs(title = "Distribution de job dans donne",
       x="Job",
       y="effectif")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

g

# Les variables quantitative et qualitative (Boxplot)
g <- ggplot(data = data, aes(x = y, y = age,fill=y)) +
  geom_boxplot() +
  labs(title = "distribution d'age en fonction cible",
       x="cible",
       y="age")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

g
ggplotly(g)


g<- ggplot(data = data,aes(x=y,y=duration,fill = y))+
  geom_boxplot()+
  labs(title = "distribution d'age en fonction cible",
       x="cible",
       y="duree")
g
ggplotly(g)

#-----------------------------------------
# Fin Visualisation
#-----------------------------------------



#-----------------------------------------
# Debut Test Khi2 (Uniquement pour les variables qualitative)
#-----------------------------------------

# H0: les deux variables sont independant si (p>0.05)
# H1: les deux variables sont dependant si (p<0.05)
chisq.test(data$job,data$y) # H1
chisq.test(data$marital,data$y) # H1
chisq.test(data$education,data$y) # H1
chisq.test(data$default,data$y) # H1
chisq.test(data$housing,data$y) # H0
chisq.test(data$loan,data$y) # H1
chisq.test(data$contact,data$y) # H1
chisq.test(data$previous,data$y) # H1
chisq.test(data$poutcome,data$y) # H1
#-----------------------------------------
# Fin Test Khi2 (Uniquement pour les variables qualitative)
#-----------------------------------------



#-----------------------------------------
# Debut Test shiro-wilk (Uniquement pour les variables quantitative)
#-----------------------------------------

# H0: echantillon suit une distribution normale si p >0.05
# H1: echantillon ne suit pas une distribution normal p<0.05

shapiro.test(filter(data,y=="yes")$age) # H1
shapiro.test(filter(data,y=="yes")$balance) # H1
shapiro.test(filter(data,y=="yes")$day) # H1
shapiro.test(filter(data,y=="yes")$duration) # H1
shapiro.test(filter(data,y=="yes")$campaign) # H1
shapiro.test(filter(data,y=="yes")$pdays) # H1

#-----------------------------------------
# Fin Test shiro-wilk (Uniquement pour les variables quantitative)
#-----------------------------------------


#-----------------------------------------
# Debut Mann-whitney
#-----------------------------------------

# H0: il n'y a pas de difference significative entre le moyenne deux variable si p>0.05
# H1: il n'y  de difference significative entre le moyenne deux variable si p<0.05

wilcox.test(data$age~data$y) # H0
wilcox.test(data$balance~data$y) # H1
wilcox.test(data$day~data$y) # H0
wilcox.test(data$duration~data$y) # H1
wilcox.test(data$campaign~data$y) # H1
wilcox.test(data$pdays~data$y) # H1

#-----------------------------------------
# Fin Mann-whitney
#-----------------------------------------


#-----------------------------------------
#Machine Learning 
#-----------------------------------------
library(caret)
library(dplyr)
library(naivebayes)
set.seed(3033)
# 1. verfier les variables qui ont qu'une seule valeur (oki)
sapply(data,function(x) length(x)==1)
# 2. supprimer une fois trouver  (oki)
# 3. tranformer categorielle en quantitative
dummy_variable=dummyVars(~.,data,fullRank = TRUE)
dummy_variable_data<-predict(dummy_variable,newdata = data)
dummy_variable_data<-as.data.frame(dummy_variable_data)
dummy_variable_data$"Souscription" <- ifelse(dummy_variable_data$"y.yes" == 1, "Yes", "No")
dummy_variable_data$y.yes=NULL
dummy_variable_data<-dummy_variable_data%>%
  mutate_if(is.character,as.factor)
# 3. Division de donne
training<-floor(0.7*nrow(dummy_variable_data))
indices<-sample(seq_len(nrow(dummy_variable_data)),training)
data_train<-dummy_variable_data[indices,]
data_test<-dummy_variable_data[-indices,]

# 5. Normalisation de donnees
data_value_proprocess<-preProcess(data_train,method=c("center","scale"))
data_train.scale<-predict(data_value_proprocess,data_train)
data_test.scale<-predict(data_value_proprocess,data_test)
# 6
# downsample
table(data_train.scale[,"Souscription"])
set.seed(3033)
'%in%'=Negate("%in%")
data_train.scale.dowsample<-downSample(x=data_train.scale[,names(data_train.scale)%in%"Souscription"],
                                       y=as.factor(data_train.scale$"Souscription"))

names(data_train.scale.dowsample[names(data_train.scale.dowsample)=="Class"])=="Souscription"
colnames(data_train.scale.dowsample)[which(names(data_train.scale.dowsample)=="Class")]="Souscription"

data_test.scale.dowsample<-downSample(x=data_test.scale[,names(data_test.scale)%in%"Souscription"],
                                      y=as.factor(data_test.scale$"Souscription"))

names(data_test.scale.dowsample[names(data_test.scale.dowsample)=="Class"])=="Souscription"
colnames(data_test.scale.dowsample)[which(names(data_test.scale.dowsample)=="Class")]="Souscription"
# 7. Creation du modeles
trainControl<-trainControl(method = "cv",number = 10)
naive_baye<-train(Souscription~.,data = data_train.scale,method="svmRadial",preProcess=NULL)
print(naive_baye)
# Test
prediction_test<-predict(naive_baye,newdata = data_test.scale[-ncol(data_test.scale)])
# creation matrice confusion 
confusionMatrix(prediction_test,as.factor(data_test.scale[,ncol(data_test.scale)]))

# Avec downsample
set.seed(3033)
trainControl<-trainControl(method = "cv",number = 10)
naive_baye_dowsample<-train(Souscription~.,data = data_train.scale.dowsample,method="rf",preProcess=NULL)
print(naive_baye_dowsample)




