#install.packages('survival')
library(survival)
library(tidyverse)

#### Lectura de datos
data <- read.csv('https://raw.githubusercontent.com/BUFDataScience/2_tidydata_survival_analysis/master/whas500/whas500.csv', header = T)

#### Obtenemos los datos de tipo survival
whas500 <- survival::Surv(time=data$lenfol, event = data$fstat)

#### Análisis de Kaplan-Maier

### SIN VARIABLES > Datos globales

## Visualización
survfit_one <- survival::survfit(whas500 ~ 1)
plot(survfit_one)

## Resumen de datos
sum_one <- survfit(Surv(data$lenfol, data$fstat) ~ 1, data = data)
print(sum_one)

## Supervivencia tras 1 y 3 años
sum_one_1y <- summary(survfit(Surv(data$lenfol, data$fstat) ~ 1, data = data), times = 365.25)
print(sum_one_1y)

sum_one_3y <- summary(survfit(Surv(data$lenfol, data$fstat) ~ 1, data = data), times = 365.25*3)
print(sum_one_3y)

### DOS VARIABLES (GÉNERO)

## Visualización
survfit_gender <- survival::survfit(whas500 ~ gender, data = data)
plot(survfit_gender)

## Resumen de datos
sum_gender <- survfit(Surv(data$lenfol, data$fstat) ~ gender, data = data)
print(sum_gender)

## Supervivencia tras 1 y 3 años
sum_gender_1y <- summary(survfit(Surv(data$lenfol, data$fstat) ~ gender, data = data), times = 365.25)
print(sum_gender_1y)

sum_gender_3y <- summary(survfit(Surv(data$lenfol, data$fstat) ~ gender, data = data), times = 365.25*3)
print(sum_gender_3y)

## pvalue entre género
sd <- survdiff(Surv(data$lenfol, data$fstat) ~ gender, data = data)
pvalue <- 1 - pchisq(sd$chisq, length(sd$n) - 1)
print(pvalue)


#### REGRESIÓN DE COX

## Regresión de Cox (solo con género)
cox_gender <- coxph(Surv(data$lenfol, data$fstat) ~ gender, data = data)
summary(cox_gender)

## Regresión de Cox (con diferentes variables)
cox_multi <- coxph(Surv(data$lenfol, data$fstat) ~ gender + age + diasbp, data =  data)
summary(cox_multi)

#Test de proporcionalidad
#whas500_assump <- survival::cox.zph(whas500_cox)
#whas500_assump