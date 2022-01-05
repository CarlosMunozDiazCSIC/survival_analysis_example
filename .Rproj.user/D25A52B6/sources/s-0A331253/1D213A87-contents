library(survival)
library(survminer)
library(tidyverse)

# Carga inicial de datos
veteran_aux <- veteran

#Especificamos que los datos sean de tipo survival
veteran <- Surv(time=veteran_aux$time, event = veteran_aux$status)

###################

# Visualización
survfit_vet <- survfit(veteran ~ 1)
plot(survfit_vet)

# Resumen
sum_vet <- survfit(Surv(veteran_aux$time, veteran_aux$status) ~ 1, data = veteran_aux)
print(sum_vet)

# Segundo resumen
summary(veteran_aux$time)
head(sort(veteran_aux$time,decreasing=TRUE), n = 10)

# Supervivencia tras 1 y 3 años
sum_vet_1y <- summary(survfit(Surv(veteran_aux$time, veteran_aux$status) ~ 1, data = veteran_aux), times = 365.25)
print(sum_vet_1y)

sum_vet_3y <- summary(survfit(Surv(veteran_aux$time, veteran_aux$status) ~ 1, data = veteran_aux), times = 365.25*3)
print(sum_vet_3y)

##################

# Visualización
survfit_celltype <- survfit(veteran ~ celltype, data = veteran_aux)
ggsurvplot(survfit_celltype,
           pval = FALSE, 
           conf.int = FALSE,
           ggtheme = theme_minimal(), # Change ggplot2 theme
           palette = c("red", "green", 'blue', 'yellow'))

# Resumen de datos
sum_celltype <- survfit(Surv(veteran_aux$time, veteran_aux$status) ~ celltype, data = veteran_aux)
print(sum_celltype)

# Supervivencia tras 1 y 3 años
sum_celltype_1y <- summary(survfit(Surv(veteran_aux$time, veteran_aux$status) ~ celltype, data = veteran_aux), times = 365.25, extend = TRUE)
print(sum_celltype_1y)

sum_celltype_3y <- summary(survfit(Surv(veteran_aux$time, veteran_aux$status) ~ celltype, data = veteran_aux), times = 365.25*3, extend = TRUE)
print(sum_celltype_3y)

# p-value
sd <- survdiff(Surv(veteran_aux$time, veteran_aux$status) ~ celltype, data = veteran_aux)
pvalue <- 1 - pchisq(sd$chisq, length(sd$n) - 1)
print(pvalue)

################

# Regresión de Cox con celltype
cox_celltype <- coxph(Surv(veteran_aux$time, veteran_aux$status) ~ celltype, data = veteran_aux)
summary(cox_celltype)

# Regresión de Cox con tres variables
cox_multi_vet <- coxph(Surv(veteran_aux$time, veteran_aux$status) ~ celltype + age + trt + prior + diagtime, data =  veteran_aux)
summary(cox_multi_vet)

# Comprobar si es necesario crear alguna nueva categoría