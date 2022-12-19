
################################ Mammary tumours risk of mammary lesion versus any lesion, and risk of malignant or multiple lesions
#### Code: Grace Edmunds September 2021

######### Prep environment 

# clear ws()
rm(list = ls())

getwd()

##set the working directory if not opening an Rproject file (in which case your wd is set to the location of the project)
#setwd(yourworkingdirectoryhere)

# load packages (if not already installed first, install each of these using <install.packages("packagename")>)
library(naniar)
library(data.table)
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(aod)
library(caret)
library(pROC)
library(ResourceSelection)
library(lmtest)
library(sjPlot)
library(sjmisc)


############### Read in the Dataset and clean it up, select the columns you need and name them 

rm(list=ls())

OS_trim <- as.data.frame(fread("./data/mydata.csv"))
head(OS_trim)
OS_trim <- OS_trim[,c(1,2,3,5,6,7)]
colnames(OS_trim)
colnames(OS_trim) <- c("ID", "Breed", "Age", "Sex_Neuter", "Diagnosis", "Number")

#Make a copy of OS in case you make a mistake
OS2 <- OS

############ Check missing values coded as NA and not as anything else or spaces

#exmple using the number of lesions column, repeat for all columns 

table(OS$Number)
#check that everything in the data was coded as yes or no, with no missing values 
list_Number <- c("No","Yes")
OS$Number[(!(OS$Number %in% list_Number))] <- "NA"
#check
table(OS$Number)
#change the coding
OS$Number[((OS$Number == "No"))] <- "Single"
OS$Number[((OS$Number == "Yes"))] <- "Multiple"
#check
table(OS$Number)

#Once you have cleaned it up, make a copy of OS in case you make a mistake
OS2 <- OS

###Get the breed info for OS cases into a table
##Breed

OS_split <- split(OS, OS$Diagnosis)

breed_case <- as.data.frame(table(OS_split[[1]]$Breed))
breed_case$CPercent <- (breed_case$Freq/sum(breed_case$Freq))*100
colnames(breed_case) <- c("breed", "CNumber", "CPercentage")
breed_case

breed_noncase <- as.data.frame(table(OS_split[[2]]$Breed))
breed_noncase$NCPercent <- (breed_noncase$Freq/sum(breed_noncase$Freq))*100
colnames(breed_noncase) <- c("breed", "NCNumber", "NCPercentage")
breed_noncase

OS_split <- split(OS, OS$Number)

breed_case_N <- as.data.frame(table(OS_split[[1]]$Breed))
breed_case_N$CPercent <- (breed_case_N$Freq/sum(breed_case_N$Freq))*100
colnames(breed_case_N) <- c("breed", "CNumber", "CPercentage")
breed_case_N

breed_noncase_N <- as.data.frame(table(OS_split[[2]]$Breed))
breed_noncase_N$NCPercent <- (breed_noncase_N$Freq/sum(breed_noncase_N$Freq))*100
colnames(breed_noncase_N) <- c("breed", "NCNumber", "NCPercentage")
breed_noncase_N

breed_diagnosis <- merge(breed_case, breed_noncase, by = "breed", all= T)
breed_number <- merge(breed_case_N, breed_noncase_N, by = "breed", all= T)
breed_diagnosis$CPercentage <- as.numeric(breed_diagnosis$CPercentage)
breed_diagnosis$CPercentage <- format(round(breed_diagnosis$CPercentage, 1), nsmall = 1)
breed_diagnosis$NCPercentage <- as.numeric(breed_diagnosis$NCPercentage)
breed_diagnosis$NCPercentage <- format(round(breed_diagnosis$NCPercentage, 1), nsmall = 1)

breed_number <- merge(breed_case, breed_noncase, by = "breed", all= T)
breed_number <- merge(breed_case_N, breed_noncase_N, by = "breed", all= T)
breed_number$CPercentage <- as.numeric(breed_number$CPercentage)
breed_number$CPercentage <- format(round(breed_number$CPercentage, 1), nsmall = 1)
breed_number$NCPercentage <- as.numeric(breed_number$NCPercentage)
breed_number$NCPercentage <- format(round(breed_number$NCPercentage, 1), nsmall = 1)

#format for publication and export- diagnosis
breed_diagnosis$CaseNopercent <- paste(breed_diagnosis$CNumber, " ", "(",breed_diagnosis$CPercentage, ")", sep = "")
breed_diagnosis$NonCaseNopercent <- paste(breed_diagnosis$NCNumber, " ", "(",breed_diagnosis$NCPercentage, ")", sep = "")
#format for publication and export - number
breed_number$CaseNopercent <- paste(breed_number$CNumber, " ", "(",breed_number$CPercentage, ")", sep = "")
breed_number$NonCaseNopercent <- paste(breed_number$NCNumber, " ", "(",breed_number$NCPercentage, ")", sep = "")

diagnosis <- breed_diagnosis[,c(1,6,7)]
names(diagnosis) <- c("Breed", "Benign", "Malignant")

write.table(diagnosis, "./analysis/Benign_Malig_Breeds.txt", col.names = T, row.names = F, quote = F, sep = '\t')

number <- breed_number[,c(1,6,7)]
names(number) <- c("Breed", "Multiple", "Single")

write.table(number, "./analysis/Single_Multiple_Breeds.txt", col.names = T, row.names = F, quote = F, sep = '\t')


############ Calculate Further Descriptive Statistics and export in a table 
head(OS)
OS_split <- OS

##Breed
breed_case <- as.data.frame(table(OS_split$Breed))
breed_case$CPercent <- (breed_case$Freq/sum(breed_case$Freq))*100
colnames(breed_case) <- c("breed", "CNumber", "CPercentage")
#format for publication and export
breed_case
breed <- breed_case
breed$CPercentage <- as.numeric(breed$CPercentage)
breed$CPercentage <- format(round(breed$CPercentage, 1), nsmall = 1)
breed$CaseNopercent <- paste(breed$CNumber, " ", "(",breed$CPercentage, ")", sep = "")

#Number
Number_case <- as.data.frame(table(OS_split$Number))
Number_case$CPercent <- (Number_case$Freq/sum(Number_case$Freq))*100
colnames(Number_case) <- c("Number", "CNumber", "CPercentage")
Number <- Number_case
Number$CPercentage <- as.numeric(Number$CPercentage)
Number$CPercentage <- format(round(Number$CPercentage, 1), nsmall = 1)
#format for publication and export
Number$CaseNopercent <- paste(Number$CNumber, " ", "(",Number$CPercentage, ")", sep = "")
# revalue(Number$Number, c("Unrecorded" = "Number_Unrecorded")) -> Number$Number

#Diagnosis
Diagnosis_case <- as.data.frame(table(OS_split$Diagnosis))
Diagnosis_case$CPercent <- (Diagnosis_case$Freq/sum(Diagnosis_case$Freq))*100
colnames(Diagnosis_case) <- c("Diagnosis", "CNumber", "CPercentage")
Diagnosis <- Diagnosis_case
Diagnosis$CPercentage <- as.numeric(Diagnosis$CPercentage)
Diagnosis$CPercentage <- format(round(Diagnosis$CPercentage, 1), nsmall = 1)
#format for publication and export
Diagnosis$CaseNopercent <- paste(Diagnosis$CNumber, " ", "(",Diagnosis$CPercentage, ")", sep = "")
# revalue(Diagnosis$Diagnosis, c("Unrecorded" = "Diagnosis_Unrecorded")) -> Diagnosis$Diagnosis

#Sex_Neuter
sex_case <- as.data.frame(table(OS_split$Sex_Neuter))
sex_case$CPercent <- (sex_case$Freq/sum(sex_case$Freq))*100
colnames(sex_case) <- c("Sex", "CNumber", "CPercentage")
sex <- sex_case
sex$CPercentage <- as.numeric(sex$CPercentage)
sex$CPercentage <- format(round(sex$CPercentage, 1), nsmall = 1)
#format for publication and export
sex$CaseNopercent <- paste(sex$CNumber, " ", "(", sex$CPercentage, ")", sep = "")
#revalue(sex$Sex, c("Unrecorded" = "Sex_Neuter_Unrecorded")) -> sex$Sex

##Age 
age_case <- as.data.frame(table(OS_split$Age))
age_case$CPercent <- (age_case$Freq/sum(age_case$Freq))*100
colnames(age_case) <- c("age", "CNumber", "CPercentage")
age_case
age <- age_case
age$CPercentage <- as.numeric(age$CPercentage)
age$CPercentage <- format(round(age$CPercentage, 1), nsmall = 1)
#format for publication and export
age$CaseNopercent <- paste(age$CNumber, " ", "(", age$CPercentage, ")", sep = "")
#write.table(age, "./analysis/age.txt", col.names = T, row.names = F, quote = F, sep = '\t')

# combine these data frames 
desc <- c(Diagnosis, breed, Number, sex, age)
lapply(desc, as.data.frame )
desc <- list(Diagnosis, breed,Number, sex, age)
desc_stats <- rbindlist(desc, use.names = F)
desc_stats <- desc_stats[, c(1,4)]

#write.table(desc_stats, "./analysis/Desc_stats.txt", row.names = F, col.names = T, quote = F, sep = '\t')

################################### Part 2 - Univariable Logistic Regression

#Go back to the copy of the non-split data
OS <- OS2

# regress breed on mammary Number using logistic regression
colnames(OS)

#make the variables into a factor variable 
OS$breedf <- factor(OS$Breed)
OS$Numberf <- factor(OS$Number)
OS$Diagnosisf <- factor(OS$Diagnosis)
OS$agef <- factor(OS$Age)
OS$sex_neuterf <- factor(OS$Sex_Neuter)

table(OS$Numberf)
colnames(OS)
is.factor(OS$Numberf)
#set the base as single
OS$Numberf <- relevel(OS$Numberf, "Single")
table(OS$Numberf)

table(OS$breedf)
is.factor(OS$breedf)
#set the base
OS$breedf <- relevel(OS$breedf, "Crossbreed")
table(OS$breedf)

table(OS$Diagnosisf)
is.factor(OS$Diagnosisf)
#set the base 
OS$Diagnosisf <- relevel(OS$Diagnosisf, "Benign")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$sex_neuterf)
is.factor(OS$sex_neuterf)
#set the base 
OS$sex_neuterf <- relevel(OS$sex_neuterf, "Female_Entire")

##### NUMBER AS OUTCOME

#A) Breed

#checks
head(OS)
is.factor(OS$breedf)
table(OS$breedf)

#run univ regn
breed_model <- glm(Numberf ~ breedf, data = OS, family = binomial)
model <- breed_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Numberf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "Crossbreed"
Breed_res <- Breed_res[,2:8]

BreedNumber_res <- Breed_res

#B) Age 

is.factor(OS$agef)
table(OS$agef)

#run univ regn
age_model <- glm(Numberf ~ agef, data = OS, family = binomial)
model <- age_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Numberf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "<3"
Breed_res <- Breed_res[,2:8]

AgeNumber_res <- Breed_res
AgeNumber_res$Category <- str_replace(AgeNumber_res$Category, "agef", "")

#C) Sex_neuter 

is.factor(OS$sex_neuterf)
table(OS$sex_neuterf)

#run univ regn
sex_neuter_model <- glm(Numberf ~ sex_neuterf, data = OS, family = binomial)
model <- sex_neuter_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Numberf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "Female_Entire"
Breed_res <- Breed_res[,2:8]

sex_neuterNumber_res <- Breed_res
sex_neuterNumber_res$Category <- str_replace(sex_neuterNumber_res$Category, "sex_neuterf", "")


#Export the results of breed on number
univ_res <- list(BreedNumber_res, AgeNumber_res, sex_neuterNumber_res)
univ_res <- rbindlist(univ_res)
univ_res$OR <- round(univ_res$OR, 2)
univ_res$Pvalue <- round(univ_res$`Pr(>|z|)`,3)
univ_res$Pvalue[univ_res$Pvalue == 0] <- "<0.001"
colnames(univ_res)
res_trim <- univ_res[,c(7,5,6,8)]
colnames(desc_stats)[1] <- "Category"


figure1 <- merge(desc_stats, res_trim, by = "Category", sort = F)
Number <- figure1
write.table(figure1, "./analysis/Univ_Number.txt", row.names = F, col.names = T, quote = F, sep = '\t')

#DIAGNOSIS

#A) Breed

#checks
head(OS)
is.factor(OS$breedf)
table(OS$breedf)

#run univ regn
breed_model <- glm(Diagnosisf ~ breedf, data = OS, family = binomial)
model <- breed_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Diagnosisf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "Crossbreed"
Breed_res <- Breed_res[,2:8]

BreedDiagnosis_res <- Breed_res

#B) Age 

is.factor(OS$agef)
table(OS$agef)

#run univ regn
age_model <- glm(Diagnosisf ~ agef, data = OS, family = binomial)
model <- age_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Diagnosisf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "<3"
Breed_res <- Breed_res[,2:8]

AgeDiagnosis_res <- Breed_res
AgeDiagnosis_res$Category <- str_replace(AgeDiagnosis_res$Category, "agef", "")

#C) Sex_neuter 

is.factor(OS$sex_neuterf)
table(OS$sex_neuterf)

#run univ regn
sex_neuter_model <- glm(Diagnosisf ~ sex_neuterf, data = OS, family = binomial)
model <- sex_neuter_model
#check that R is not dropping any breeds with zero cases 
nobs(model)

# Calculate p-value for model
# In R, the most common way to calculate the p-value for a fitted model is to compare the fitted model to a null 
# model with the anova function.

# Create null model
null = glm (Diagnosisf ~ 1, data = OS, family = binomial(link="logit"))
#get p-value for breed variable 
anova (model,
       null,
       test="Chisq")

Breed_res <- cbind(summary(model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
cat2 <- str_replace_all(cat$Category, "breedf", "")
cat2 <- as.character(cat2)
Breed_res$Category <- cat2
tail(Breed_res)
"(Intercept)" %in% Breed_res$Category
Breed_res$Category[((Breed_res$Category == "(Intercept)"))] <- "Female_Entire"
Breed_res <- Breed_res[,2:8]

sex_neuterDiagnosis_res <- Breed_res
sex_neuterDiagnosis_res$Category <- str_replace(sex_neuterDiagnosis_res$Category, "sex_neuterf", "")

#Export the results of breed on Diagnosis
univ_res <- list(BreedDiagnosis_res, AgeDiagnosis_res, sex_neuterDiagnosis_res)
univ_res <- rbindlist(univ_res)
univ_res$OR <- round(univ_res$OR, 2)
univ_res$Pvalue <- round(univ_res$`Pr(>|z|)`,3)
univ_res$Pvalue[univ_res$Pvalue == 0] <- "<0.001"
colnames(univ_res)
res_trim <- univ_res[,c(7,5,6,8)]
colnames(desc_stats)[1] <- "Category"

figure1 <- merge(desc_stats, res_trim, by = "Category", sort = F)
Diagnosis <- figure1

write.table(figure1, "./analysis/Univ_Diagnosis.txt", row.names = F, col.names = T, quote = F, sep = '\t')


############################################## Part 3 - Multivariable logistic regression

## 1 ### In the number model, we take forward all variables

OS <- OS2
colnames(OS)

#make the variables into a factor variable 
OS$breedf <- factor(OS$Breed)
OS$Numberf <- factor(OS$Number)
OS$Diagnosisf <- factor(OS$Diagnosis)
OS$agef <- factor(OS$Age)
OS$sex_neuterf <- factor(OS$Sex_Neuter)

table(OS$Numberf)
colnames(OS)
is.factor(OS$Numberf)
#set the base as single
OS$Numberf <- relevel(OS$Numberf, "Single")
table(OS$Numberf)

table(OS$breedf)
is.factor(OS$breedf)
#set the base
OS$breedf <- relevel(OS$breedf, "Crossbreed")
table(OS$breedf)

table(OS$Diagnosisf)
is.factor(OS$Diagnosisf)
#set the base 
OS$Diagnosisf <- relevel(OS$Diagnosisf, "Benign")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$sex_neuterf)
is.factor(OS$sex_neuterf)
#set the base 
OS$sex_neuterf <- relevel(OS$sex_neuterf, "Female_Entire")

#trim further
colnames(OS)
OS <- OS[,c(7,8,10,11)]
colnames(OS)

multi_model <- glm(Numberf ~. , data = OS, family = binomial(link="logit"))

#to get the global p-value for each variable in the multivariate model we run the model without that variable then compare to multi_model using 
# liklihood ratio test

noage_model <- glm(Numberf ~ breedf + sex_neuterf, data = OS, family = binomial(link="logit"))
lrtest(multi_model, noage_model)

nobreed_model <- glm(Numberf ~ agef +sex_neuterf , data = OS, family = binomial(link="logit"))
lrtest(multi_model, nobreed_model)

nosex_model <- glm(Numberf ~ agef + breedf , data = OS, family = binomial(link="logit"))
lrtest(multi_model, nosex_model)

#Tabulate the results from this main model for use in Figure 2A
Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace(cat$Category, "breedf","")
cat$Category <- str_replace(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)
colnames(Breed_res)
Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

write.table(Breed_res, "./analysis/Multi_Number.txt", row.names = F, col.names = T, quote = F, sep = '\t')

# get the area under the roc curve
pROC_obj <- roc(OS$Numberf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

pROC_obj$auc

#check if any multi-colinearity
car::vif(multi_model)

#In the diagnosis model we also take forward all variables

OS <- OS2
colnames(OS)

#make the variables into a factor variable 
OS$breedf <- factor(OS$Breed)
OS$Numberf <- factor(OS$Number)
OS$Diagnosisf <- factor(OS$Diagnosis)
OS$agef <- factor(OS$Age)
OS$sex_neuterf <- factor(OS$Sex_Neuter)

table(OS$Numberf)
colnames(OS)
is.factor(OS$Numberf)
#set the base as single
OS$Numberf <- relevel(OS$Numberf, "Single")
table(OS$Numberf)

table(OS$breedf)
is.factor(OS$breedf)
#set the base
OS$breedf <- relevel(OS$breedf, "Crossbreed")
table(OS$breedf)

table(OS$Diagnosisf)
is.factor(OS$Diagnosisf)
#set the base 
OS$Diagnosisf <- relevel(OS$Diagnosisf, "Benign")

table(OS$agef)
is.factor(OS$agef)
#set the base 
OS$agef <- relevel(OS$agef, "<3")

table(OS$sex_neuterf)
is.factor(OS$sex_neuterf)
#set the base 
OS$sex_neuterf <- relevel(OS$sex_neuterf, "Female_Entire")

#trim further
colnames(OS)
OS <- OS[,c(7,9,10,11)]
colnames(OS)

multi_model <- glm(Diagnosisf ~. , data = OS, family = binomial(link="logit"))

#to get the global p-value for each variable in the multivariate model we run the model without that variable then compare to multi_model using 
# liklihood ratio test

noage_model <- glm(Diagnosisf ~ breedf + sex_neuterf, data = OS, family = binomial(link="logit"))
lrtest(multi_model, noage_model)

nobreed_model <- glm(Diagnosisf ~ agef +sex_neuterf, data = OS, family = binomial(link="logit"))
lrtest(multi_model, nobreed_model)

nosex_model <- glm(Diagnosisf ~ agef +breedf, data = OS, family = binomial(link="logit"))
lrtest(multi_model, nosex_model)

#Tabulate the results from this main model for use in Figure 2A
Breed_res <- cbind(summary(multi_model)$coefficients)
Breed_res <- as.data.frame(Breed_res)
Breed_res <- tibble::rownames_to_column(Breed_res, "Breed")
Breed_res$OR <- exp(Breed_res$Estimate)
Breed_res$OR <- round(Breed_res$OR, 2)
UCI <- round(exp(Breed_res$Estimate+(1.96*Breed_res$`Std. Error`)),2)
LCI <- round(exp(Breed_res$Estimate-(1.96*Breed_res$`Std. Error`)),2)
Breed_res$CI <- paste(LCI, UCI, sep = " - ")

cat <- as.data.frame(Breed_res$Breed)
colnames(cat)[1] <- "Category"
table(cat$Category)
cat$Category <- str_replace(cat$Category, "breedf","")
cat$Category <- str_replace(cat$Category, "agef", "")
cat2 <- as.character(cat$Category)
Breed_res$Category <- cat2
tail(Breed_res)
Breed_res$pvalue <- round(Breed_res$`Pr(>|z|)`, 3)
colnames(Breed_res)
Breed_res <- Breed_res[,c(8,6,7,9)]
Breed_res$pvalue[Breed_res$pvalue == 0.000] <- "<0.001"

write.table(Breed_res, "./analysis/Multi_Diagnosis.txt", row.names = F, col.names = T, quote = F, sep = '\t')

# get the area under the roc curve
pROC_obj <- roc(OS$Diagnosisf, multi_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

pROC_obj$auc

#check if any multi-colinearity
car::vif(multi_model)

#get the area under the roc curve 
pROC_obj <- roc(OS$Diagnosisf, Iact_model$fitted.values, plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)

pROC_obj$auc

#check if any multi-colinearity
car::vif(Iact_model)

