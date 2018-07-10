## ----load-libs, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE--------
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)

## ----read-in-data, echo=FALSE, eval=TRUE---------------------------------
insurance <- read.csv("https://raw.githubusercontent.com/heathergeiger/Data621_hw4/master/insurance_training_data.csv",header=TRUE,stringsAsFactors=FALSE)

insurance <- insurance[,setdiff(colnames(insurance),"INDEX")]

for(column in c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM"))
{
insurance[,column] <- str_replace_all(insurance[,column],pattern='\\$',replace='')
insurance[,column] <- str_replace_all(insurance[,column],pattern=',',replace='')
insurance[,column] <- as.numeric(as.vector(insurance[,column]))
}

## ----read-in-evaluation, echo=FALSE, eval=TRUE---------------------------
evaluation <- read.csv("https://raw.githubusercontent.com/heathergeiger/Data621_hw4/master/insurance-evaluation-data.csv",header=TRUE,stringsAsFactors=FALSE)

evaluation <- evaluation[,setdiff(colnames(evaluation),"INDEX")]

for(column in c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM"))
{
evaluation[,column] <- str_replace_all(evaluation[,column],pattern='\\$',replace='')
evaluation[,column] <- str_replace_all(evaluation[,column],pattern=',',replace='')
evaluation[,column] <- as.numeric(as.vector(evaluation[,column]))
}

## ----num-non-NA-per-column, echo=FALSE, eval=TRUE------------------------
print("Total number of records in data:")
nrow(insurance)

print("Number of non-NA values per variable in data:")

non_NA_per_column <- insurance %>%
gather() %>%
na.omit(value) %>%
count(key)

non_NA_per_column <- data.frame(non_NA_per_column)

non_NA_per_column <- non_NA_per_column[order(non_NA_per_column[,2]),]

data.frame(Variable = non_NA_per_column[,1],n = non_NA_per_column[,2])

## ----num-unique-per-column, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
num_unique_values_per_variable <- insurance %>%
gather() %>%
group_by(key) %>%
summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

num_unique_values_per_variable[order(num_unique_values_per_variable[,2]),]

print("Levels of EDUCATION:")
unique(insurance$EDUCATION)
print("Levels of HOMEKIDS:")
unique(insurance$HOMEKIDS[order(insurance$HOMEKIDS)])

## ----format-binary-vars-training-data, echo=FALSE, eval=TRUE-------------
colnames(insurance) <- plyr::mapvalues(colnames(insurance),
	from=c("CAR_USE","MSTATUS","PARENT1","RED_CAR","REVOKED","SEX","URBANICITY"),
	to=c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural"))

binary_variable_translations <- data.frame(Variable = c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural"),
	True.value = c("Commercial","Yes","Yes","yes","Yes","M","Highly Urban/ Urban"),
	stringsAsFactors=FALSE)

for(i in 1:length(binary_variable_translations$Variable))
{
var <- binary_variable_translations$Variable[i]
true_value <- binary_variable_translations$True.value[i]
insurance[,var] <- ifelse(insurance[,var] == true_value,1,0)
}

## ----format-binary-vars-evaluation-data, echo=FALSE, eval=TRUE-----------
colnames(evaluation) <- plyr::mapvalues(colnames(evaluation),
	from=c("CAR_USE","MSTATUS","PARENT1","RED_CAR","REVOKED","SEX","URBANICITY"),
	to=c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural"))

for(i in 1:length(binary_variable_translations$Variable))
{
var <- binary_variable_translations$Variable[i]
true_value <- binary_variable_translations$True.value[i]
evaluation[,var] <- ifelse(evaluation[,var] == true_value,1,0)
}

## ----barplot-binary-vars, echo=FALSE, eval=TRUE--------------------------
insurance[,c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural","TARGET_FLAG")] %>%
gather("variable","value") %>%
group_by(variable) %>%
count(value) %>%
mutate(value = factor(value)) %>%
mutate(percent = n*100/8161) %>%
ggplot(.,
aes(variable,percent)) +
geom_bar(stat = "identity", aes(fill = value)) +
xlab("Variable") +
ylab("Percent of records") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----barplot-select-discrete-numeric-vars, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
insurance[,c("CLM_FREQ","HOMEKIDS","KIDSDRIV")] %>%
gather("variable","value") %>%
group_by(variable) %>%
count(value) %>%
mutate(value = factor(value,levels=5:0)) %>%
mutate(percent = n*100/8161) %>%
ggplot(.,
aes(variable,percent)) +
geom_bar(stat = "identity", aes(fill = value)) +
xlab("Variable") +
ylab("Percent of records") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_manual(values = rev(c("#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442","#CC79A7")))

## ----convert-blank-job-to-not-stated-training-and-evaluation, echo=FALSE, eval=TRUE----
insurance[insurance$JOB == "","JOB"] <- "Not stated"
evaluation[evaluation$JOB == "","JOB"] <- "Not stated"

## ----barplot-factor-vars, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
for(var in c("EDUCATION","CAR_TYPE","JOB"))
{
frequency_per_level <- data.frame(table(insurance[,var]))
colnames(frequency_per_level) <- c("value","n")

print(ggplot(frequency_per_level,
aes(value,n*100/8161)) +
geom_bar(stat = "identity",col="black",fill="darkgrey") +
xlab("") +
ylab("Percent of records") +
ggtitle(var) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

## ----convert-clmfreq-homekids-and-kidsdriv-to-binary-in-training-and-evaluation, echo=FALSE, eval=TRUE----
colnames(insurance) <- plyr::mapvalues(colnames(insurance),
	from=c("CLM_FREQ","HOMEKIDS","KIDSDRIV"),
	to=c("Past_claim","Kids","Driving_kids"))

colnames(evaluation) <- plyr::mapvalues(colnames(evaluation),
	from=c("CLM_FREQ","HOMEKIDS","KIDSDRIV"),
	to=c("Past_claim","Kids","Driving_kids"))

for(var in c("Past_claim","Kids","Driving_kids"))
{
insurance[,var] <- ifelse(insurance[,var] > 0,1,0)
evaluation[,var] <- ifelse(evaluation[,var] > 0,1,0)
}

## ----barplot-mvr-pts, echo=FALSE, eval=TRUE------------------------------
frequency_per_level <- data.frame(table(insurance[,"MVR_PTS"]))
colnames(frequency_per_level) <- c("value","n")
#frequency_per_level$value <- factor(frequency_per_level$value,levels=as.numeric(frequency_per_level$value)[order(as.numeric(frequency_per_level$value))])
frequency_per_level$value <- factor(frequency_per_level$value,levels=c(0:10,11,13))

ggplot(frequency_per_level,
aes(value,n*100/8161)) +
geom_bar(stat = "identity",col="black",fill="darkgrey") +
xlab("") +
ylab("Percent of records") +
ggtitle("MVR_PTS") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----mvr-pts-max-10-training-and-eval, echo=FALSE, eval=TRUE-------------
insurance$MVR_PTS <- ifelse(insurance$MVR_PTS > 10,10,insurance$MVR_PTS)
evaluation$MVR_PTS <- ifelse(evaluation$MVR_PTS > 10,10,evaluation$MVR_PTS)

## ----barplot-carage-tif-yoj, echo=FALSE, eval=TRUE-----------------------
for(var in c("CAR_AGE","TIF","YOJ"))
{
frequency_per_level <- data.frame(table(insurance[,var]))
colnames(frequency_per_level) <- c("value","n")
frequency_per_level$value <- as.vector(frequency_per_level$value)
frequency_per_level <- rbind(frequency_per_level,data.frame(value = "Not stated",n = length(which(is.na(insurance[,var]) == TRUE)),stringsAsFactors=FALSE))
frequency_per_level$value <- factor(frequency_per_level$value,levels=c(unique(insurance[,var])[order(unique(insurance[,var]))],"Not stated"))

#print(var)
#print(frequency_per_level)
#frequency_per_level$value

print(ggplot(frequency_per_level,
aes(value,n*100/8161)) +
geom_bar(stat = "identity",col="black",fill="darkgrey") +
xlab("") +
ylab("Percent of records") +
ggtitle(var) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

## ----convert-yoj-to-binary-training-and-eval, echo=FALSE, eval=TRUE------
colnames(insurance) <- plyr::mapvalues(colnames(insurance),from="YOJ",to="New_on_the_job")
colnames(evaluation) <- plyr::mapvalues(colnames(evaluation),from="YOJ",to="New_on_the_job")

insurance[,"New_on_the_job"] <- ifelse(insurance[,"New_on_the_job"] == 0,1,0)
evaluation[,"New_on_the_job"] <- ifelse(evaluation[,"New_on_the_job"] == 0,1,0)

insurance[which(is.na(insurance[,"New_on_the_job"]) == TRUE),"New_on_the_job"] <- 0
evaluation[which(is.na(evaluation[,"New_on_the_job"]) == TRUE),"New_on_the_job"] <- 0

## ----convert-homeval-NA-to-0-training-and-eval, echo=FALSE, eval=TRUE----
insurance[which(is.na(insurance$HOME_VAL) == TRUE),"HOME_VAL"] <- 0
evaluation[which(is.na(evaluation$HOME_VAL) == TRUE),"HOME_VAL"] <- 0

## ----count-zeroes-homeval-and-income, echo=FALSE, eval=TRUE--------------
print("Percent of records with HOME_VAL > 0:")
round((length(which(insurance$HOME_VAL > 0))*100)/nrow(insurance),digits=2)

print("Percent of records with INCOME not stated:")
round((length(which(is.na(insurance$INCOME) == TRUE))*100)/nrow(insurance),digits=2)
print("Percent of records with INCOME == 0:")
round((length(which(insurance$INCOME == 0))*100)/nrow(insurance),digits=2)
print("Percent of records with INCOME > 0:")
round((length(which(insurance$INCOME > 0))*100)/nrow(insurance),digits=2)

## ----hist-numeric-vars, echo=FALSE, eval=TRUE,fig.width=12,fig.height=8----
par(mfrow=c(2,4))

for(var in c("AGE","TRAVTIME","BLUEBOOK","HOME_VAL","INCOME","OLDCLAIM","TARGET_AMT"))
{
hist(insurance[insurance[,var] > 0,var],
xlab="Values",
ylab="Number of records",
main=var,
labels=TRUE)
}

hist(insurance$TARGET_AMT[insurance$TARGET_AMT >0 & insurance$TARGET_AMT < 10000],
xlab="Values",
ylab="Number of records",
main="TARGET AMT < $10,000",
labels=TRUE)

## ----add-homeowner-var, echo=FALSE, eval=TRUE----------------------------
insurance <- data.frame(insurance,
	Homeowner = ifelse(insurance$HOME_VAL > 0,1,0),
	stringsAsFactors=FALSE,check.names=FALSE)

evaluation <- data.frame(evaluation,
	Homeowner = ifelse(evaluation$HOME_VAL > 0,1,0),
	stringsAsFactors=FALSE,check.names=FALSE)

## ----recount-unique-values-per-var, echo=FALSE, eval=TRUE----------------
num_unique_values_per_variable <- insurance %>%
gather() %>%
group_by(key) %>%
summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

## ----crash-rate-binary-vars, echo=FALSE, eval=TRUE-----------------------
binary_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques == 2]

binary_variables_data <- insurance[,binary_variables]

binary_variables_data <- data.frame(TARGET_FLAG = rep(binary_variables_data$TARGET_FLAG,times=(ncol(binary_variables_data) - 1)),
	gather(binary_variables_data[,setdiff(colnames(binary_variables_data),"TARGET_FLAG")],"variable","value"),
	stringsAsFactors=FALSE)

binary_variables_data$value <- factor(binary_variables_data$value)

count_per_target_and_predictor <- binary_variables_data %>%
				group_by(TARGET_FLAG,variable) %>%
				count(value)

count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- count_per_target_and_predictor %>%
	group_by(variable,value) %>%
	summarize(total = sum(n))

count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by=c("variable","value"))

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$TARGET_FLAG == 1,]

count_per_target_and_predictor <- data.frame(variable = count_per_target_and_predictor$variable,
	value = count_per_target_and_predictor$value,
	crash.rate = count_per_target_and_predictor$n*100/count_per_target_and_predictor$total)

ggplot(count_per_target_and_predictor,
aes(variable,crash.rate)) +
geom_bar(stat = "identity", aes(fill = value),position="dodge") +
xlab("Variable") +
ylab("Crash rate (%)") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----single-parents-def, echo=FALSE, eval=TRUE---------------------------
print("Number of unmarried individuals with children:")
length(which(insurance[,"Married"] == 0 & insurance[,"Kids"] == 1))
print("Number of single parents:")
length(which(insurance[,"Single_parent"] == 1))

## ----crash-rate-partnered-parents-vs-no-kids, echo=FALSE, eval=TRUE------
print("Crash rate of single parents:")
round((length(which(insurance[,"Single_parent"] == 1 & insurance[,"TARGET_FLAG"] == 1))*100)/length(which(insurance[,"Single_parent"] == 1)),digits=2)

print("Crash rate of partnered parents:")
round((length(which(insurance[,"Kids"] == 1 & insurance[,"Married"] == 1 & insurance[,"TARGET_FLAG"] == 1))*100)/length(which(insurance[,"Kids"] == 1 & insurance[,"Married"] == 1)),digits=2)

print("Crash rate of individuals without children:")
round((length(which(insurance[,"Kids"] == 0 & insurance[,"TARGET_FLAG"] == 1))*100)/length(which(insurance[,"Kids"] == 0)),digits=2)

## ----crash-rate-factor-vars, echo=FALSE, eval=TRUE-----------------------
factor_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques > 2 & num_unique_values_per_variable$Num.uniques < 10]

factor_variables_data <- insurance[,factor_variables]

factor_variables_data <- data.frame(TARGET_FLAG = rep(insurance$TARGET_FLAG,times=ncol(factor_variables_data)),
	gather(factor_variables_data,"variable","value"),
	stringsAsFactors=FALSE,check.names=FALSE)

count_per_target_and_predictor <- factor_variables_data %>%
				group_by(TARGET_FLAG,variable) %>%
				count(value)

count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- count_per_target_and_predictor %>%
			group_by(variable,value) %>%
			summarize(total = sum(n))

count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by=c("variable","value"))
count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$TARGET_FLAG == 1,]

count_per_target_and_predictor <- data.frame(variable = count_per_target_and_predictor$variable,
		value = count_per_target_and_predictor$value,
		crash.rate = count_per_target_and_predictor$n*100/count_per_target_and_predictor$total)

for(var in unique(count_per_target_and_predictor$variable))
{
print(ggplot(count_per_target_and_predictor[count_per_target_and_predictor$variable == var,],
aes(value,crash.rate)) +
geom_bar(stat = "identity") +
xlab("") +
ylab("Crash rate (%)") +
ggtitle(var) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

## ----add-binary-education-minivan-job-to-training-and-eval, echo=FALSE, eval=TRUE----
insurance <- data.frame(insurance,
	College_education = ifelse(insurance$EDUCATION == "Bachelors" | insurance$EDUCATION == "Masters" | insurance$EDUCATION == "PhD",1,0),
	Minivan = ifelse(insurance$CAR_TYPE  == "Minivan",1,0),
	Student = ifelse(insurance$JOB == "Student",1,0),
	Blue_collar = ifelse(insurance$JOB == "z_Blue Collar",1,0),
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	College_education = ifelse(evaluation$EDUCATION == "Bachelors" | evaluation$EDUCATION == "Masters" | evaluation$EDUCATION == "PhD",1,0),
	Minivan = ifelse(evaluation$CAR_TYPE == "Minivan",1,0),
	Student = ifelse(evaluation$JOB == "Student",1,0),
	Blue_collar = ifelse(evaluation$JOB == "z_Blue Collar",1,0),
	check.names=FALSE,stringsAsFactors=FALSE)

## ----car-age-vs-target-flag, echo=FALSE, eval=TRUE-----------------------
print("Crash rate when CAR_AGE = 1:")
round((length(which(insurance$CAR_AGE == 1 & insurance$TARGET_FLAG == 1))*100)/length(which(insurance$CAR_AGE == 1)),digits=2)
print("Crash rate when CAR_AGE >= 20:")
round((length(which(insurance$CAR_AGE >= 20 & insurance$TARGET_FLAG == 1))*100)/length(which(insurance$CAR_AGE >= 20)),digits=2)

count_per_target_and_predictor <- insurance[insurance$CAR_AGE >= 3 & insurance$CAR_AGE <= 19 & is.na(insurance$CAR_AGE) == FALSE,c("CAR_AGE","TARGET_FLAG")] %>%
			count(TARGET_FLAG,CAR_AGE)	

count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- insurance[insurance$CAR_AGE >= 3 & insurance$CAR_AGE <= 19 & is.na(insurance$CAR_AGE) == FALSE,c("CAR_AGE","TARGET_FLAG")] %>%
			count(CAR_AGE)

count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="CAR_AGE")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$TARGET_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$CAR_AGE,
				crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=3:19)

ggplot(count_per_target_and_predictor,
aes(value,crash.rate)) +
geom_bar(stat = "identity") +
xlab("CAR_AGE") +
ylab("Crash rate (%)") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----tif-vs-target-flag, echo=FALSE, eval=TRUE---------------------------
tif_for_barplot <- insurance[insurance$TIF == 1 | insurance$TIF == 3 | insurance$TIF == 4 | insurance$TIF == 6 | insurance$TIF == 7 | insurance$TIF == 9 | insurance$TIF >= 10,c("TARGET_FLAG","TIF")]

tif_for_barplot$TIF <- ifelse(tif_for_barplot$TIF > 10,"11+",tif_for_barplot$TIF)

count_per_target_and_predictor <- tif_for_barplot %>% count(TARGET_FLAG,TIF)
count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- tif_for_barplot %>% count(TIF)
count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="TIF")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$TARGET_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$TIF,
		crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=c("1","3","4","6","7","9","10","11+"))

ggplot(count_per_target_and_predictor,
aes(value,crash.rate)) +
geom_bar(stat = "identity") +
xlab("TIF") +
ylab("Crash rate (%)") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----mvr-pts-vs-target-flag, echo=FALSE, eval=TRUE-----------------------
count_per_target_and_predictor <- insurance[,c("TARGET_FLAG","MVR_PTS")] %>% count(TARGET_FLAG,MVR_PTS)
count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- insurance[,c("TARGET_FLAG","MVR_PTS")] %>% count(MVR_PTS)
count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="MVR_PTS")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$TARGET_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$MVR_PTS,
	crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=0:10)

ggplot(count_per_target_and_predictor,
aes(value,crash.rate)) +
geom_bar(stat = "identity") +
xlab("MVR_PTS") +
ylab("Crash rate (%)") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----mvr-pts-add-constant, echo=FALSE, eval=TRUE-------------------------
par(mfrow=c(1,2))

plot(as.numeric(as.vector(count_per_target_and_predictor$value)),
count_per_target_and_predictor$crash.rate,
xlab="Motor vehicle record points (raw)",
ylab="Crash rate (%)",
type="o")

abline(lm(count_per_target_and_predictor$crash.rate ~ as.numeric(as.vector(count_per_target_and_predictor$value))),lty=2)

plot(ifelse(as.numeric(as.vector(count_per_target_and_predictor$value)) >= 7,as.numeric(as.vector(count_per_target_and_predictor$value)) + 3,as.numeric(as.vector(count_per_target_and_predictor$value))),
count_per_target_and_predictor$crash.rate,
xlab="Motor vehicle record points (modified)",
ylab="Crash rate (%)",
main="Add 3 when points >= 7",
type="o")

abline(lm(count_per_target_and_predictor$crash.rate ~ ifelse(as.numeric(as.vector(count_per_target_and_predictor$value)) >= 7,as.numeric(as.vector(count_per_target_and_predictor$value)) + 3,as.numeric(as.vector(count_per_target_and_predictor$value)))),lty=2)

## ----add-mvr-pts-modified, echo=FALSE, eval=TRUE-------------------------
insurance <- data.frame(insurance,
	MVR_PTS_MOD = ifelse(insurance$MVR_PTS >= 7,insurance$MVR_PTS + 3,insurance$MVR_PTS),
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	MVR_PTS_MOD = ifelse(evaluation$MVR_PTS >= 7,evaluation$MVR_PTS + 3,evaluation$MVR_PTS),
	check.names=FALSE,stringsAsFactors=FALSE)

## ----age-groups-vs-target-flag, echo=FALSE, eval=TRUE--------------------
age_groups <- ifelse(insurance$AGE < 25,"Young (<25)","In-between (25-64)")
age_groups <- ifelse(insurance$AGE >= 65,"Senior (65+)",age_groups)

for(var in c("Young (<25)","In-between (25-64)","Senior (65+)"))
{
crash_rate <- round((length(which(age_groups == var & insurance$TARGET_FLAG == 1))*100)/length(which(age_groups == var)),digits=2)
print(paste0("Crash rate for age group ",var,": ",crash_rate))
}

## ----add-binary-var-young-age, echo=FALSE, eval=TRUE---------------------
insurance <- data.frame(insurance,
	Young_age = ifelse(insurance$AGE < 25,1,0),
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	Young_age = ifelse(evaluation$AGE < 25,1,0),
	check.names=FALSE,stringsAsFactors=FALSE)

insurance[which(is.na(insurance$Young_age) == TRUE),"Young_age"] <- 0
evaluation[which(is.na(evaluation$Young_age) == TRUE),"Young_age"] <- 0

## ----cor-travtime-bluebook-income-target-flag, echo=FALSE, eval=TRUE-----
par(mfrow=c(1,2))
boxplot(TRAVTIME ~ factor(TARGET_FLAG),data=insurance,ylab="TRAVTIME")
boxplot(TRAVTIME ~ factor(TARGET_FLAG),data=insurance[insurance$TRAVTIME < 60,],ylab="TRAVTIME",main="TRAVTIME < 60 only")

par(mfrow=c(1,2))
boxplot(INCOME ~ factor(TARGET_FLAG),data=insurance,ylab="INCOME")
boxplot(BLUEBOOK ~ factor(TARGET_FLAG),data=insurance,ylab="BLUEBOOK")
boxplot(log10(INCOME + 1)  ~ factor(TARGET_FLAG),data=insurance,ylab="log10(INCOME + 1)")
boxplot(log10(BLUEBOOK) ~ factor(TARGET_FLAG),data=insurance,ylab="log10(BLUEBOOK)")

## ----obtain-latest-binary-vars, echo=FALSE, eval=TRUE--------------------
num_unique_values_per_variable <- insurance %>%
gather() %>%
group_by(key) %>%
summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

binary_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques == 2]

binary_variables <- setdiff(binary_variables,c("TARGET_FLAG","Red_car","Sex_male"))

## ----correlations-within-binary, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
binary_variables_data <- insurance[,binary_variables]

correlation_variables <- abs(cor(binary_variables_data,use="pairwise.complete.obs"))

for(i in 1:(nrow(correlation_variables) - 1))
{
correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(binary_variables_data),ncol(binary_variables_data)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(binary_variables_data))
correlation_variables$y <- factor(correlation_variables$y,levels=colnames(binary_variables_data))

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2) +
ggtitle("Magnitude only shown (abs value if negative)")

## ----replot-correlations-within-binary, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
correlation_variables$correlation[as.vector(correlation_variables$x) == "Kids" & as.vector(correlation_variables$y) == "Driving_kids"] <- NA
correlation_variables$correlation[as.vector(correlation_variables$x) == "Single_parent" & as.vector(correlation_variables$y) == "Married"] <- NA
correlation_variables$correlation[as.vector(correlation_variables$x) == "Single_parent" & as.vector(correlation_variables$y) == "Kids"] <- NA

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2) +
ggtitle("Magnitude only shown (abs value if negative)")

## ----correlations-within-numeric, echo=FALSE, eval=TRUE------------------
pairs(insurance[,c("INCOME","BLUEBOOK","TRAVTIME")],lower.panel=NULL,cex=0.5)

print("Correlation INCOME and BLUEBOOK:")
round(cor(insurance$INCOME,insurance$BLUEBOOK,use="pairwise.complete.obs"),digits=2)
print("Correlation INCOME and TRAVTIME:")
round(cor(insurance$INCOME,insurance$TRAVTIME,use="pairwise.complete.obs"),digits=2)
print("Correlation TRAVTIME and BLUEBOOK:")
round(cor(insurance$BLUEBOOK,insurance$TRAVTIME,use="pairwise.complete.obs"),digits=2)

## ----correlations-binary-vs-income, echo=FALSE, eval=TRUE----------------
correlations_income_vs_binary_var <- c()

for(var in binary_variables)
{
correlations_income_vs_binary_var <- c(correlations_income_vs_binary_var,cor(insurance[,var],insurance$INCOME,use="pairwise.complete.obs"))
}

correlations_income_vs_binary_var <- data.frame(Variable = binary_variables,
	Correlation = round(correlations_income_vs_binary_var,digits=2),
	stringsAsFactors=FALSE)

correlations_income_vs_binary_var[order(correlations_income_vs_binary_var$Correlation),]

## ----correlations-binary-vs-travtime, echo=FALSE, eval=TRUE--------------
correlations_travtime_vs_binary_var <- c()

for(var in binary_variables)
{
correlations_travtime_vs_binary_var <- c(correlations_travtime_vs_binary_var,cor(insurance[,var],insurance$TRAVTIME,use="pairwise.complete.obs"))
}

correlations_travtime_vs_binary_var <- data.frame(Variable = binary_variables,
	Correlation = round(correlations_travtime_vs_binary_var,digits=2),
	stringsAsFactors=FALSE)

correlations_travtime_vs_binary_var[order(correlations_travtime_vs_binary_var$Correlation),]

## ----correlation-numeric-vars-target-amt, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
numeric_variables <- c("TIF","CAR_AGE","AGE","INCOME","BLUEBOOK","TARGET_AMT","OLDCLAIM")

numeric_variables_data <- insurance[insurance$TARGET_FLAG == 1,numeric_variables]

numeric_variables_data <- data.frame(numeric_variables_data,
	log10.BLUEBOOK = log10(numeric_variables_data$BLUEBOOK),
	log10.income = log10(numeric_variables_data$INCOME + 1),
	log10.target = log10(numeric_variables_data$TARGET_AMT))

numeric_variables_data$OLDCLAIM <- ifelse(numeric_variables_data$OLDCLAIM > 0,numeric_variables_data$OLDCLAIM,NA)

numeric_variables_data <- data.frame(numeric_variables_data,
	log10.OLDCLAIM = log10(numeric_variables_data$OLDCLAIM))

correlation_variables <- abs(cor(numeric_variables_data,use="pairwise.complete.obs"))

for(i in 1:(nrow(correlation_variables) - 1))
{
correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(numeric_variables_data),ncol(numeric_variables_data)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(numeric_variables_data))
correlation_variables$y <- factor(correlation_variables$y,levels=colnames(numeric_variables_data))

correlation_variables$correlation[correlation_variables$x == "log10.OLDCLAIM" & correlation_variables$y == "OLDCLAIM"] <- NA
correlation_variables$correlation[correlation_variables$x == "BLUEBOOK" & correlation_variables$y == "log10.BLUEBOOK"] <- NA
correlation_variables$correlation[correlation_variables$x == "log10.target" & correlation_variables$y == "TARGET_AMT"] <- NA
correlation_variables$correlation[correlation_variables$x == "log10.income" & correlation_variables$y == "INCOME"] <- NA

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2) +
ggtitle("Magnitude only shown (abs value if negative)")

## ----boxplot-cartype-vs-target-amt, echo=FALSE, eval=TRUE----------------
boxplot(log10(TARGET_AMT) ~ factor(CAR_TYPE,levels=c("Minivan","Sports Car","z_SUV","Pickup","Van","Panel Truck")),
xlab="Car type",
ylab="log10(TARGET_AMT)",
data=insurance[insurance$TARGET_FLAG == 1,])

## ----boxplot-commercial-vs-target-amt, echo=FALSE, eval=TRUE-------------
par(mfrow=c(1,2))

boxplot(log10(TARGET_AMT) ~ factor(Commercial_vehicle),
xlab="Commercial vehicle",
ylab="log10(TARGET_AMT)",
data=insurance[insurance$TARGET_FLAG == 1,])

boxplot(log10(TARGET_AMT) ~ factor(Commercial_vehicle),
xlab="Commercial vehicle",
ylab="log10(TARGET_AMT)",
data=insurance[insurance$TARGET_FLAG == 1 & insurance$TARGET_AMT >= 1000,],
main="TARGET_AMT >= $1,000 only")

## ----correlations-binary-vars-with-target-amt, echo=FALSE, eval=TRUE-----
correlations_binary_vs_target_amt <- c()

for(var in binary_variables)
{
correlations_binary_vs_target_amt <- c(correlations_binary_vs_target_amt,cor(insurance[insurance$TARGET_FLAG == 1,var],log10(insurance[insurance$TARGET_FLAG == 1,"TARGET_AMT"]),use="pairwise.complete.obs"))
}

correlations_binary_vs_target_amt <- data.frame(Variable = binary_variables,
	Correlation = round(correlations_binary_vs_target_amt,digits=2),
	stringsAsFactors=FALSE)

correlations_binary_vs_target_amt[order(correlations_binary_vs_target_amt$Correlation),]

## ----add-dummy-var-for-car-type-to-training-and-eval, echo=FALSE, eval=TRUE----
insurance <- data.frame(insurance,
	Car_type_for_claim_amount = ifelse(insurance$CAR_TYPE != "Van" & insurance$CAR_TYPE != "Panel Truck",0,1),
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	Car_type_for_claim_amount = ifelse(evaluation$CAR_TYPE != "Van" & evaluation$CAR_TYPE != "Panel Truck",0,1),
	check.names=FALSE,stringsAsFactors=FALSE)

insurance$Car_type_for_claim_amount <- ifelse(insurance$CAR_TYPE == "Panel Truck",2,insurance$Car_type_for_claim_amount)
evaluation$Car_type_for_claim_amount <- ifelse(evaluation$CAR_TYPE == "Panel Truck",2,evaluation$Car_type_for_claim_amount)

## ----add-marriage-home-kids-score, echo=FALSE, eval=TRUE-----------------
marriage_home_and_kids_score_insurance <- rep(8,times=nrow(insurance))
marriage_home_and_kids_score_evaluation <- rep(8,times=nrow(evaluation))

marriage_home_and_kids_score_insurance <- ifelse(insurance$Married == 0,marriage_home_and_kids_score_insurance - 1,marriage_home_and_kids_score_insurance)
marriage_home_and_kids_score_insurance <- ifelse(insurance$Homeowner == 0,marriage_home_and_kids_score_insurance - 2,marriage_home_and_kids_score_insurance)
marriage_home_and_kids_score_insurance <- ifelse(insurance$Single_parent == 1,marriage_home_and_kids_score_insurance - 3,marriage_home_and_kids_score_insurance)
marriage_home_and_kids_score_insurance <- ifelse(insurance$Single_parent == 1 & insurance$Homeowner == 0,marriage_home_and_kids_score_insurance - 2,marriage_home_and_kids_score_insurance)

marriage_home_and_kids_score_evaluation <- ifelse(evaluation$Married == 0,marriage_home_and_kids_score_evaluation - 1,marriage_home_and_kids_score_evaluation)
marriage_home_and_kids_score_evaluation <- ifelse(evaluation$Homeowner == 0,marriage_home_and_kids_score_evaluation - 2,marriage_home_and_kids_score_evaluation)
marriage_home_and_kids_score_evaluation <- ifelse(evaluation$Single_parent == 1,marriage_home_and_kids_score_evaluation - 3,marriage_home_and_kids_score_evaluation)
marriage_home_and_kids_score_evaluation <- ifelse(evaluation$Homeowner == 0 & evaluation$Single_parent == 1,marriage_home_and_kids_score_evaluation - 2,marriage_home_and_kids_score_evaluation)

insurance <- data.frame(insurance,
	marriage_home_and_kids_score = marriage_home_and_kids_score_insurance,
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	marriage_home_and_kids_score = marriage_home_and_kids_score_evaluation,
	check.names=FALSE,stringsAsFactors=FALSE)

## ----fill-in-income-NAs, echo=FALSE, eval=TRUE---------------------------
print("Median income in training data:")
median(insurance$INCOME,na.rm=TRUE)
print("Median income in evaluation data:")
median(evaluation$INCOME,na.rm=TRUE)
print("Median income across both:")
median(c(insurance$INCOME,evaluation$INCOME),na.rm=TRUE)

insurance[which(is.na(insurance$INCOME) == TRUE),"INCOME"] <- median(c(insurance$INCOME,evaluation$INCOME),na.rm=TRUE)
evaluation[which(is.na(evaluation$INCOME) == TRUE),"INCOME"] <- median(c(insurance$INCOME,evaluation$INCOME),na.rm=TRUE)

## ----model-select-binary-vars-marriage-home-kids-score-travtime-mvr-pts, echo=FALSE, eval=TRUE----
model1 <- glm(TARGET_FLAG ~ Driving_kids + New_on_the_job + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + marriage_home_and_kids_score,data=insurance,family="binomial")

summary(model1)

## ----model2-add-income, echo=FALSE, eval=TRUE----------------------------
model2 <- glm(TARGET_FLAG ~ Driving_kids + New_on_the_job + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + marriage_home_and_kids_score + log10(INCOME + 1),data=insurance,family="binomial")

summary(model2)

## ----model2-add-income-remove-new-on-the-job, echo=FALSE, eval=TRUE------
model2 <- glm(TARGET_FLAG ~ Driving_kids + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + marriage_home_and_kids_score + log10(INCOME + 1),data=insurance,family="binomial")

summary(model2)

## ----model3-separate-marriage-singleparent-homeowner, echo=FALSE, eval=TRUE----
model3 <- glm(TARGET_FLAG ~ Driving_kids + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + New_on_the_job + Married + Homeowner + Single_parent,data=insurance,family="binomial")

summary(model3)

## ----model1-claim-amt, echo=FALSE, eval=TRUE-----------------------------
model1_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + Car_type_for_claim_amount,data=insurance[insurance$TARGET_FLAG == 1,])

summary(model1_linear_model)

## ----model1-claim-amt-factor-cartype, echo=FALSE, eval=TRUE--------------
model1_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + factor(Car_type_for_claim_amount),data=insurance[insurance$TARGET_FLAG == 1,])

summary(model1_linear_model)

## ----model1-claim-amt-bluebook-only, echo=FALSE, eval=TRUE---------------
model1_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK),data=insurance[insurance$TARGET_FLAG == 1,])

summary(model1_linear_model)

## ----model2-linear-model-backward-selection, echo=FALSE, eval=TRUE-------
insurance_for_model2_linear_model <- data.frame(insurance,
        log10.BLUEBOOK = log10(insurance$BLUEBOOK),
        check.names=FALSE,stringsAsFactors=FALSE)

model2_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + Driving_kids + New_on_the_job + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + marriage_home_and_kids_score,
data=insurance[insurance$TARGET_FLAG == 1,])

summary(model2_linear_model)

## ----model-linear-model-backward-selection-remove-very-high-pvalues, echo=FALSE, eval=TRUE----
model2_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + MVR_PTS_MOD + marriage_home_and_kids_score,
	data=insurance[insurance$TARGET_FLAG == 1,])

summary(model2_linear_model)

## ----model-linear-model-married-homeowner-singleparent-individually, echo=FALSE, eval=TRUE----
model2_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + Kids + Married + Homeowner + Single_parent + MVR_PTS_MOD,
	data=insurance[insurance$TARGET_FLAG == 1,])

summary(model2_linear_model)

## ----model-linear-model-bluebook-married-mvrpts, echo=FALSE, eval=TRUE----
model2_linear_model <- lm(log10(TARGET_AMT) ~ log10(BLUEBOOK) + Married + MVR_PTS_MOD,
	data=insurance[insurance$TARGET_FLAG == 1,])

summary(model2_linear_model)

## ----add-log10, echo=FALSE, eval=TRUE------------------------------------
insurance <- data.frame(insurance,
	log10.INCOME = log10(insurance$INCOME + 1),
	log10.BLUEBOOK = log10(insurance$BLUEBOOK),
	log10.TARGET = log10(insurance$TARGET_AMT + 1),
	check.names=FALSE,stringsAsFactors=FALSE)

evaluation <- data.frame(evaluation,
	log10.INCOME = log10(evaluation$INCOME + 1),
	log10.BLUEBOOK = log10(evaluation$BLUEBOOK),
	log10.TARGET = log10(evaluation$TARGET_AMT + 1),
	check.names=FALSE,stringsAsFactors=FALSE)

model2 <- glm(TARGET_FLAG ~ Driving_kids + TRAVTIME + Commercial_vehicle + Past_claim + Revoked + MVR_PTS_MOD + Urban_not_rural + College_education + Minivan + Young_age + marriage_home_and_kids_score + log10.INCOME,data=insurance,family="binomial")

model1_linear_model <- lm(log10.TARGET ~ log10.BLUEBOOK,data=insurance[insurance$TARGET_FLAG == 1,])
model2_linear_model <- lm(log10.TARGET ~ log10.BLUEBOOK + Married + MVR_PTS_MOD,data=insurance[insurance$TARGET_FLAG == 1,])

## ----confusmatrix, echo=FALSE, eval=TRUE---------------------------------
model1_numeric_predictions <- predict(object = model1,data=insurance,type="response")
model2_numeric_predictions <- predict(object = model2,data=insurance,type="response")
model3_numeric_predictions <- predict(object = model3,data=insurance,type="response")

caret::confusionMatrix(data=ifelse(model1_numeric_predictions > 0.5,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

caret::confusionMatrix(data=ifelse(model2_numeric_predictions > 0.5,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

caret::confusionMatrix(data=ifelse(model3_numeric_predictions > 0.5,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

## ----roc-curves, echo=FALSE, eval=TRUE-----------------------------------
roc(predictor = model1_numeric_predictions,
response = insurance$TARGET_FLAG,
print.thres=c(0.25,0.30,0.35,0.40,0.45,0.5),
main="Model1",
plot=TRUE)

roc(predictor = model2_numeric_predictions,
response = insurance$TARGET_FLAG,
print.thres=c(0.25,0.30,0.35,0.40,0.45,0.5),
main="Model2",
plot=TRUE)

roc(predictor = model3_numeric_predictions,
response = insurance$TARGET_FLAG,
print.thres=c(0.25,0.30,0.35,0.40,0.45,0.5),
main="Model3",
plot=TRUE)

## ----confusmatrix-p-035, echo=FALSE, eval=TRUE---------------------------
caret::confusionMatrix(data=ifelse(model1_numeric_predictions > 0.35,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

caret::confusionMatrix(data=ifelse(model2_numeric_predictions > 0.35,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

caret::confusionMatrix(data=ifelse(model3_numeric_predictions > 0.35,1,0),
reference=insurance$TARGET_FLAG,
positive="1")

## ----precision-and-F1, echo=FALSE, eval=TRUE-----------------------------
true_and_false_positives_and_negatives <- function(dat,actual_column_name,predicted_column_name,positive_value){
        TN_number = length(which(dat[,actual_column_name] != positive_value & dat[,predicted_column_name] != positive_value))
        FP_number = length(which(dat[,actual_column_name] != positive_value & dat[,predicted_column_name] == positive_value))
        TP_number = length(which(dat[,actual_column_name] == positive_value & dat[,predicted_column_name] == positive_value))
        FN_number = length(which(dat[,actual_column_name] == positive_value & dat[,predicted_column_name] != positive_value))
        return(data.frame(TN = TN_number,FP = FP_number,TP = TP_number,FN = FN_number))         
}

precision_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
        true_and_false_for_this_dat <- true_and_false_positives_and_negatives(dat,actual_column_name,predicted_column_name,positive_value)
        return(true_and_false_for_this_dat$TP/(true_and_false_for_this_dat$TP + true_and_false_for_this_dat$FP))
}

sensitivity_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
        true_and_false_for_this_dat <- true_and_false_positives_and_negatives(dat,actual_column_name,predicted_column_name,positive_value)
        return(true_and_false_for_this_dat$TP/(true_and_false_for_this_dat$TP + true_and_false_for_this_dat$FN))
}

specificity_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
        true_and_false_for_this_dat <- true_and_false_positives_and_negatives(dat,actual_column_name,predicted_column_name,positive_value)
        return(true_and_false_for_this_dat$TN/(true_and_false_for_this_dat$TN + true_and_false_for_this_dat$FP))
}

F1_score_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
        precision_this_dat <- precision_homebrew(dat,actual_column_name,predicted_column_name,positive_value)
        sensitivity_this_dat <- sensitivity_homebrew(dat,actual_column_name,predicted_column_name,positive_value)
        return((2 * precision_this_dat * sensitivity_this_dat)/(precision_this_dat + sensitivity_this_dat))
}

precision_model3 <- precision_homebrew(data.frame(Actual = insurance$TARGET_FLAG,Predicted = ifelse(model3_numeric_predictions > 0.35,1,0)),"Actual","Predicted","1")

print(paste0("Precision = ",round(precision_model3,digits=4)))

F1_model3 <- F1_score_homebrew(data.frame(Actual = insurance$TARGET_FLAG,Predicted = ifelse(model3_numeric_predictions > 0.35,1,0)),"Actual","Predicted","1")

print(paste0("F1 = ",round(F1_model3,digits=4)))

## ----residual-plots, echo=FALSE, eval=TRUE,fig.width=12,fig.height=8-----
par(mfrow=c(2,4))

plot(model1_linear_model)

plot(model2_linear_model)

## ----residuals-histograms, echo=FALSE, eval=TRUE-------------------------
par(mfrow=c(1,2))

hist(model1_linear_model$residuals,xlab="Residuals",ylab="# records",main="Model1")
hist(model2_linear_model$residuals,xlab="Residuals",ylab="# records",main="Model2")

## ----check-evaluation-data, echo=TRUE, eval=TRUE-------------------------
apply(evaluation[,c("Driving_kids","Commercial_vehicle","Past_claim","Revoked","MVR_PTS_MOD","Urban_not_rural","College_education","Minivan","Young_age","New_on_the_job","Married","Homeowner","Single_parent")],2,function(x)table(x,useNA="ifany"))

summary(evaluation$TRAVTIME)

summary(evaluation$log10.BLUEBOOK)

## ----apply-models, echo=FALSE, eval=TRUE---------------------------------
numeric_predictions_target_flag <- predict(object = model3,newdata=evaluation,type="response")
factor_predictions_target_flag <- ifelse(numeric_predictions_target_flag > 0.35,1,0)

predictions_target_amt <- predict(object = model2_linear_model,newdata=evaluation[factor_predictions_target_flag == 1,])
predictions_target_amt <- 10^predictions_target_amt

predictions_target_amt_incl_zeroes <- rep(0,times=nrow(evaluation))
predictions_target_amt_incl_zeroes[factor_predictions_target_flag == 1] <- predictions_target_amt

evaluation_predictions <- data.frame(Crash.probability = round(numeric_predictions_target_flag,digits=4),
	TARGET_FLAG = factor_predictions_target_flag,
	TARGET_AMT = round(predictions_target_amt_incl_zeroes,digits=2),
	check.names=FALSE)

write.table(evaluation_predictions,
file="insurance_evaluation_predictions.csv",
row.names=FALSE,col.names=TRUE,quote=FALSE,sep=",")

