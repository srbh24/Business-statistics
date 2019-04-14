#Reading the input file
cardio = read.csv(file.choose(), header=T)
View(cardio)
attach(cardio)

#How much is the data?
nrow(cardio)

#Summary of data
summary(cardio)

#Type of data
class(cardio) #data.frame
class(Product) #Product is a factor
class(Age) #Age is integer
class(Gender) #Gender is a factor
class(Education) #Education is integer
class(MaritalStatus) #Marital Status is factor
class(cardio$Usage) #Usage is integer
class(cardio$Fitness) #Fitness is integer
class(cardio$Income) #Income is integer
class(cardio$Miles) #Miles is integer

#or simply use the str command

###########################
#Exploratory Data Analysis#
###########################

##Number of cardio products
unique(Product)
table(Product)
#We see that there are 3 cardio products:
##TM195: 80 occurences
##TM498: 60 occurences
##TM798: 40 occurences
cat("There are", (80/180)*100, "% people using TM195,", (60/180)*100, "% people using TM498 and",(40/140)*100, "% people using TM798")
#Plotting a barplot for product frequency
barplot(table(Product), ylab="Count of products")

##Age wise distribution
summary(Age)
#We see that the age range of the people varies from 18 to 50
boxplot(Age)
#Most of the people are withing the 1st and the 2nd quartile ranges i.e. 24 and 26
hist(Age)
#The distribution is right skewed.

##Gender
table(Gender)
cat((104/180)*100, "% people are males while", (76/180)*100, "% are females")
#Plotting a barplot for Gender frequency
colors1 <- c("lightblue", "blue")
col1 <- c("red", "yellow")
barplot(table(Gender), ylab="Gender counts", col=col1)
#Pie chart to show division
mf <- c(104/180, 76/180)
mf_labels <- c("males", "females")
pie(mf, mf_labels, col = c("blue", "pink"))

##Education
table(Education)
#The education level ranges from 12 to 21
boxplot(Education)
#The median and 3rd quartile coincide implies that a huge % of people have education level 16
hist(Education)
#Most people have an education level of 16, followed by 14 and 18. The others are rare.

##Marital Status
table(MaritalStatus)
cat((109/180)*100, "% people are parterned while", (73/180)*100, "% are single")
#Plotting a barplot for marital status frequency
colors2 <- c("hotpink","pink")
barplot(table(MaritalStatus), ylab="Count", col=colors2)

##usage
table(Usage)
#Usage level ranges from 2 to 7, mostly cummulated upto 4.
boxplot(Usage)
#The median coincides with the 1st quartile.
hist(Usage)
#Most people have a usage level of 2, followed by 3 and 4. The others are rare.

##fitness
table(Fitness)
#Usage level ranges from 2 to 5, mostly cummulated at 3.
boxplot(Fitness)
#The median coincides with the 1st quaartile.
hist(Fitness)
#Most people have a usage level of 3. The others are rare.

##income
summary(Income)
#Income ranges from 29562 to 104581
boxplot(Income)
#The salaries are evenly distributed
hist(Income)
#The income is almost normally distributed

##miles
summary(Miles)
#The miles run range from 21 to 360
boxplot(Miles)
#Miles are evenly distributed
hist(Miles)
#Distribution is right skewed

#################################
#Relating features to each other#
#################################

#Analysis of education vs gender
edu_m12 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 12),]
edu_m13 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 13),]
edu_m14 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 14),]
edu_m15 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 15),]
edu_m16 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 16),]
edu_m17 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 17),]
edu_m18 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 18),]
edu_m19 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 19),]
edu_m20 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 20),]
edu_m21 <- cardio[which(cardio$Gender == "Male" & cardio$Education == 21),]
male_edu <- c(
  nrow(edu_m12), 
  nrow(edu_m13), 
  nrow(edu_m14), 
  nrow(edu_m15), 
  nrow(edu_m16), 
  nrow(edu_m17), 
  nrow(edu_m18), 
  nrow(edu_m19), 
  nrow(edu_m20), 
  nrow(edu_m21))
male_edu_level <- data.frame(c(12:21), male_edu)
View(male_edu_level)

edu_f12 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 12),]
edu_f13 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 13),]
edu_f14 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 14),]
edu_f15 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 15),]
edu_f16 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 16),]
edu_f17 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 17),]
edu_f18 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 18),]
edu_f19 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 19),]
edu_f20 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 20),]
edu_f21 <- cardio[which(cardio$Gender == "Female" & cardio$Education == 21),]
female_edu <- c(
  nrow(edu_f12), 
  nrow(edu_f13), 
  nrow(edu_f14), 
  nrow(edu_f15), 
  nrow(edu_f16), 
  nrow(edu_f17), 
  nrow(edu_f18), 
  nrow(edu_f19), 
  nrow(edu_f20), 
  nrow(edu_f21))
female_edu_level <- data.frame(c(12:21), female_edu)
View(female_edu_level)

#This can also be done graphically
plot(Age, Income)
plot(Age, Miles)

#Gender + Age + Marital Status
#install.packages("ggplot2")
library(ggplot2)
ggplot(data=cardio, mapping=aes(x=Gender, y=Age)) + geom_boxplot(aes(color=MaritalStatus))



################################
#Impact of Product - 1 variable#
################################

##Creating age brackets
#install.packages("dplyr")
library(dplyr)
cardio$age_bracket <- ntile(cardio$Age,4)

#Impact on Age brackets
age_bracket1 <- cardio[which(cardio$age_bracket == 1),]
table(age_bracket1$Product)
nrow(age_bracket1)
cat("In Age Group 1,", (25/45)*100, "% people use TM195,", (14/45)*100, "% people use TM498 and",(6/45)*100, "% people use TM798")
barplot(table(age_bracket1$Product), col = "red")

age_bracket2 <- cardio[which(cardio$age_bracket == 2),]
table(age_bracket2$Product)
nrow(age_bracket2)
cat("In Age Group 2,", (16/45)*100, "% people use TM195,", (17/45)*100, "% people use TM498 and",(12/45)*100, "% people use TM798")
barplot(table(age_bracket2$Product), col = "green")

age_bracket3 <- cardio[which(cardio$age_bracket == 3),]
table(age_bracket3$Product)
nrow(age_bracket3)
cat("In Age Group 3,", (20/45)*100, "% people use TM195,", (12/45)*100, "% people use TM498 and",(13/45)*100, "% people use TM798")
barplot(table(age_bracket3$Product))

age_bracket4 <- cardio[which(cardio$age_bracket == 4),]
table(age_bracket4$Product)
nrow(age_bracket4)
cat("In Age Group 4,", (19/45)*100, "% people use TM195,", (17/45)*100, "% people use TM498 and",(9/45)*100, "% people use TM798")
barplot(table(age_bracket4$Product), col="blue")

## Similar thing could be done for Age as well.

#Impact on gender
males <- cardio[which(cardio$Gender == "Male"),]
table(males$Product)
nrow(males)
cat("Amongst males,", (40/104)*100, "% use TM195,", (31/104)*100, "% use TM498 and",(33/104)*100, "% use TM798")
barplot(table(males$Product), col="blue")

females <- cardio[which(cardio$Gender == "Female"),]
table(females$Product)
nrow(females)
cat("Amongst females,", (40/76)*100, "% use TM195,", (29/76)*100, "% use TM498 and",(7/76)*100, "% use TM798")
barplot(table(females$Product), col="pink")

#Males and females together
colors3 <- c("Red", "Yellow")
mf_gender <- table(cardio$Gender, cardio$Product)
barplot(mf_gender, col=colors3, legend=rownames(mf_gender), main="Genderwise products")

## Similar could be done for fitness level to determine which product gives the highest fitness


########################################
#Impact of Product - Multiple variables#
########################################

#Impact on Marital Status and Gender
SM <- cardio[which(cardio$MaritalStatus=="Single" & cardio$Gender=="Male"),]
SF <- cardio[which(cardio$MaritalStatus=="Single" & cardio$Gender=="Female"),]
PM <- cardio[which(cardio$MaritalStatus=="Partnered" & cardio$Gender=="Male"),]
PF <- cardio[which(cardio$MaritalStatus=="Partnered" & cardio$Gender=="Female"),]

table(SM$Product)
barplot(table(SM$Product), main = "Single Males", col="cadetblue2")
table(SF$Product)
barplot(table(SF$Product), main = "Single Females", col="chocolate2")
table(PM$Product)
barplot(table(PM$Product), main = "Partnered Males", col="darkolivegreen3")
table(PF$Product)
barplot(table(PF$Product), main = "Partnered Females", col="lightpink1")

