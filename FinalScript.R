library(readr)
library(stargazer) # generates html version of table
library(jtools)
library(coefplot)
library(plyr)
# Reading in pilot data
d<- read_csv("anes_pilot_2016.csv")

# Filtering out data that is not relevant and recoding certain variables to be binary

# Filtering gender to show only female respondents due to article specifications
d<- subset(d, gender == 2)

# Creating a subset of the dataframe to remove the skipped value for 
# disc_wo and only displaying repsponses that weren't skipped
d <- subset(d, disc_wo < 8)
d$disc_wo <- factor(d$disc_wo)
d$disc_wo <- revalue(d$sign, c("1" ="1", "2"="1","3"="1","4"="1","5"="0"))
table(d$disc_wo)

# Dropping values >97 because it likely correlates to "Did not Respond/Chose not to Respond"
table(d$faminc)
d<- subset(d, faminc<97)
barplot(table(d$faminc))
# Creating a barplot for dependent var "sign"
sign<- c("Have Done","Have not Done")
barplot(table(d$sign),
        main = "Worn Campaign Button, Sticker, or Placed Sign in Yard",
        xlab= "Done this or not Done This",
        ylab= "Respondents",
        ylim= c(0,1000),
        names.arg = sign,
        col = "royalblue"
)


# Refactoring sign variable and replacing them with 0's and 1's
# 0 for have worn/displayed 1 for have not worn/displayed
d$sign <- factor(d$sign)
d$sign <- revalue(d$sign, c("1" ="0", "2"="1"))
table(d$sign)

# Addressing the Control Variables 
# Author group age by decades using brithyr
# Creating age variable by subtracting birthyr from 2016(pilot data year)
age<- 2016-d$birthyr
# Adding a column for the age in decade and assigning values to each decade from 1 to 9
d$ageInDecade<- NA
d$ageInDecade[age>=90]<-9
d$ageInDecade[age<90 & age>=80]<-8
d$ageInDecade[age<80 & age>=70]<-7
d$ageInDecade[age<70 & age>=60]<-6
d$ageInDecade[age<60 & age>=50]<-5
d$ageInDecade[age<50 & age>=40]<-4
d$ageInDecade[age<40 & age>=30]<-3
d$ageInDecade[age<30 & age>=20]<-2
d$ageInDecade[age<20]<-1
table(d$ageInDecade)
# Making X-axis labels for decade grouping
decs<- c("18-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
# Barplot of ageInDecade
barplot(table(d$ageInDecade),
        main = "Age by Decade",
        xlab = "Decade Grouping",
        ylab = "Respondents",
        ylim = c(0,150),
        names.arg = decs,
        col = "royalblue")
# Refactoring pid7, which measures partisan strength and breaking them down into 3 values between 0 and   1
d<- subset(d, pid7<8)
d<-subset(d, pid7!=4)
d$pid7<- factor(d$pid7)
table(d$pid7)
# 1 and 7 are strong Democrat and strong Republican, respectively
# 2 and 6 are weak Democrat and weak Republican, respectively
# 3 and 5 are lean Democrat and lean Republican, respectively
d$pid7<- revalue(d$pid7, c("1"="1", "7"="1", "2"="0", "6"="0","3"=".5", "5"=".5"))
table(d$pid7)
party<- c("Strong","Weak","Lean")
barplot(table(d$pid7),
        main = "Partisan Strength",
        xlab = "Partisan Strength",
        ylab = "Respondents",
        ylim = c(0,250),
        names.arg = party,
        col = "royalblue"
)
# Refactoring Marital Status of Women. 1 for Married, 0 for not Married
d$marstat<-factor(d$marstat)
d$marstat<- revalue(d$marstat, c("1"="1","2"="0","3"="0","4"="0","5"="0","6"="0"))
table(d$marstat)
marriage<- c("Married","Not Married")
barplot(table(d$marstat),
        main= "Marital Status of Respondents",
        xlab = "Staus",
        ylab= "Respondents",
        ylim = c(0,300),
        col = "royalblue",
        names.arg = marriage
)
# Refactoring/revaluing racial status of respondents. 1 for White, 0 for non-White
d$race <- factor(d$race)
d$race <- revalue(d$race, c("1"= "1", "2"="0","3"="0","4"="0","5"="0","6"="0","7"="0"))
table(d$race)
white<- c("White","Not White")
barplot(table(d$race),
        main = "Racial Makeup of Respondents",
        xlab= "Race",
        ylab = "Respondents",
        ylim = c(0,500),
        col = "royalblue",
        names.arg = white)
table(d$religpew)
# Refactoring/revaluing Religion variable. 1 for Religious, 2 for Not Religious
# Both 9 and 12 correlate to Not Religious
# Dropping values that are greater than 12
d<- subset(d, religpew<13)
d$religpew <- factor(d$religpew)
d$religpew <- revalue(d$religpew,c("1"="1","2"="1","3"="1","4"="1","5"="1","6"="1","7"="1","8" ="1", "9"="0","10"="1","11"="1","12"="0"))
table(d$religpew)
religion<- c("Is Religious","Is Not Religious")
barplot(table(d$religpew),
        main = "Is Respondent Religious?",
        xlab = "Religious or Not",
        ylab = "Respondents",
        ylim = c(0,600),
        names.arg = religion)


# Creating a new variable called "new" to hold the percent16 data and change it to
# a binomial variable
# We noticed that a majority of people claimed that they were at least 90% likely
# to vote. So we split it into 2 groups. 0 for <90, 1 for >90
d$new<- NA
d$new[d$percent16<90]<-0
d$new[d$percent16>=90]<-1
table(d$new)
# Refactoring disc_selfsex which reports experienced discrimination
# We are having "A Great Deal" to "A Little" to be coded as a 1 with "None" as a 0
d$disc_selfsex <- factor(d$disc_selfsex)
d$disc_selfsex <- revalue(d$disc_selfsex, c("1" ="1", "2"="1", "3"="1", "4"="1", "5"="0"))
table(d$disc_selfsex)

# Refactoring/revaluing education
# Dividing each education level by 6. 1 for no High School up to 6 for Post-Grad
d$educ <- factor(d$educ)
d$educ <- revalue(d$educ, c("1" = "0","2" = ".2","3"="0.4","4"= ".6","5"=".8","6"="1"))
table(d$educ)

# Regression. Using alpha value of 0.62 like the author used in her study
# Anything with a p-value< alpha will be considered significant to models and allow us
# To reject the null hypotheses

# Refactoring give12 to span from 0 to 1
table(d$give12mo)
d$give12mo <- factor(d$give12mo)
d$give12mo <- revalue(d$give12mo, c("1" ="0", "2"="1"))
table(d$give12mo)

#ref from https://www.statology.org/mode-in-r/
getMode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#### Table of Independent Variables ####
#import make function to find mode


means_ind <- c("-","-")
mode_ind <- c(getMode(d$disc_selfsex),getMode(d$disc_wo))
sd_ind <- c("-","-")
max_ind <- c("-","-")
min_ind <- c("-","-")
counts_ind <- c(length(unique(d$disc_selfsex)), length(unique(d$disc_wo)))
observations_ind <- c(nrow(d), nrow(d))

tab_ind <- matrix(c(means_ind, sd_ind, mode_ind, max_ind, min_ind, counts_ind, observations_ind), ncol=7)

rownames(tab_ind) <- c("Experienced Sexism (disc_selfsex)", "Percieved Sexism (disc_wo)")
colnames(tab_ind) <- c("Mean","St. Dev","Mode","Max", "Min", "Unique Values", "Observations")



#### Table of Dependent Variables ####
means_dep <- c("-", "-")
mode_dep <- c(getMode(as.integer(d$politicalEngagement)),getMode(as.integer(d$newPercent16)))
sd_dep <- c("-","-")
max_dep <- c("-","-")
min_dep <- c("-","-")
counts_dep <- c(length(unique(d$politicalEngagement)),length(unique(d$newPercent16)))

tab_dep <- matrix(c(means_dep,sd_dep,mode_dep, max_dep,min_dep,counts_dep, observations_ind),ncol = 7,nrow = 2)

rownames(tab_dep) <- c("Political Engagement (sign and give12mo)", "Chance of Voting (percent16)")
colnames(tab_dep) <- c("Mean","St. Dev","Mode","Max", "Min", "Unique Values", "Observations")

#### Table Control Variables ####

means_con <- c("-","-", "-", (2016 - mean(as.integer(d$birthyr))),"-",mean(as.integer(d$faminc)),"-")
mode_con <- c(getmode(d$marstat),getMode(d$educ),getMode(d$race), (2016 - getMode(d$birthyr)), getMode(d$religpew), getMode(d$faminc),getMode(d$pid7))
sd_con <- c("-","-", "-", sd(as.integer(d$birthyr)),"-",sd(as.integer(d$faminc)),"-")
# for birthyear where a birth year of 1924 is smaller than 1996, if taking the min 1924 would be the min value
# but really the age is older than one who was born in 1996. So max and min will be flipped for the birth year variable
# to make the correct finding on age when subtracting it from 2016.
max_con <- c("-","-", "-", 2016 - min(as.integer(d$birthyr)),"-",max(as.integer(d$faminc)),"-")
min_con <- c("-","-", "-", 2016 - max(as.integer(d$birthyr)),"-",min(as.integer(d$faminc)),"-")
unique_con <- c(length(unique(d$marstat)), length(unique(d$educ)),length(unique(d$race)),
                length(unique(d$birthyr)),length(unique(d$religpew)), length(unique(d$faminc)), 
                length(unique(d$pid7)))
observations_con <- c(nrow(d),nrow(d),nrow(d),nrow(d),nrow(d),nrow(d),nrow(d))

tab_con <- matrix(c(means_con,sd_con,mode_con,max_con,min_con,unique_con,observations_con),ncol = 7)

rownames(tab_con) <- c("Marital Status (marstat)", "Education (educ)", "Race (race)", "Age (birthyr)", "Religion (religpew)", "Family Income (faminc)", "Partisian Strength (pid7)")
colnames(tab_con) <- c("Mean","St. Dev","Mode","Max", "Min", "Unique Values", "Observations")

tab_con

# Creating Binary Political Engagement where 0 has done no political engagement
# and 1 means the respondent did some sort of political engagement in sign or give12mo

d$politicalEngagement <- 0
d$politicalEngagement[d$sign == 0] <- 1
d$politicalEngagement[d$give12mo == 1] <- 1
table(d$politicalEngagement)


# Checking on analyses
# Analysis 1 checks political engagement AND chance of voting based on EXPERIENCED discrimination
# Signage based on experience
m9.1<- glm(politicalEngagement~ disc_selfsex, data = d, family = binomial)
summary(m9.1)
coefplot(m9.1, intercept = FALSE)
m9.1effect<- effect_plot(m9.1, pred = disc_selfsex, interval= TRUE,
                         x.label = "Experienced Discrimination",
                         y.label = "Political Engagement")
m9.1effect
# Do Chi-Square test on the two variables
chisq_1 <- chisq.test(d$disc_selfsex,d$politicalEngagement,correct = FALSE)
chisq_1


# Adding control variables for PoliticalEngagement
Engagement<- glm(politicalEngagement~ disc_selfsex +disc_wo + marstat+ educ+ race + ageInDecade
           + religpew+ faminc+ pid7, data = d, family = binomial)
summary(Engagement)
exp(coef(Engagement))
coefplot(Engagement, intercept = FALSE, title = "Coefficient Plot for Political Engagement")

# Likelihood of voting based on EXPERIENCED discrimination
m9.3<-glm(new~disc_selfsex, data = d, family = binomial)
summary(m9.3)
exp(coef(m9.3))
m9.3effect<- effect_plot(m9.3, pred = disc_selfsex, interval = TRUE,
                        x.label = "Experienced Discrimination",
                        y.label = "Probability of Voting",
                        )
m9.3effect

# Chi Squared Testing for Chance of Voting and Experienced Discrimination
# Do Chi-Square test on the two variables
chisq_2 <- chisq.test(d$disc_selfsex,d$new,correct = FALSE)
chisq_2

# Adding control variables for chance of voting
m9.4<- glm(new~ disc_selfsex +disc_wo + marstat+ educ+ race + ageInDecade
           + religpew+ faminc+ pid7, data = d, family = binomial)
summary(m9.4)
exp(coef(m9.4))
coefplot(m9.4, intercept = FALSE,
         title = "Coefficient Plot for Voting Probabilty")


# Analysis 3
# Checks on voting likelihood based on perceived/experienced discrimination
# Likelihood of voting based on PERCEIVED discrimination
m10.1<-glm(new~ disc_wo, data = d, family = binomial)
summary(m10.1)
exp(coef(m10.1))
m10.1effect<- effect_plot(m10.1, pred = disc_wo, interval = TRUE,
                          x.label = "Perceived Discrimination",
                          y.label = "Probability of Voting")
m10.1effect
# Do Chi-Square test on the two variables
chisq_3 <- chisq.test(d$disc_wo,d$new,correct = FALSE)
chisq_3

# Do Chi-Square test on the two variables
chisq_4 <- chisq.test(d$disc_wo,d$politicalEngagement,correct = FALSE)
chisq_4


# Model for New with the control variables included
LikelihoodOfVoting<- glm(new~disc_selfsex + disc_wo + marstat+educ+race+ageInDecade
         + religpew+ faminc+ pid7, data = d, family = binomial)
summary(LikelihoodOfVoting)
exp(coef(LikelihoodOfVoting))
coefplot(LikelihoodOfVoting, intercept = FALSE)

# Model for Political Engagement based on PERCEIVED discrimination
m10.3<- glm(politicalEngagement~ disc_wo, data= d, family = binomial)
summary(m10.3)
exp(coef(m10.3))
m10.3effect<- effect_plot(m10.3, pred = disc_wo, interval = TRUE,
                          x.label = "Perceived Discrimination",
                          y.label = "Political Engagement")
m10.3effect


multiplot(Engagement,LikelihoodOfVoting,
          title = "Comparison of Models"
          )

