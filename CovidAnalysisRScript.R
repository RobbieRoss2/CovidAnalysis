install.packages("Hmisc")
library(Hmisc)

data <- read.csv("~/Professional/Portfolio/Project 3 - COVID Analysis/COVID19_line_list_data.csv") # load data
describe(data) # initial check of data, it appears that there are a lot of missing values

data$death_YN <- as.integer(data$death != 0) #gives a yes/no value for every record to avoid missing data

sum(data$death_YN  / nrow(data)) # 0.058 = 5.8% mortality rate

#GENDER ANALYSIS

#H0: There is no effect of gender on the mortality rate

men <- subset(data, gender == "male")
women <- subset(data, gender == "female")
mean(men$death_YN, na.rm = TRUE) # 0.085 = 8.5% mortality rate
mean(women$death_YN, na.rm = TRUE) # 0.037 = 3.7% mortality rate

# this gives the initial idea that men have a higher mortality rate than females (8.5% > 3.7%)
# however, this still has to be tested for statistical significance

t.test(men$death_YN, women$death_YN, alternative = "two.sided", conf.level = 0.95)

#results at 95% CI: men have a higher mortality rate of between (1.7%, 7.8%) compared to women, p = 0.002105

# as our p-value < 0.05 we can reject our null hypothesis and conclude that gender does have a significant impact
# on mortality rate for people with COVID-19 with men having a higher mortality rate then women.

#AGE ANALYSIS

#H0: There is no effect of age on the mortality rate

alive <- subset(data, death_YN == 0)
died <- subset(data, death_YN == 1)
mean(died$age, na.rm = TRUE) # 68.59
mean(alive$age, na.rm = TRUE) # 48.07

# this gives the initial idea that on average those who die are older than those who survive (68.59 > 48.07)
# however, this still has to be tested for statistical significance

t.test(alive$age, died$age, alternative = "two.sided", conf.level = 0.95)

#results at 95% CI: (-24.29, -16.74), p < 2.2e-16

# as our p-value < 0.05 we can reject our null hypothesis and conclude that age does have a significant impact
# on mortality rate for people with COVID-19

#LOCATION ANALYSIS

#H0: There is no effect on mortality rate for those from Wuhan and those who are from elsewhere.

Wuhan <- subset(data, from.Wuhan == 1)
Not_Wuhan <- subset(data, from.Wuhan == 0)
mean(Wuhan$death_YN, na.rm = TRUE) # 0.22 = 22%
mean(Not_Wuhan$death_YN, na.rm = TRUE) # 0.031 = 3.1%

# this gives the initial idea that on average, the mortality rate is higher for people from Wuhan (22% > 3.1%)
# however, this still has to be tested for statistical significance

t.test(Wuhan$death_YN, Not_Wuhan$death_YN, alternative = "two.sided", conf.level = 0.95)

#results at 95% CI: (0.1201495, 0.2530453) = (12.01%, 25.30%), p = 1.152e-07

# as our p-value < 0.05 we can reject our null hypothesis and conclude that people from Wuhan have between 12.01% and
# 25.30% higher mortality rate then those not from Wuhan for people with COVID-19
