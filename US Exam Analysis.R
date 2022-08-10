library(tidyverse)

exams = exams %>% mutate(total_score = math.score + reading.score + writing.score)
exams = exams %>% mutate(percentage_score = (total_score/300) * 100)


exams %>% summary()

# check for missing values 
sum(is.na(exams))


# EXPLORING THE DATA

exams = exams %>% mutate(z_total_score = (total_score - mean(total_score))/sd(total_score))
# there are a few z scores that may be considered unusual, particularly on the lower end
# taking a look at those 50 observations
exams %>% filter(z_total_score > 2 | z_total_score < -2) %>% arrange(z_total_score)
50/1000 * 100
# 5 percent of our data is outside of two standard deviations from the mean

# Let's check if it's a normal distribution graph
ggplot(exams) + geom_histogram(aes(x=total_score))
ggplot(exams) + geom_histogram(aes(x=math.score))
ggplot(exams) + geom_histogram(aes(x=writing.score))
ggplot(exams) + geom_histogram(aes(x=reading.score))
# all of these graphs appear to have a slight skew to the left, but basically normal

ggplot(exams) + geom_point(aes(x=math.score, y=writing.score))
ggplot(exams) + geom_point(aes(x=math.score, y=reading.score))
ggplot(exams) + geom_point(aes(x=reading.score, y=writing.score))
#reading and writing score appear to be correlated 
cor(exams$writing.score, exams$reading.score)
# correlation is .95, which is close to a perfect positive relationship


# looking at gender and exam scores
exams %>% group_by(gender) %>% summarize(count = n())
ggplot(exams) + geom_boxplot(aes(x=percentage_score, y=gender))
# females score a little better than males one average

# looking at parental education and exam scores
exams %>% group_by(parental.level.of.education) %>% summarize(count = n())
ggplot(exams) + geom_boxplot(aes(x=percentage_score, y=parental.level.of.education))
# students with parents that have masters degrees score higher on average
# it interesting to note that students that have parents with some high school 
# scored better than students with parents with an associates degree


# looking at lunch and exam scores
exams %>% group_by(lunch) %>% summarize(count = n())
ggplot(exams) + geom_boxplot(aes(x=percentage_score, y=lunch))
# standard lunch students tend to score higher

# looking at race and exam scores
exams %>% group_by(race.ethnicity) %>% summarize(count = n())
ggplot(exams) + geom_boxplot(aes(x=percentage_score, y=race.ethnicity))
# it appears that group E scored the highest 


# how many people failed the three tests (using 60 as an F as most colleges do)
exams %>% filter(math.score < 60) %>% summarize(count =n())
# 292 students failed math
exams %>% filter(reading.score < 60) %>% summarize(count =n())
# 249 students failed reading
exams %>% filter(writing.score < 60) %>% summarize(count =n())
# 265 students failed writing


# Does the test preparation course help?
# Looking at how many took the course
exams %>% group_by(test.preparation.course) %>% summarize(count = n())
ggplot(exams) + geom_boxplot(aes(x= test.preparation.course, y=math.score))
ggplot(exams) + geom_boxplot(aes(x= test.preparation.course, y=reading.score))
ggplot(exams) + geom_boxplot(aes(x= test.preparation.course, y=writing.score))
# Test Preparation course increases students average score

# LINEAR REGRESSION
model = lm(total_score ~ gender + race.ethnicity + parental.level.of.education 
           + lunch + test.preparation.course, data = exams)
summary(model)
# the model accounts for 27 percent of the variablity in total score for a student

library(car)
vif(model)
# vif is less than 5, so multicolinearity is likely not present

library(effectsize)
eta_squared(model, partial=FALSE)
#lunch appears to account for the most variability based on the current model

