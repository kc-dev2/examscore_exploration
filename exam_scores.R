# install.packages("tidyverse")
# install.packages("glmnet")
library(tidyverse)
library(glmnet)
data <- read_csv("data/exams.csv")
head(data)
summary(data) # we see that there seem to be no missing variables


#rename variables for readability throughout code
data <- data %>% rename(math = `math score`, reading = `reading score`, writing = `writing score`, race = `race/ethnicity`, par_ed = `parental level of education`, prep = `test preparation course`)

#vector of subject names to make code more readable
subjects <- c('math', 'reading', 'writing')

#find types for all categorical variables to make sure no misspelled columns or accidental duplicates
lapply(data[, !names(data) %in% subjects], unique)

#change column types to factors
data[,!names(data) %in% subjects] <- lapply(data[, !names(data) %in% subjects], as.factor)


##I believe test preparation will provide the greatest impact on student scores
#First, I will find avg score across all subjects for each student and create new column
`avg` <- round(rowSums(data[, subjects])/3, 2)
data <- cbind(data, `avg`)

#Then, I will create histogram plots for the density of counts for prep takers and non takers (use density because sample sizes are not the same)
ggplot(data = data, aes(x=avg, fill=prep)) + geom_histogram(alpha = 0.3, aes(y=..density..), position = 'identity')

#Finally, I will run a two sample t-test to see if the means between test prep course takers and non takers are significantly different
#start off by change values of prep to 0s and 1s
data <- data %>% mutate(prep = recode(prep, 'none' = 0, 'completed' = 1))

#separate prep takers from non prep takers
prep_scores <- data %>% filter(prep==1)
nonprep_scores <- data %>% filter(prep==0)

#run t-test
t.test(nonprep_scores$avg, prep_scores$avg)
# could also have run code as t.test(avg ~ prep, data=data)
#low p-value suggests that those who take prep course score significantly different than those who do not
#can I say that those who took the prep course scored 8 pts better on average than those who did not?

## Let's also run an ANOVA to see if there are significant differences in how different races/ethnicities performed
#First, let's plot mean scores by group
ggplot(data) + aes(x=race, y=avg, color=race) + geom_jitter()
#from plot it looks like all groups have similar means and variance, so let's run ANOVA just for confirmation
group_aov <- aov(avg ~ race, data=data)
summary(group_aov)
#significant p-value means we can reject null hypothesis that all groups performed equally well (?) idk but the plot looks like they all performed relatively the same.. I guess group E is slightly better than other groups and group A is slightly worse than other groups??

#maybe I can check if group E and group A performed significantly different
E_scores <- data %>% filter(race=='group E')
A_scores <- data %>% filter(race=='group A')
t.test(E_scores$avg, A_scores$avg)
#small p-value suggests that they did indeed perform differently

#sanity check of t-test b/w groups B and C, which look almost exactly alike
B_scores <- data %>% filter(race=='group B')
C_scores <- data %>% filter(race=='group C')
t.test(B_scores$avg, C_scores$avg)
#p-val is large, so maybe tests are accurate

#it seems like Tukey test can be used to find groups that are significantly different from each other. this eliminates the need to do t tests one at a time (?)
TukeyHSD(group_aov)
#these pairs have significant difference in scores: (A,E), (B,E), (C,E), (B,D)
# we can find significant differences using code:
# cols <- dim(TukeyHSD(group_aov)$race)[2]
# sig_groups <- TukeyHSD(group_aov)$race[,cols] < 0.05

# t-test on gender
#first, plot data like we did for race
ggplot(data) + aes(x=gender, y=avg, color=gender) + geom_jitter()
#from observation looks like females performed better on average than males
t.test(avg ~ gender, data=data)
# low p-value suggests significant difference. We can interpret that females scored almost 5 points better on average than males. 



### This is sketch. Not sure if this is appropriate analysis.
## Now that we've done some fun, preliminary testing, let's see if we can create a model that can estimate how well a student will perform on a test. We will perform feature selection using regularization and prediction of test scores.
#first, let's reorganize our data such that X contains only predictors and we have 4 Y output vectors (reading, math, writing, avg)
X <- data[,1:5]
X$prep <- as.factor(X$prep) # set prep column as factor
X <- X %>% as.matrix() # set as matrix

# since we need to recode categorical variables as dummy ones, we use model.matrix()
real_X <- data[,!names(data) %in% subjects]
real_X <- model.matrix(avg ~ ., real_X)


y_math <- data$math; y_math <- y_math %>% scale(center = TRUE, scale = FALSE)
y_reading <- data$reading; y_reading <- y_reading %>% scale(center = TRUE, scale = FALSE)
y_writing <- data$writing; y_writing <- y_writing %>% scale(center = TRUE, scale = FALSE)
y_avg <- data$avg; y_avg <- y_avg %>% scale(center = TRUE, scale = FALSE)

#create range of lambdas to try
lambdas <- 10^seq(-3, 5, length.out=100)

#let us try to create a model that predicts math scores
#we first try lasso regression by setting alpha = 1
y_math <- y_math %>% as.matrix()
lasso_cv <- cv.glmnet(real_X, y_math, alpha = 1, lambda = lambdas, standardize=TRUE, nfolds = 10)

predict(lasso_cv, lambda = lasso_cv$lambda.min)
# i am now officially confused
