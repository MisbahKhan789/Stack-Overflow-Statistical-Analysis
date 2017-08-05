#######################################################################################################################################
#This script is designed to answer the following questions:                                                                           
#RQ1: What is the distribution for number of answers and views per question,                                                          
#number of comments per answer and question, score and reputation per question, answer                                            
#and comment, and the answerer’s reputation per accepted and non-accepted answer? 

#RQ2: Do questions and answers have similar distribution of score and number of comments? 

#RQ3: Is there a relationship between a question’s score and the number of its answers? Is there a relationship between
#a question’s score and the number of comments its receives? 

#RQ4: Does the reputation of users who ask questions, answer questions, 
#or leave comments tend to be significantly different? 

#RQ5: Compare and discuss the distributions of answers that were accepted and those that were not?

#RQ6: Is user reputation and score strongly correlated with an answer being accepted
#######################################################################################################################################

#STEP 1- LOADING DATA
## Replace "/PATH/"
questions <- read.csv("/PATH/questions.csv")
answers <- read.csv("/PATH/answers.csv")
comments <- read.csv("/PATH/comments.csv")

## Sys.getenv("USERNAME") 
## On Mac this element in Sys.getenv() is either USER or LOGNAME

#STEP 2- EDITING COLUMN HEADERS
# str(questions)
names(questions) <- c("answer_count", "comment_count", "score", "view_count", "tags", "owner_user_id", "askers_reputation")
# View(head(questions, n=10))
names(answers) <- c("comment_count", "score", "view_count", "tags", "user_reputation", "acceptance")
# View(head(answers, n=10))
names(comments) <- c("score", "user_reputation", "tags", "post_type_id")
# View(head(comments, n=10))

#STEP 3- STATISTICAL ANALYSIS
# required (ggplot2)

library(ggplot2)

questions1= subset(questions,!is.na(answer_count))
questions2= subset(questions1,!is.na(view_count))
questions3= subset(questions,!is.na(comment_count))

#--------- Start Plot# 01 ----------#

# 1. Plot and discuss the distribution of answers and views per question
#To estimate the distribution shape for view-count and answer_count in questions 
#library (rriskDistributions)
#library (tcltk)
#fit.cont(questions2$view_count)

library(MASS)

distributions = c("normal","exponential","binomial","negative binomial","gamma","t","lognormal")
distributions = c("normal","exponential","negative binomial","gamma","t","lognormal")

for ( dist in distributions ) {
  print( paste( "fitting parameters for ", dist ) )
  params = fitdistr( questions2$view_count, dist )
  print( params )
  print( summary( params ) )
  print( params$loglik )
  y<-params$loglik
}
# required (MASS)
library(MASS)
#-------view_count Distribution---------#
# Testing different type of distribution for view_count in questions, then compare the largest value for loglike
fits <- list(
  no = fitdistr(questions2$view_count,"normal"),
  lo = fitdistr(questions2$view_count,"logistic"),
  ca = fitdistr(questions2$view_count,"cauchy"),
  we = fitdistr(questions2$view_count, "weibull"),
  logn= fitdistr(questions2$view_count, 'lognormal'),
  expo= fitdistr(questions2$view_count, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

library(devtools)
library (rriskDistributions)
res <- fit.cont(questions2$view_count)

#-------answer_count Distribution---------#
# same thing we have to find the best fit distripution for answer_count in questions
# Testing different type of distribution for view_count in questions, then compare the largest value for loglike
fits <- list(
  no = fitdistr(questions2$answer_count,"normal"),
  lo = fitdistr(questions2$answer_count,"logistic"),
  ca = fitdistr(questions2$answer_count,"cauchy"),
  we = fitdistr(questions2$answer_count, "weibull"),
  logn= fitdistr(questions2$answer_count, 'lognormal'),
  expo= fitdistr(questions2$answer_count, 'exponential')
)

# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# WE had to make sure from our hypotheseis around view_count and answer_count distributions

#Some tools for checking the validity of the assumption of normality in R
# required (moments)
library(moments)
# required (nortest)
library(nortest)
# required (e1071)
library(e1071)

#-----------perform 3 tests on view_count and then check the Q-Q ----------# 
# skewness and kurtosis for view_count
skewness(questions2$view_count)
kurtosis(questions2$view_count)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(questions2$view_count)

# Kolmogorov-Smirnov test for view_count
ks.test(questions2$view_count,"pnorm",mean(questions2$view_count),sqrt(var(questions2$view_count)))

# Anderson-Darling test 
ad.test(questions2$view_count)

# qq-plot: you should observe a good fit of the straight line
qqnorm(questions2$view_count)
qqline(questions2$view_count)

# drawing the histogram with curve for view_count
hist(questions2$view_count,freq=FALSE)
lines(density(questions2$view_count))
#distribution of view count 
plot(questions2$view_count, main = "view counts in Questions")

#-----------perform 3 tests on answer_count and then check the Q-Q ----------# 
# skewness and kurtosis for view_count
skewness(questions2$answer_count)
kurtosis(questions2$answer_count)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(questions2$view_count)

# Kolmogorov-Smirnov test for view_count
ks.test(questions2$answer_count,"pnorm",mean(questions2$answer_count),sqrt(var(questions2$answer_count)))

# Anderson-Darling test 
ad.test(questions2$answer_count)

# qq-plot: you should observe a good fit of the straight line
qqnorm(questions2$answer_count)
qqline(questions2$answer_count)

# drawing the histogram with curve for answer_count
hist(questions2$answer_count, freq = FALSE)
lines(density(questions2$answer_count), na.rm= TRUE)
# distribution of answer count
plot(questions2$answer_count, main = "answer counts in Questions")
#------------------#

# density of answer_count and view_count 
plot(density(questions2$answer_count), main = "number of answers in questions")
plot(density(questions2$view_count), main = "number of views in questions")

#-------End----of--------plot01--------#

#---------- Start Plot # 2 ----------#
# 2. plot the distribution of number of comments per question and answer
#To estimate the distribution shape for view-count and answer_count in questions 

# distribution of comments per questions (blue circles) and comments per answers (orange circle)
mysample<-sample(questions$comment_count, 1000, replace = FALSE)
mysample2<-sample(answers$comment_count, 1000, replace = FALSE)
plot(mysample, col="blue", ylim=c(0,100))
points(mysample2, col = "orange")

# required (MASS)
library(MASS)
#-------question comment_count Distribution---------#
# Testing different type of distribution for comment_count in questions, then compare the largest value for loglike
fits <- list(
  no = fitdistr(questions3$comment_count,"normal"),
  lo = fitdistr(questions3$comment_count,"logistic"),
  ca = fitdistr(questions3$comment_count,"cauchy"),
  expo= fitdistr(questions3$comment_count, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#-------comment_count per answers Distribution---------#
# same thing we have to find the best fit distripution for answer_count in questions
# Testing different type of distribution for view_count in questions, then compare the largest value for loglike
answers1= subset(answers,!is.na(comment_count))

fits <- list(
  no = fitdistr(answers1$comment_count,"normal"),
  lo = fitdistr(answers1$comment_count,"logistic"),
  ca = fitdistr(answers1$comment_count,"cauchy"),
  expo= fitdistr(answers1$comment_count, 'exponential')
)

# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# WE had to make sure from our hypotheseis around comment_count per answers and questions distributions

#Some tools for checking the validity of the assumption of normality in R
# required (moments)
library(moments)
# required (nortest)
library(nortest)
# required (e1071)
library(e1071)

# find the skewness 
skewness(questions3$comment_count)
skewness(answers1$comment_count)

# find the kurtosis
kurtosis(questions3$comment_count)
kurtosis(answers1$comment_count)

#Q-Q plot for question comment_count 
qqnorm(questions$comment_count)
qqline(questions$comment_count, col="red")

#Q-Q plot for answers comment_count 
qqnorm(answers$comment_count)
qqline(answers$comment_count, col="red")

# drawing the histogram with curve for questions comment_count
hist(questions$comment_count,freq=FALSE)
lines(density(questions3$comment_count))

# drawing the histogram with curve for answers comment_count
hist(answers$comment_count,freq=FALSE)
lines(density(answers1$comment_count))

# Anderson-Darling normality test for comment_count per questions and answers
ad.test(questions$comment_count)
ad.test(answers$comment_count)
# -------- End of Plot #02 -------# 

#-------- Start Plot# 3 ---------#
# 3. the reputation of the owner and the score for questions, comments, and answers; 
#####FIRST:QUESTIONS######
questions4=subset(questions,!is.na(askers_reputation))
questions5=subset(questions,!is.na(score))

#distribution of reputation for questions
plot(questions$askers_reputation)
#distribution of score for questions
plot(questions$score)

#find the distribution of reputation for questions
fits <- list(
  no = fitdistr(questions4$askers_reputation,"normal"),
  lo = fitdistr(questions4$askers_reputation,"logistic"),
  ca = fitdistr(questions4$askers_reputation,"cauchy"),
  we = fitdistr(questions4$askers_reputation, "weibull"),
  logn= fitdistr(questions4$askers_reputation, 'lognormal'),
  expo= fitdistr(questions4$askers_reputation, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#find the distribution of score for questions
# I remove lognormal, weibull, and exponential cause I have negative values
library(MASS)
fits <- list(
  no = fitdistr(questions5$score,"normal"),
  lo = fitdistr(questions5$score,"logistic"),
  ca = fitdistr(questions5$score,"cauchy")
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# find the skewness for questions score and reputation
skewness(questions4$askers_reputation)
skewness(questions5$score)

#Q-Q plot for askers_reputation
qqnorm(questions$askers_reputation)
qqline(questions$askers_reputation, col="red")

#The Q-Q plot for questions score 
qqnorm(questions$score)
qqline(questions$score,col="red")

# find the kurtosis for questions score and reputation
kurtosis(questions4$askers_reputation)
kurtosis(questions5$score)

# drawing the histogram with curve for questions askers_reputation
hist(questions4$askers_reputation,freq=FALSE)
lines(density(questions4$askers_reputation))

# drawing the histogram with curve for questions score
hist(questions5$score,freq=FALSE)
lines(density(questions5$score))


#####SECOND:ANSWERS######
answers1=subset(answers,!is.na(user_reputation))
answers2=subset(answers,!is.na(score))

#distribution of reputation for answers
plot(answers1$user_reputation)
#distribution of score for questions
plot(answers2$score)
#distribution of reputation and score for answers
mysample<-sample(answers1$user_reputation, 1000, replace = FALSE)
mysample2<-sample(answers2$score, 1000, replace = FALSE)
plot(mysample, col="blue")
points(mysample2, col = "orange")

#find the distribution of reputation for answers
fits <- list(
  no = fitdistr(answers1$user_reputation,"normal"),
  lo = fitdistr(answers1$user_reputation,"logistic"),
  ca = fitdistr(answers1$user_reputation,"cauchy"),
  we = fitdistr(answers1$user_reputation, "weibull"),
  logn= fitdistr(answers1$user_reputation, 'lognormal'),
  expo= fitdistr(answers1$user_reputation, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#find the distribution of score for answers
# I remove  weibull, lognormal, exponential cause I have negative values
fits <- list(
  no = fitdistr(answers2$score,"normal"),
  lo = fitdistr(answers2$score,"logistic"),
  ca = fitdistr(answers2$score,"cauchy")
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# find the skewness for answers score and reputation
skewness(answers1$user_reputation)
skewness(answers2$score)

#Q-Q plot for answer user_reputation
qqnorm(answers1$user_reputation)
qqline(answers1$user_reputation, col="red") 


#The Q-Q plot for answer score 
qqnorm(answers2$score)
qqline(answers2$score,col="red") 

# find the kurtosis for answers score and reputation
kurtosis(answers1$user_reputation)
kurtosis(answers2$score)

# drawing the histogram with curve for answers user_reputation
hist(answers1$user_reputation,freq=FALSE)
lines(density(answers1$user_reputation))

# drawing the histogram with curve for answers score
hist(answers2$score,freq=FALSE)
lines(density(answers2$score))


#####THIRD:COMMENTS######
comments1=subset(answers,!is.na(user_reputation))
comments2=subset(answers,!is.na(score))

#distribution of reputation for comments
plot(comments1$user_reputation)
#distribution of score for comments
plot(comments2$score)
#distribution of reputation and score for comments
mysample<-sample(comments1$user_reputation, 1000, replace = FALSE)
mysample2<-sample(comments2$score, 1000, replace = FALSE)
plot(mysample, col="blue")
points(mysample2, col = "orange")


#find the distribution of reputation for comments
fits <- list(
  no = fitdistr(comments1$user_reputation,"normal"),
  lo = fitdistr(comments1$user_reputation,"logistic"),
  ca = fitdistr(comments1$user_reputation,"cauchy"),
  we = fitdistr(comments1$user_reputation, "weibull"),
  logn= fitdistr(comments1$user_reputation, 'lognormal'),
  expo= fitdistr(comments1$user_reputation, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#find the distribution of score for comments
# I remove  weibull, lognormal, exponential cause I have negative values
fits <- list(
  no = fitdistr(comments2$score,"normal"),
  lo = fitdistr(comments2$score,"logistic"),
  ca = fitdistr(comments2$score,"cauchy")
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# find the skewness for comments score and reputation
skewness(comments1$user_reputation)
skewness(comments2$score)

#Q-Q plot for comments user_reputation
qqnorm(comments1$user_reputation)
qqline(comments1$user_reputation, col="red") 


#The Q-Q plot for comments score 
qqnorm(comments2$score)
qqline(comments2$score,col="red") 

# find the kurtosis for comments score and reputation
kurtosis(comments1$user_reputation)
kurtosis(comments2$score)

# drawing the histogram with curve for comments user_reputation
hist(comments1$user_reputation,freq=FALSE)
lines(density(comments1$user_reputation))

# drawing the histogram with curve for comments score
hist(comments2$score,freq=FALSE)
lines(density(comments2$score)) 

#---------- End Plot # 3------------#

#----------Start Plot# 04 ------------#
# 4. the reputation of the owner and the score for accepted and unaccepted answers separately.
# 4.1 the reputation of the owner and the score for accepted answers
answersY=  subset(answers,answers$acceptance=="YES")
answersY1= subset(answersY,!is.na(user_reputation))
answersY2= subset(answersY1,!is.na(score))

# plot distribution for both the user reoutation and the score for accepted answers
plot (answersY2$user_reputation, main = "users reputation distribution in accepted answers")
plot (answersY2$score, main = "score distribution in accepted answers")

#To estimate the distribution shape for user reputation and score in accepted answers 

# required (MASS)
library(MASS)
#-------accepted answers - user_reoutation Distribution---------#
# Testing different type of distribution for user_reputation in accepted answers, then compare the largest value for loglike
fits <- list(
  no = fitdistr(answersY2$user_reputation,"normal"),
  lo = fitdistr(answersY2$user_reputation,"logistic"),
  ca = fitdistr(answersY2$user_reputation,"cauchy"),
  we = fitdistr(answersY2$user_reputation, "weibull"),
  logn= fitdistr(answersY2$user_reputation, 'lognormal'),
  expo= fitdistr(answersY2$user_reputation, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#-------accepted answers score Distribution---------#
# same thing we have to find the best fit distripution for score in questions
# Testing different type of distribution for score in accepted answers, then compare the largest value for loglike
fits <- list(
  no = fitdistr(answersY2$score,"normal"),
  lo = fitdistr(answersY2$score,"logistic"),
  ca = fitdistr(answersY2$score,"cauchy"),
  we = fitdistr(answersY2$score, "weibull"),
  logn= fitdistr(answersY2$score, 'lognormal'),
  expo= fitdistr(answersY2$score, 'exponential')
)

# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# We had to make sure from our hypotheseis around user_reputation and score distributions

#Some tools for checking the validity of the assumption of normality in R
# required (moments)
library(moments)
# required (nortest)
library(nortest)
# required (e1071)
library(e1071)

#-----------perform 3 tests on user_repuation and then check the Q-Q ----------# 
# skewness and kurtosis for user_reputation
skewness(answersY2$user_reputation)
kurtosis(answersY2$user_reputation)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(questions2$view_count)

# Kolmogorov-Smirnov test for user_repuation
# ks.test(answersY2$user_reputation,"pnorm",mean(answersY2$user_reputation),sqrt(var(answersY2$user_reputation)))

# Anderson-Darling test 
ad.test(answersY2$user_reputation)

# qq-plot: you should observe a good fit of the straight line
qqnorm(answersY2$user_reputation)
qqline(answersY2$user_reputation, col="red")

# the log of user_reputation
log.reputation <- log(answersY2$user_reputation)
qqnorm(log.reputation)
qqline(log.reputation, col="red")

# drawing the histogram with curve for user_repuation
hist(answersY2$user_reputation,freq=FALSE)
lines(density(answersY2$user_reputation))

#-----------perform 3 tests on score and then check the Q-Q ----------# 
# skewness and kurtosis for score
skewness(answersY2$score)
kurtosis(answersY2$score)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(answersY2$score)

# Kolmogorov-Smirnov test for score
# ks.test(answersY2$score,"pnorm",mean(answersY2$score),sqrt(var(answersY2$score)))

# Anderson-Darling test 
ad.test(answersY2$score)

# qq-plot: you should observe a good fit of the straight line
qqnorm(answersY2$score)
qqline(answersY2$score, col="red")

#log.score <- log(answersY2$score)
#qqnorm(log.score)
#qqline(log.score, col="red")

# drawing the histogram with curve for score
hist(answersY2$score,freq=FALSE)
lines(density(answersY2$score))
#-------- End of plot # 4.1 ---------#

#------- Start plot # 4.2 -----------#
# 4.2 plot distribution for user_reputation and score in non-accepted answers
answersN= subset(answers,answers$acceptance=="NO")
answersN1= subset(answersN,!is.na(user_reputation))
answersN2= subset(answersN1,!is.na(score))

# plot the distribution for users reputation in non-accepted answers
plot (answersN2$user_reputation, main = "users reputation for non-accepted answers")
plot (answersN2$score, main = "score for non-accepted answers")

#To estimate the distribution shape for user reputation and score in non-accepted answers 

# required (MASS)
library(MASS)
#-------non-accepted answers - user_reoutation Distribution---------#
# Testing different type of distribution for user_reputation in non-accepted answers, then compare the largest value for loglike
fits <- list(
  no = fitdistr(answersN2$user_reputation,"normal"),
  lo = fitdistr(answersN2$user_reputation,"logistic"),
  ca = fitdistr(answersN2$user_reputation,"cauchy"),
  we = fitdistr(answersN2$user_reputation, "weibull"),
  logn= fitdistr(answersN2$user_reputation, 'lognormal'),
  expo= fitdistr(answersN2$user_reputation, 'exponential')
)
# get the logliks for each model...
sapply(fits, function(i) i$loglik)

#-------non-accepted answers score Distribution---------#
# same thing we have to find the best fit distripution for score in non-accepted answers
# Testing different type of distribution for score in non-accepted answers, then compare the largest value for loglike
fits <- list(
  no = fitdistr(answersN2$score,"normal"),
  lo = fitdistr(answersN2$score,"logistic"),
  ca = fitdistr(answersN2$score,"cauchy"),
  we = fitdistr(answersN2$score, "weibull"),
  logn= fitdistr(answersN2$score, 'lognormal'),
  expo= fitdistr(answersN2$score, 'exponential')
)

# get the logliks for each model...
sapply(fits, function(i) i$loglik)

# We had to make sure from our hypotheseis around user_reputation and score distributions

#Some tools for checking the validity of the assumption of normality in R
# required (moments)
library(moments)
# required (nortest)
library(nortest)
# required (e1071)
library(e1071)

#-----------perform 3 tests on user_repuation and then check the Q-Q ----------# 
# skewness and kurtosis for user_reputation
skewness(answersN2$user_reputation)
kurtosis(answersN2$user_reputation)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(questions2$view_count)

# Kolmogorov-Smirnov test for user_repuation
# ks.test(answersN2$user_reputation,"pnorm",mean(answersN2$user_reputation),sqrt(var(answersN2$user_reputation)))

# Anderson-Darling test 
ad.test(answersN2$user_reputation)

# qq-plot: you should observe a good fit of the straight line
qqnorm(answersN2$user_reputation)
qqline(answersN2$user_reputation, col="red")

# the log of user_reputation
log.reputation <- log(answersN2$user_reputation)
qqnorm(log.reputation)
qqline(log.reputation, col="red")

# drawing the histogram with curve for user_repuation
hist(answersN2$user_reputation,freq=FALSE)
lines(density(answersN2$user_reputation))

#-----------perform 3 tests on score and then check the Q-Q ----------# 
# skewness and kurtosis for score
skewness(answersN2$score)
kurtosis(answersN2$score)

# Shapiro-Wilks test --> Error: need the values to be between  3-5000
# shapiro.test(answersN2$score)

# Kolmogorov-Smirnov test for score
# ks.test(answersY2$score,"pnorm",mean(answersY2$score),sqrt(var(answersY2$score)))

# Anderson-Darling test 
ad.test(answersN2$score)

# qq-plot: you should observe a good fit of the straight line
qqnorm(answersN2$score)
qqline(answersN2$score, col="red")

#log.score <- log(answersN2$score)
#qqnorm(log.score)
#qqline(log.score, col="red")

# drawing the histogram with curve for score
hist(answersN2$score,freq=FALSE)
lines(density(answersN2$score))

#--------- End of Plot# 04 --------#

#--------- Start Plot# 05 ---------#

# 5. Do questions and answers have similar distributions of scores 

questions5= subset(questions,!is.na(score))
answers2= subset(answers,!is.na(score))

# perform Kolmogorov-Smirnov Test to check 2 hypothesis => H1= they are same; H2= they are not
ks.test(questions5$score, answers2$score)

# p>.05 significance level; but here we have p = 2.2e-16 
# we conclude that the score data of questions and score data in answers are nonidentical populations
# null hypothesis 
wilcox.test (questions5$score, answers2$score)

 
# questions$score < answers$score
#wilcox.test(questions5$score, answers2$score, alternative="l", mu=0, exact=FALSE, paired=FALSE)   
# questions$score > answers$score
#wilcox.test(questions5$score, answers2$score, alternative="g", mu=0, exact=FALSE, paired=FALSE)
# ttest assumes that both data drom normal distribution
#library (graphics)
#library (dgof)
#plot(ecdf(questions5$score), xlim=range(c(questions5$score, answers2$score)), col= 'red')
#plot(ecdf(answers2$score), add=TRUE, lty="dashed")
#ttest= t.test(questions5$score, answers2$score, alternative="g")
#ttest$p.value
#--------- End of plot# 5---------------#

#--------Start Plot# 6 -------------#
# Do questions and answers have similar distributions of number of comments 

questions3= subset(questions,!is.na(comment_count))
answers2= subset(answers,!is.na(comment_count))

# perform Kolmogorov-Smirnov Test to check 2 hypothesis => H0= they are same; H1= they are not
ks.test(questions3$comment_count, answers2$comment_count)

# p>.05 significance level; but here we have p = 2.2e-16 
# we conclude that the comment_count data of questions and comment_count data in answers are nonidentical populations
# null hypothesis 
wilcox.test (questions3$comment_count, answers2$comment_count)

#------- End of Plot# 6-------#
#--------Start plot # 7------#
#7 Is there a relationship between a questions’ score and the number of answers?
questions1= subset(questions,!is.na(answer_count))
questions2= subset(questions1,!is.na(score))

# find the correlation and covariance between answer_count and score
cor(questions2$answer_count,questions2$score)
cov(questions2$answer_count,questions2$score)

# calculate a linear model (linear regression)
res <- lm(questions2$score~questions2$answer_count)
#print regression summary 
summary(res)
#plot the result
plot(questions2$answer_count,questions2$score)
abline(res, col="blue")
#check the linear regression validity
plot(res)

#7 Does the reputation of users who ask questions, answer questions, or leave comments tend to be significantly different? 
questions7 = subset(questions,!is.na(askers_reputation))
answers4 = subset(answers,!is.na(user_reputation))
comments1 = subset(comments,!is.na(user_reputation))

plot(density(questions7$askers_reputation), main = "User reputation in Questions")
plot(density(answers4$user_reputation), main = "User reputation in Answers")
plot(density(comments1$user_reputation), main = "User reputation in Comments")

questions8= (questions7$askers_reputation-mean(questions7$askers_reputation))/sd(questions7$askers_reputation)
answers5 = (answers4$user_reputation-mean(answers4$user_reputation))/sd(answers4$user_reputation)
library(corrgram)
df <- data.frame( cbind(questions8,answers5))
corrgram(df, order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="questions and answers - users reputations")
#comments2= (comments1$user_reputation-mean(comments1$user_reputation))/sd(comments1$user_reputation)
#---------End Plot # 7 ------------#

#----------Start Plot # 8--------#
#Is there a relationship between a questions’ score and comments it receives?

questions1= subset(questions,!is.na(comment_count))
questions2= subset(questions1,!is.na(score))

# find the correlation and covariance between comment_count and score
cor(questions2$comment_count,questions2$score)
cov(questions2$comment_count,questions2$score)


# calculate a linear model (linear regression)
res <- lm(questions2$score~questions2$comment_count)
#print regression summary 
summary(res)
#plot the result
plot(questions2$comment_count,questions2$score)
abline(res, col="blue")
#check the linear regression validity
plot(res)
#---------- End Plot# 8--------#




#----------Start Plot # 9--------#
# Q09: Does the reputation of users who ask questions, answer questions, or leave comments tend to be significantly different?
questions9=subset(questions,!is.na(questions$askers_reputation))
answers9=subset(answers,!is.na(answers$user_reputation))
comments9=subset(comments,!is.na(comments$user_reputation))
str(comments)
# Similarity Test 1: comparing distribution with plots
opar= par()
par(mfrow=c(1,3))
plot(density(questions9$askers_reputation), main="Questioner's Reputation")
plot(density(answers9$user_reputation), main="Answerer's Reputation") 
plot(density(comments9$user_reputation), main="Commenter's Reputation") 
par(opar)

opar= par()
par(mfrow=c(1,3))
plot(density(questions9$askers_reputation), type="l", main="Questioner's Reputation")
plot(density(questions9$askers_reputation), type="l", main="Questioner's & Answerer's Reputation")
lines(density(answers9$user_reputation), col="red")
plot(density(questions9$askers_reputation), type="l", main="Questioner's,Answerer's & Commenter's Reputation")
lines(density(answers9$user_reputation), col="red")
lines(density(comments9$user_reputation), col="blue")
par(opar)

# Similarity Test 2: comparing data with statistical information
summary(questions9$askers_reputation)
summary(answers9$user_reputation)
summary(comments9$user_reputation)

# Similarity Test 3: ks test
ks.test(questions9$askers_reputation,comments9$user_reputation)
ks.test(questions9$askers_reputation,answers9$user_reputation)
ks.test(answers9$user_reputation,comments9$user_reputation)
# Result obtained :- D = 0.3555, p-value < 2.2e-16; alternative hypothesis: two-sided
# Result obtained :- D = 0.44233, p-value < 2.2e-16; alternative hypothesis: two-sided
# Result obtained :- D = 0.087159, p-value < 2.2e-16; alternative hypothesis: two-sided
#---------- End Plot# 9--------#

#----------Start Plot # 10--------#

# Q10: Compare and discuss the distributions of answers that were accepted and those that were not?
accepted_answers= subset(answers, answers$acceptance=="YES")
unaccepted_answers= subset(answers, answers$acceptance=="NO")

# Similarity Test 1: plotting distributions
opar=par()
par(mfrow=c(2,4))
plot(density(na.omit(accepted_answers$comment_count)), main="Accepted Answer's Comment_count")
plot(density(na.omit(accepted_answers$score)), main="Accepted Answer's Score")
plot(density(na.omit(accepted_answers$view_count)), main="Accepted Answer's View_count")
plot(density(na.omit(accepted_answers$user_reputation)), main="Accepted Answer's User_reputation")
plot(density(na.omit(unaccepted_answers$comment_count)), main="Unaccepted Answer's Comment_count")
plot(density(na.omit(unaccepted_answers$score)), main="Unaccepted Answer's Score")
plot(density(na.omit(unaccepted_answers$view_count)), main="Unaccepted Answer's View_count")
plot(density(na.omit(unaccepted_answers$user_reputation)), main="Unaccepted Answer's User_reputation")
par=opar

# Similarity Test 2: statistics
summary(accepted_answers)
summary(unaccepted_answers)
summary(accepted_answers$tags)
summary(unaccepted_answers$tags)

#Similarity Test 3: ks test
ks.test(accepted_answers$comment_count, unaccepted_answers$comment_count)     #D = 0.079158, p-value < 2.2e-16
ks.test(accepted_answers$score, unaccepted_answers$score)                     #D = 0.28239, p-value < 2.2e-16
ks.test(accepted_answers$view_count, unaccepted_answers$view_count)           #D = 7.2647e-15, p-value = 1
ks.test(accepted_answers$user_reputation, unaccepted_answers$user_reputation) #D = 0.10584, p-value < 2.2e-16

#---------- End Plot# 10--------#

#----------Start Plot # 11--------#
# Q11: is user reputation and score strongly correlated with an answer being accepted?
# Method 1:
# To answer this, we need to check if 
# (1) accepted_answers$user_reputation is similar to unaccepted_answers$user_reputation
# (2) accepted_answers$score is similar to unaccepted_answers$score

# Similarity Test 1: plotting: 
opar=par()
par(mfrow=c(2,2))
plot(density(na.omit(accepted_answers$score)), main="Accepted Answer's Score")
plot(density(na.omit(accepted_answers$user_reputation)), main="Accepted Answer's User_reputation")
plot(density(na.omit(unaccepted_answers$score)), main="Unaccepted Answer's Score")
plot(density(na.omit(unaccepted_answers$user_reputation)), main="Unaccepted Answer's User_reputation")
par=opar

# Similarity Test 2: comparing statistics: 
summary(accepted_answers$user_reputation)
summary(unaccepted_answers$user_reputation)
summary(accepted_answers$score)
summary(unaccepted_answers$score)

# Similarity Test 3: ks.test(): 
ks.test(accepted_answers$user_reputation, unaccepted_answers$user_reputation) #D = 0.10584, p-value < 2.2e-16
ks.test(accepted_answers$score, unaccepted_answers$score)                     #D = 0.28239, p-value < 2.2e-16

# Method 2: 
# using corelation function from stats package, i.e. cor.test()
install.packages("stats")
library(stats)
answers$acceptancebool <- ifelse (answers$acceptance=="NO",0,1)
# summary(answers$acceptancebool)
cor.test(answers$score,answers$acceptancebool,method=c("pearson"))
cor.test(answers$user_reputation,answers$acceptancebool,method=c("pearson"))
#---------- End Plot# 11--------#






#
#
#
#
#
# STATISTICAL ANALYSIS- CORRELATION

if(!require(corrgram))  {install.packages("corrgram")}      
library(corrgram)
corrgram(questions)      
corrgram(answers)      
corrgram(comments)      

## Save the Workspace
## Specify the PATH and the Name
save.image(file = "/PATH/NAME.RData")