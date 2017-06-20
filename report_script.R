# package declarations
library("leaps")
library("lmtest")
library("sandwich")
library("ggplot2")
library("stringr")
library("plyr")
# ggplot2 theme
theme_set(theme_bw())

# load dataset
project_data <- readRDS("project_data.RDS")

# Part A
# run the regression of wait time on age, type of ailment, doctor and acuity
reg_a <- lm(wait_time ~ count + age + md + chief_complaint + acuity, 
            data = project_data)
# we do not show the summary output of the model here because there are too
#   many indicator variables
reg_a_coef_matrix <- summary(reg_a)$coefficients
# look at the estimate for count
count_a_report <- reg_a_coef_matrix["count", ]
count_a_report
# have a sense of the distribution of the waiting time
fivenum(project_data$wait_time)

# Part b
# we need to have a sense of how the typical waiting time is distributed

# we should get rid of the indicator variables for complaints
# the reason is that only a very small proportion of these indicator variables
# are statistically significant
complaint_index <- str_detect(rownames(reg_a_coef_matrix), 
                              pattern = "chief_complaint")
chief_complaint_p <- reg_a_coef_matrix[complaint_index, 4]
chief_com_significant <- which(chief_complaint_p <= 0.2)
length(chief_com_significant) / length(complaint_index)
# optional robust estimation
# robust_vcov <- vcovHC(reg_a, type = "HC0")

# get rid of the complaint indicator variables
# find a best model with regsubsets from the leaps package
model_find <- regsubsets(wait_time ~ count + age + md + acuity, 
                         data = project_data, nvmax = 30, 
                         method = "exhaustive")
# use Baysian Information Criterion to find the optional number of predictors
# the best level is when the BIC is minimised
qplot(x = 1:17, y = summary(model_find)$bic, geom = "line") + 
  scale_x_continuous(breaks = 1:17) + 
  labs(x = "Number of Predictors", y = "BIC", 
       title = "Bayesian Information Criterion Over Number of Predictors")
# model says that we should choose four variables
# take a look at the model search
summary(model_find)
# the model suggests we include an indicator variable for a single doctor
# this is not useful for our study for the emergency room as a whole
# so we get rid of the doctor variable as well
model_find_2 <- regsubsets(wait_time ~ count + age + acuity, 
                           data = project_data, nvmax = 30, 
                           method = "exhaustive")
# plot BIC
qplot(x = 1:6, y = summary(model_find_2)$bic, geom = "line") + 
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Number of Predictors", y = "BIC", 
       title = "Bayesian Information Criterion Over Number of Predictors")
# algorithm recommends a 3 variable model
# take a look at the search algorithm
summary(model_find_2)
# software recommends use count, age and acuity level very urgent
# to run this model, create an indicator variable for the very urgent
# run the selected model
reg_b_search <- lm(wait_time ~ count + age + very_urgent, data = project_data)
summary(reg_b_search)
# run a Ramsey reset test to see whether we have omitted variable bias
resettest(reg_b_search)
# we have another two guesses for additional variables
# first, more older one gets, we should expect another extra year of age
# should further shortens the waiting time
# The other guess is that it there should be an interaction term between age
# the same guess applies to count
#   and acuity
reg_b_quadratic <- lm(wait_time ~ count + I(count^2) + age + I(age^2), 
                      data = project_data)
summary(reg_b_quadratic)
# we perform a chow test
chow_model <- update(reg_b_quadratic, formula. = . ~ . * very_urgent)
chow_anova <- anova(chow_model)
chow_anova
chow <- lm(wait_time ~ (age), project_data)
chow_test_stat <- (sum(chow_anova[5:9, 2]) / 5) / chow_anova[10, 3]
# the chow_test_stat is quite large, so we should add the indicator term
# re run the algorithm to find our model
model_find_3 <- regsubsets(
  wait_time ~ (count + I(count^2) + age + I(age^2)) * very_urgent, 
  data = project_data, nvmax = 20, 
  method = "exhaustive"
)
# bic over # of predictors
qplot(x = 1:9, y = summary(model_find_3)$bic, geom = "line") + 
  scale_x_continuous(breaks = 1:9) + 
  labs(x = "Number of Predictors", y = "BIC", 
       title = "Bayesian Information Criterion Over Number of Predictors")
# take a look
summary(model_find_3)
# model from third search
reg_b_3 <- lm(wait_time ~ count : very_urgent + I(age^2) + I(count^2), 
              project_data)
summary(reg_b_3)

# add md
reg_b_final <- update(reg_b_3, formula. = . ~ . + md)
