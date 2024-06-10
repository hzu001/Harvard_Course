

######### Machine Learning ##########

###### Prerequisite test #######

data(heights)
heights
class(heights$height)
class(heights$sex)
class(heights)
b <- "Male"
class(b)
c <- 75.00000
class(c)
nrow(heights)
heights$height[777]
heights[777,1]
heights$sex[777]
max(heights$height)
which.min(heights$height)
mean(heights$height)
median(heights$height)
heights %>% count(sex=="Male")
812/(812+238)
heights %>% count(height>78&sex=="Female")


###### Introduction to Machine Learning ######


library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# compare heights in males and females in our data set
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# now try predicting "male" if the height is within 2 SD of the average male
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


#### Mini Assessment ####
?read_mnist()
library(dslabs)
mnist <- read_mnist()
ncol(mnist$train$images)


#### Videos again####

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)



# get the metrics
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

# access specific metrics
cm$overall["Accuracy"]

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]



# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)



p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


##### Assessment Chapter 2 ########


library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

table(x)
summary(y[1:39])
26/39
summary(y[40:150])
42/111



dat <- dat %>% mutate(y_hat = ifelse(type == "inclass", "Female", "Male"))
dat$sex <- factor(dat$sex)
dat$y_hat <- factor(dat$y_hat, levels = levels(dat$sex))
accuracy <- mean(dat$sex == dat$y_hat)

table(dat$y_hat,dat$sex)
confusionMatrix(dat$y_hat,dat$sex)





library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)




predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)


plot(iris, pch=21, bg=iris$Species)

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

find_optimal_cutoff <- function(feature, species) {
  rangedValues <- seq(range(feature)[1], range(feature)[2], by=0.1)
  accuracies <- sapply(rangedValues, function(cutoff) {
    predictions <- ifelse(feature > cutoff, 'virginica', 'versicolor')
    mean(predictions == species)
  })
  max_accuracy_index <- which.max(accuracies)
  optimal_cutoff <- rangedValues[max_accuracy_index]
  list(cutoff = optimal_cutoff, accuracy = accuracies[max_accuracy_index])
}

optimal_petal_length <- find_optimal_cutoff(train$Petal.Length, train$Species)
optimal_petal_width <- find_optimal_cutoff(train$Petal.Width, train$Species)
cat("Optimal cutoff for Petal.Length:", optimal_petal_length$cutoff, "\n")
cat("Optimal cutoff for Petal.Width:", optimal_petal_width$cutoff, "\n")
length_cutoff <- optimal_petal_length$cutoff
width_cutoff <- optimal_petal_width$cutoff

predictions_test <- ifelse(test$Petal.Length > length_cutoff & test$Petal.Width > width_cutoff, 'virginica', 'versicolor')
overall_accuracy <- mean(predictions_test == test$Species)

cat("Overall accuracy on the test data:", overall_accuracy, "\n")


######### Section 2.2. Conditional Probabilities ########


### Assessment ####


pdiseasegivenpositive <- 0.85*0.02/(0.85*0.02+0.1*0.98)



set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))


mean(test)

pdiseasegivennegative <- 0.15*0.02/(0.9*0.98+0.15*0.02)
mean(disease[test==0])

mean(disease[test==1])

mean(disease[test==1])/0.02




library(dslabs)
data("heights")

heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>% qplot(height, p, data =.)




ps <- seq(0, 1, 0.1)
heights %>% mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>% group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)



Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)


ps <- seq(0, 1, 0.1)
dat %>% 
  
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  
  qplot(x, y, data =.)



########### Chapter 2.3 Smoothing and Linear Regression for Prediction ###########


# load the dataset
library(tidyverse)
library(dslabs)
data("mnist_27")

# explore the data by plotting the two predictors
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# smallest and largest values of x1 and x2
if(!exists("mnist")) mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p1 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_1")

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p2 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_2")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# fit the model
fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

# build a decision rule
library(caret)

p_hat < predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# plot the true values
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# visual representation of p_hat
p_hat <- predict(fit, newdata = mnist_27$true_p)
p_hat <- scales::squish(p_hat, c(0, 1))
p1 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p2 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
gridExtra::grid.arrange(p1, p2, ncol = 2)



##### Assessment 3.1 ########

library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))





calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

replications <- 100
# Replicate partitioning, model training, prediction, and RMSE calculation process
results <- replicate(replications, {
  # Create partition indices using the y variable
  trainIndex <- createDataPartition(dat$y, p = 0.5, list = FALSE)
  
  # Split the data into training and testing sets
  trainSet <- dat[trainIndex, ]
  testSet <- dat[-trainIndex, ]
  
  # Train a linear model predicting y from x
  model <- lm(y ~ x, data = trainSet)
  
  # Generate predictions on the test set
  predictions <- predict(model, newdata = testSet)
  
  # Calculate the RMSE
  rmse <- calculate_rmse(testSet$y, predictions)
  
  # Store the training set, testing set, model, predictions, and RMSE
  list(train = trainSet, test = testSet, model = model, predictions = predictions, rmse = rmse)
}, simplify = FALSE)

# Extract RMSE values from the results
rmse_values <- sapply(results, function(res) res$rmse)

# Calculate mean and standard deviation of the RMSEs
mean_rmse <- mean(rmse_values)
sd_rmse <- sd(rmse_values)

# Print the mean and standard deviation of the RMSEs
mean_rmse
sd_rmse








calculate_rmse_stats <- function(n) {
  # Build the dataset with n observations
  Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>%
    setNames(c("x", "y"))
  
  # Replicate partitioning, model training, prediction, and RMSE calculation process
  results <- replicate(100, {
    # Create partition indices using the y variable
    trainIndex <- createDataPartition(dat$y, p = 0.5, list = FALSE)
    
    # Split the data into training and testing sets
    trainSet <- dat[trainIndex, ]
    testSet <- dat[-trainIndex, ]
    
    # Train a linear model predicting y from x
    model <- lm(y ~ x, data = trainSet)
    
    # Generate predictions on the test set
    predictions <- predict(model, newdata = testSet)
    
    # Calculate the RMSE
    rmse <- calculate_rmse(testSet$y, predictions)
    
    # Return the RMSE
    rmse
  })
  
  # Calculate mean and standard deviation of the RMSEs
  mean_rmse <- mean(results)
  sd_rmse <- sd(results)
  
  # Return a list with mean and standard deviation of RMSEs
  list(mean_rmse = mean_rmse, sd_rmse = sd_rmse)
}

# Set the seed
set.seed(1)

# Apply the function to different values of n
n_values <- c(100, 500, 1000, 5000, 10000)
rmse_stats <- sapply(n_values, function(n) calculate_rmse_stats(n), simplify = FALSE)

# Print the results
rmse_stats










set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
set.seed(1)
mean(rmse)
sd(rmse)




##### compare the 3 different cases and see where RMSE is lowest ###

#### case for x_1 and x_2 ###
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

rmse <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1+x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
set.seed(1)
mean(rmse)




#### case for x_1 ###
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

rmse <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
set.seed(1)
mean(rmse)



#### case for x_2 ###


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

rmse <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
set.seed(1)
mean(rmse)






# Question 8 compare again different cases#
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
rmse <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1+x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
set.seed(1)
mean(rmse)




###### Chapter 3.2 Smoothing #####



# see that the trend is wobbly
library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

# estimate the time trend in the 2008 US popular vote poll margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)




# bin smoothers
span <- 3.5
tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) 

tmp %>% filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = center), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~center)

# larger span
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")




polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))




total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()



##### Assessment 3.2 #######


library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")






span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")




dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)




library(dslabs)
data("mnist_27")
library(broom)
library(caret)


train <- mnist_27$train
test <- mnist_27$test

# Convert the target variable to numeric
train$y_numeric <- as.numeric(train$y) - 1  # Converts '2' to 0 and '7' to 1
test$y_numeric <- as.numeric(test$y) - 1    # Converts '2' to 0 and '7' to 1

# Fit the loess model on the training set
loess_fit <- loess(y_numeric ~ x_2, data = train, degree = 1, span = 0.5)

# Make predictions on the test set
test_predictions <- predict(loess_fit, newdata = test)

# Convert predictions to class labels
predicted_labels <- ifelse(test_predictions > 0.5, "7", "2")

# Calculate the accuracy on the test set
accuracy <- mean(predicted_labels == test$y,na.rm = TRUE)
accuracy










########## Section 4: Cross-validation and k-Nearest Neighbors Overview #####







library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)


data("mnist_27")

x <- as.matrix(mnist_27$train)
y <- mnist_27$train$y
knn_fit <- knn3(x,y)

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

knn_fit <- knn3(y ~ ., data = mnist_27$train)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]




knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)


####### Assessment 4.1 #######



set.seed(1)
data("heights")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

k_values <- seq(1, 101, 3)
set.seed(1)
calculate_f1 <- function(k) {
  knn_model <- knn3(sex ~ height, data = train_set, k = k)
  predictions <- predict(knn_model, test_set, type = "class")
  F_meas(data = predictions, reference = test_set$sex)
}
set.seed(1)
f1_scores <- sapply(k_values, calculate_f1)


set.seed(1)
max_f1_index <- which.max(f1_scores)
best_k <- k_values[max_f1_index]
best_k
max(f1_scores)





library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1)
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train_set <- tissue_gene_expression$x[-test_index, ]
train_labels <- tissue_gene_expression$y[-test_index]
test_set <- tissue_gene_expression$x[test_index, ]
test_labels <- tissue_gene_expression$y[test_index]

set.seed(1)
k_values <- seq(1, 11, 2)
calculate_accuracy <- function(k) {
  knn_model <- knn3(train_set, train_labels, k = k)
  predictions <- predict(knn_model, test_set, type = "class")
  mean(predictions == test_labels)
}

set.seed(1)
accuracies <- sapply(k_values, calculate_accuracy)
accuracies




####### Section 4.2 Cross-Validation ######

ks <- seq(3, 251, 2)

library(purrr)
library(dslabs)

data("mnist_27")

accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)



set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
m <- median(income)
N <- 100
X <- sample(income, N)
median(X)

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + 
  geom_abline()
grid.arrange(p1, p2, ncol = 2)

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})
quantile(M_star, c(0.025, 0.975))




##### Assessment 4.2 #####


library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)


sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)


sum(indexes[[1]] == 3)+sum(indexes[[2]] == 3)+sum(indexes[[3]] == 3)+sum(indexes[[4]] == 3)+sum(indexes[[5]] == 3)+sum(indexes[[6]] == 3)+sum(indexes[[7]] == 3)+sum(indexes[[8]] == 3)+sum(indexes[[9]] == 3)+sum(indexes[[10]] == 3)
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)


y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)
set.seed(1)
MC <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(MC)
sd(MC)


set.seed(1)
y <- rnorm(100, 0, 1)

n_bootstrap <- 10000

quantile_75 <- function(data) {
  return(quantile(data, 0.75))
}

bootstrap_samples <- replicate(n_bootstrap, quantile_75(sample(y, length(y), replace = TRUE)))

mean(bootstrap_samples)
sd(bootstrap_samples)



########### Section 5: The Caret Package ###########


library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")

ggplot(train_knn, highlight = TRUE)

data.frame(k = seq(9, 67, 2))

set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

ggplot(train_knn, highlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

names(train_knn_cv$results)

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])




install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1



########## Assessment 6.1 ###########



library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]


set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results


pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}


ind <- which(pvals <= 0.01)
length(ind)


set.seed(1)

x_subset <- x[, ind]

fit <- train(x_subset, y, method = "glm")
fit$results


set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


### Code to read ###

# Load tidyverse
library(tidyverse)

# load package for decision tree
library(rpart)

# load the dslabs package
library(dslabs)

# load the caret package
library(caret)

# fit a classification tree using the polls_2008 dataset, 
# which contains only one predictor (day)
# and the outcome (margin)
fit <- rpart(margin ~ ., data = polls_2008)

# display the decision tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# examine the fit from the classification tree model
polls_2008 %>%  
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# fit a classification tree on the mnist data using cross validation
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
# and plot it
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

# load library for random forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#### Assessment 5.1. cont. #####




library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) + geom_step(aes(x, y_hat), col=2)



library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
  
  
plot(fit)



library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")  
  
  
  
  
### Assessment 5.1 cont. caret package ####
  
  
library(rpart)
library(dslabs)
library(caret)
data("tissue_gene_expression")

df <- data.frame(tissue_gene_expression$x)
df$tissue <- tissue_gene_expression$y

set.seed(1991)
cp <- seq(0, 0.1, 0.01)

fit <- train(tissue ~ ., 
             method = "rpart",
             tuneGrid = data.frame(cp), 
             data = df)

plot(fit)

best_model <- fit$bestTune
best_accuracy <- max(fit$results$Accuracy)

best_model
best_accuracy




set.seed(1991)
fit <- train(tissue ~ ., 
             method = "rpart",
             control = rpart.control(minsplit = 0),
             tuneGrid = data.frame(cp), 
             data = df)

plot(fit)

best_model <- fit$bestTune
best_accuracy <- max(fit$results$Accuracy)

best_model
best_accuracy


best_tree <- fit$finalModel
rpart.plot(best_tree)

print(best_tree$frame$var[1])



mtry <- seq(50, 200, 25)
set.seed(1991)
fit <- train(tissue ~ ., 
             method = "rf",
             nodesize = 1,
             tuneGrid = data.frame(mtry=mtry), 
             data = df)


best_model <- fit$bestTune
best_accuracy <- max(fit$results$Accuracy)

best_model
best_accuracy




########### Assessment 5.2 #############


library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

table(train_set$Survived)/length(train_set$Survived)

set.seed(3)
y_hat <- sample(c("0", "1"), nrow(test_set), replace = TRUE) %>% 
  factor(levels = levels(test_set$Survived))

mean(y_hat == test_set$Survived)


female_survivors <- train_set[train_set$Sex == "female", ]
proportion_female_survived <- mean(female_survivors$Survived == 1)
proportion_female_survived


male_survivors <- train_set[train_set$Sex == "male", ]
proportion_male_survived <- mean(male_survivors$Survived == 1)
proportion_male_survived





sex_based_prediction <- ifelse(test_set$Sex == "female", 1, 0)

actual_survival <- test_set$Survived
accuracy_sex_based <- mean(sex_based_prediction == actual_survival)
accuracy_sex_based


table(train_set$Pclass)

class1 <- train_set[train_set$Pclass==1,]
table(class1$Survived)/length(class1$Survived)

class2 <- train_set[train_set$Pclass==2,]
table(class2$Survived)/length(class2$Survived)

class3 <- train_set[train_set$Pclass==3,]
table(class3$Survived)/length(class3$Survived)



class_based_prediction <- ifelse(test_set$Pclass == "1", 1, 0)

actual_survival <- test_set$Survived
accuracy_class_based <- mean(class_based_prediction == actual_survival)
accuracy_class_based


train_set %>% group_by(Pclass, Sex) %>% count(Survived==1)



sex_class_based_prediction <- ifelse(test_set$Sex == "female" & test_set$Pclass %in% c(1, 2), 1, 0)

# Compare the sex- and class-based predictions with the actual survival in the test set
actual_survival <- test_set$Survived
accuracy_sex_class_based <- mean(sex_class_based_prediction == actual_survival)
accuracy_sex_class_based




confusionMatrix(data = factor(sex_based_prediction), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_based_prediction), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_based_prediction), reference = factor(test_set$Survived))





F_meas(data = factor(sex_based_prediction), reference = factor(test_set$Survived))
F_meas(data = factor(class_based_prediction), reference = factor(test_set$Survived))
F_meas(data = factor(sex_class_based_prediction), reference = factor(test_set$Survived))



#### part 2 ####


set.seed(1)
model <- train(Survived ~ Fare, 
               data = train_set, 
               method = 'gamLoess')

prediction <- predict(model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy


set.seed(1)
model <- train(Survived ~ Age, 
               data = train_set, 
               method = 'glm')
prediction <- predict(model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy

set.seed(1)
model <- train(Survived ~ Age+Fare+Sex+Pclass, 
               data = train_set, 
               method = 'glm')
prediction <- predict(model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy


set.seed(1)
model <- train(Survived ~ ., 
               data = train_set, 
               method = 'glm')
prediction <- predict(model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy



set.seed(6)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

prediction <- predict(train_knn, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy

plot(train_knn)



set.seed(8)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = 'cv', number = 10,p=0.9))
train_knn$bestTune
prediction <- predict(train_knn, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy




set.seed(10)
dt_model <- train(Survived ~ ., 
                  data = train_set, 
                  method = 'rpart',
                  tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

optimal_cp <- dt_model$bestTune$cp
optimal_cp

prediction <- predict(dt_model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy


library(rpart.plot)
final_model <- dt_model$finalModel
rpart.plot(final_model)



set.seed(14)
rf_model <- train(Survived ~ ., 
                  data = train_set, 
                  method = 'rf',
                  tuneGrid = data.frame(mtry = seq(1:7)),
                  ntree=100)
optimal_mtry <- rf_model$bestTune$mtry
optimal_mtry

prediction <- predict(rf_model, newdata = test_set)
accuracy <- mean(prediction == test_set$Survived)
accuracy




####### Section 6: Model Fitting and Recommendation Systems Overview ####


library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)





colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]




library(randomForest)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <-  train(x[, col_index], y,
                   method = "rf",
                   nTree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

fit_rf <- randomForest(x[, col_index], y,
                       minNode = train_rf$bestTune$mtry)

y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]





imp <- importance(fit_rf)
imp

mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))


p_max <- predict(fit_rf, x_test[,col_index], type = "prob") 
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)

ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

rafalib::mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}





p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)$overall["Accuracy"]






########### Assessment 6.1 ############





models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")


library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


predictions <- sapply(fits, function(fit) {
  predict(fit, newdata = mnist_27$test)
})

dim(predictions)



accuracies <- sapply(1:ncol(predictions), function(i) {
  mean(predictions[, i] == mnist_27$test$y)
})

mean(accuracies)



ensemble_prediction <- apply(predictions, 1, function(row) {
  if (mean(row == 7) > 0.5) 7 else 2
})

mean(ensemble_prediction == mnist_27$test$y)




ind <- accuracies > 0.825
sum(ind)
models[ind]






accuracy_estimates <- sapply(fits, function(fit) {
  min(fit$results$Accuracy)
})

mean(accuracy_estimates)






filtered_fits <- fits[accuracy_estimates >= 0.8]

filtered_predictions <- sapply(filtered_fits, function(fit) {
  predict(fit, newdata = mnist_27$test)
})

filtered_ensemble_prediction <- apply(filtered_predictions, 1, function(row) {
  if (mean(row == 7) >= 0.5) 7 else 2
})

mean(filtered_ensemble_prediction == mnist_27$test$y)



######## Chapter 6.2 ######



library(dslabs)
library(tidyverse)
data("movielens")

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))


library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")




RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse


predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)


# fit <- lm(rating ~ as.factor(userId), data = movielens)


mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
model_1_rmse


train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))


user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse



##### Assessment 6.2 ########


library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movie_ratings_count <- movielens %>%
  group_by(movieId, title, year) %>%
  summarize(num_ratings = n()) %>%
  ungroup()



ggplot(movie_ratings_count, aes(x = as.factor(year), y = sqrt(num_ratings))) +
  geom_boxplot() +
  labs(title = "Number of Ratings by Year", x = "Year", y = "Square Root of Number of Ratings") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Find the year with the highest median number of ratings
median_ratings_per_year <- movie_ratings_count %>%
  group_by(year) %>%
  summarize(median_ratings = median(num_ratings)) %>%
  arrange(desc(median_ratings))

# Display the year with the highest median number of ratings
highest_median_year <- median_ratings_per_year %>%
  slice(1)

highest_median_year





movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))











end_year <- 2018

# Filter movies released in 1993 or later, calculate ratings per year, and select top 25
top_movies <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(
    n = n(),
    years = end_year - first(year),
    title = first(title),
    rating = mean(rating)
  ) %>%
  mutate(rate = n / years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# Make a plot of average rating versus ratings per year with a trend estimate
ggplot(top_movies, aes(x = rate, y = rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average Rating vs Ratings per Year",
       x = "Ratings per Year",
       y = "Average Rating") +
  theme_minimal()



movielens <- mutate(movielens, date = as_datetime(timestamp))



movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()








movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






######## Section 6.3 ########

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% pull(title)



movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  pull(title)

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  pull(title)


train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  slice(1:10) %>% 
  pull(n)


######## Assessment 6.3 ########


options(digits=7)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))


scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))



schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)


median(n)

list <- schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score) 
median(list$size)


list <- schools %>% top_n(-10, score) %>% arrange(score) %>% select(id, size, score) 
median(list$size)



schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)



overall <- mean(sapply(scores, mean))


alpha <- 25

schools <- schools %>% mutate(
  regularized_score = overall + (score - overall) * size / (size + alpha)
)

sorted_schools_regularized <- schools %>% arrange(desc(regularized_score))
head(sorted_schools_regularized, 10)









compute_rmse <- function(alpha) {
  schools <- schools %>% mutate(
    regularized_score = overall + (score - overall) * size / (size + alpha)
  )
  rmse <- sqrt(mean((schools$quality - schools$regularized_score)^2))
  return(rmse)
}


alpha_values <- seq(10, 250, by=1)
rmse_values <- sapply(alpha_values, compute_rmse)

optimal_alpha <- alpha_values[which.min(rmse_values)]
optimal_alpha




schools <- schools %>% mutate(
  regularized_score = overall + (score - overall) * size / (size + optimal_alpha)
)

sorted_schools_regularized <- schools %>% arrange(desc(regularized_score))
head(sorted_schools_regularized, 10)











alphas <- seq(10, 250)
rmse_no_centering <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x) / (length(x) + alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})


plot(alphas, rmse_no_centering, type="l", main="RMSE vs. Alpha (Without Centering)", xlab="Alpha", ylab="RMSE")


optimal_alpha_no_centering <- alphas[which.min(rmse_no_centering)]
min_rmse_no_centering <- min(rmse_no_centering)

cat("Optimal alpha (without centering):", optimal_alpha_no_centering, "\n")





########## Chapter 6.3 cont. ##########

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  dplyr::select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)]) 



y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))




y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)



##### Assessment 6.3 cont. #####



set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))



my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)



my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)



s <- svd(y)
names(s)


y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))







ss_y <- colSums(y^2)

# Compute sum of squares of columns of transformed y
ss_yv <- colSums((s$u %*% diag(s$d))^2)

# Verify the sums are equal
sum_ss_y <- sum(ss_y)
sum_ss_yv <- sum(ss_yv)




sum_ss_y
sum_ss_yv







plot(ss_y)
plot(ss_yv)







data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()













s <- svd(y)

# Extract the singular values
singular_values <- s$d

# Compute the total variability (sum of the squares of all singular values)
total_variability <- sum(singular_values^2)

# Compute the variability explained by the first three columns (sum of the squares of the first three singular values)
explained_variability_first_three <- sum(singular_values[1:3]^2)

# Compute the proportion of the total variability explained by the first three columns
proportion_explained_first_three <- explained_variability_first_three / total_variability

# Print the proportion
proportion_explained_first_three







identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))









# Load necessary libraries
library(tidyverse)
library(lubridate)
library(dslabs)
library(ggrepel)
library(RColorBrewer)

# Load data and prepare the train_small dataset
data("movielens")

train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  dplyr::select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(y) <- y[, 1]
y <- y[, -1]

movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# Center the matrix
y <- sweep(y, 1, rowMeans(y, na.rm = TRUE))
y <- sweep(y, 2, colMeans(y, na.rm = TRUE))

# Replace NA values with 0
y[is.na(y)] <- 0

# Center the rows
y <- sweep(y, 1, rowMeans(y))

# Perform PCA
pca <- prcomp(y)

# Compute the average score for each student
average_scores <- rowMeans(y)

# Extract the first principal component (PC1) scores
pc1_scores <- pca$x[, 1]

# Create a data frame for plotting
df_plot <- data.frame(Average_Scores = average_scores, PC1_Scores = pc1_scores)

# Plot the average scores against PC1 scores
ggplot(df_plot, aes(x = Average_Scores, y = PC1_Scores)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Average Scores vs. PC1 Scores",
       x = "Average Scores",
       y = "PC1 Scores") +
  theme_minimal()






my_image(s$v)






plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)





plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)



plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)



y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))
