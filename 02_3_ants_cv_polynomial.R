#' ---
#' title: "Ant data: k-fold cross validation"
#' author: Brett Melbourne
#' date: 13 Jan 2026
#' output:
#'     github_document
#' ---

#' Explore the cross-validation **inference algorithm** from scratch with the
#' ants data and a polynomial model. Our goal is to predict richness of forest
#' ants from latitude. What order of a polynomial **model algorithm** gives the
#' most accurate predictions?

#+ results=FALSE, message=FALSE, warning=FALSE
library(ggplot2) # for data visualization 
library(dplyr) # for data manipulation 
library(tidyr) #for pivot_longer()

#' Ant data:

ants <- read.csv("data/ants.csv")
head(ants)

#' Forest ant data:

forest_ants <- ants %>% # other pipe operator is |>
    filter(habitat=="forest") # grab only the rows when the "habitat" column is equal to "forest" 

forest_ants %>%
    ggplot() +
    geom_point(aes(x=latitude, y=richness)) + # create a visual object known as a geom, asesthetics 
    ylim(0,20)

#' Here's one way we could code a 3rd order polynomial by first creating new
#' variables for the quadratic (squared) and cubic (cubed) terms, and using R's
#' model formula syntax to train the model by minimizing the SSQ with the
#' function `lm`.

forest_ants$latitude_2 <- forest_ants$latitude ^ 2 # forming new columns for ^2
forest_ants$latitude_3 <- forest_ants$latitude ^ 3
head(forest_ants)
lm(richness ~ latitude + latitude_2 + latitude_3, data=forest_ants) #fitting a training algorithm to the data 
        # giving coefficients that are associated with each term 
        # intercept is beta 0 

#' A model formula provides a shorthand notation for (mostly) linear models,
#' e.g. `y ~ x + z` is shorthand for the model:
#' 
#' $$
#' y = \beta_0 + \beta_1 x + \beta_2 z
#' $$
#' 

#' Here's another way to code the same model that eliminates the need to create
#' new variables for higher order terms.

lm(richness ~ latitude + I(latitude^2) + I(latitude^3), data=forest_ants) # instead of making new columns for polynomial terms can do the calculations within the specificiations of the linear model 
        # I is the identitiy function, ^ means something special in lm syntax so it needs to be in the I, take this literally 

#' The `I()` function ensures that `^` is not interpreted as model formula
#' syntax. See `?formula` for more details about model formulae.

#' An even more convenient way uses the function `poly()`, which creates a
#' matrix of the polynomial terms.

poly(forest_ants$latitude, degree=3, raw=TRUE) # actually do this using the funtion poly, this is encapsulating the model alg in a function
        # inputs are (data we want to predict from, which degree of polynomial aka tuning)
        # not really interested in parameters, we want to control how flexible is the model 
        # raw = true means this is a form of hte algorthm that is the same as 
        # poly makes the polynomial terms for us 

#' We can use this directly within a model formula

lm(richness ~ poly(latitude, degree=3, raw=TRUE), data=forest_ants) # problem that x^2 is highly correlated with x^3

#' A potential problem with polynomial models is that the higher order terms can
#' become almost perfectly correlated with one another, leading to models where
#' the parameters can't all be uniquely estimated. For example, for these data
#' the fourth order polynomial can be trained but for the fifth order polynomial
#' we can't determine a unique value for the highest order parameter, and the
#' parameter estimates remain the same as the fourth order model. We have
#' essentially run out of uniqueness among the polynomial terms due to the high
#' correlations.

lm(richness ~ poly(latitude, degree=4, raw=TRUE), data=forest_ants)
lm(richness ~ poly(latitude, degree=5, raw=TRUE), data=forest_ants) # run into a problem that we can't fit a polynomial fit, 4 and 5 are same model essentially, run out of information 
cor(poly(forest_ants$latitude, degree=5, raw=TRUE)) # limited in its degree of flexibility, depends on the information content of the data (bigger dataset you could get to higher polynomials)

#' This problem can be markedly reduced by using orthogonal polynomials, which
#' remove the correlation among the polynomial terms. Orthogonal polynomials are
#' the default type for `poly()`.

lm(richness ~ poly(latitude, degree=5), data=forest_ants) # can re-parameterize the model into an othogonal polynomial, allows seperate, remove raw= TRUE, default is orthogonal param 
cor(poly(forest_ants$latitude, degree=5))

#' Orthogonal polynomials give the same predictions as the raw polynomials. It's
#' just a difference in parameterization of the same model. In machine learning
#' we don't care about the parameter values, just the resulting prediction, so
#' it's best to choose the more robust parameterization.
#' 

#' R's `lm()` function contains a **training algorithm** that finds the
#' parameters that minimize the sum of squared deviations of the data from the
#' model. The following code trains the order 4 polynomial and plots the fitted
#' model. Use this block of code to try different values for the order of the
#' polynomial. We can get up to order 16, after which we can no longer form
#' orthogonal polynomials.

order <- 3 #integer     # flexibility comes through changing the order of the model 
poly_trained <- lm(richness ~ poly(latitude, order), data=forest_ants)
grid_latitude  <- seq(min(forest_ants$latitude), max(forest_ants$latitude), length.out=201)
nd <- data.frame(latitude=grid_latitude)
pred_richness <- predict(poly_trained, newdata=nd)
preds <- cbind(nd, richness=pred_richness)

ggplot(data=NULL, aes(x=latitude, y=richness)) +
    geom_point(data=forest_ants) +
    geom_line(data=preds) +
    coord_cartesian(ylim=c(0,20)) +
    labs(title=paste("Polynomial order", order)) # fit model to the data and have a look at what the 
        # if you increase order a ton then you get more and more wiggles 

#' Use `predict` to ask for predictions from the trained polynomial model. For
#' example, here we are asking for the prediction at latitude 43.2 and we find
#' the predicted richness is 5.45. We need to provide the predictor variable
#' `latitude` as a data frame even if it's just one value. See `?predict.lm`.

predict(poly_trained, newdata=data.frame(latitude=43.2))

#' Exploring the k-fold CV algorithm
#'
#' First, we need a function to divide the dataset up into partitions.

# Function to divide a data set into random partitions for cross-validation
# n:       length of dataset (scalar, integer)
# k:       number of partitions (scalar, integer)
# return:  partition labels (vector, integer)
# 
random_partitions <- function(n, k) {
    min_n <- floor(n / k)
    extras <- n - k * min_n
    labels <- c(rep(1:k, each=min_n),rep(seq_len(extras)))
    partitions <- sample(labels, n)
    return(partitions)
}

#' What does the output of `random_partitions()` look like? It's a set of labels
#' that says which partition each data point belongs to.

random_partitions(nrow(forest_ants), k=5) # fold or partition labels for each data point in n 
cbind(forest_ants, random_partitions(nrow(forest_ants), k=5)) # which row of the data belongs to which partition 
# if you run it again you should get a different random partition 
random_partitions(nrow(forest_ants), k=nrow(forest_ants)) # Each data point gets its own fold 


#' Now code up the k-fold CV algorithm (from our pseudocode to R code) to
#' estimate the prediction mean squared error for one order of the polynomial.
#' Try 5-fold, 10-fold, and n-fold CV. Try different values for polynomial
#' order.


# PSEUDO-CODE: 
# divide dataset into k parts i = 1...k
# initiate vector to hold e
# for each i
#     test dataset = part i
#     training dataset = remaining data
#     find f using training dataset
#     use f to predict for test dataset 
#     e_i = prediction error (MSE)
# CV_error = mean(e)

# In Class Code ____________________
k <- 5
order <- 3 

# divide dataset into k parts i = 1...k using random partitions functions 
forest_ants$partition <- random_partitions(nrow(forest_ants), k) 


# initiate vector to hold e
e <- rep(NA, k)# initate a vector that is k values long full of NA values 

# for each i
for ( i in 1:k){ # repetition structure 
    
    # identify your test dataset = part i, subset of the data all of which belong to partition i 
    test_data <- subset(forest_ants, partition == i ) #subset is a base R function, grab only the rows where partition is equal to i, the fold you are on 
    
    #training dataset = remaining data
    train_data <- subset(forest_ants, partition != i ) # all data where partition does not equal i 
        # we have now done the train and test split! 
    
    # find f using training dataset; now we want to train the polynomial model on the training data 
    f_trained <- lm(richness ~ poly(latitude, order), data= train_data ) # train model on training data 
        # gives the output of our polynomial model 
    
    # use f to predict for test dataset; now use function to predict for test dataset 
    pred_richness <- predict(f_trained, newdata=test_data) # predict on the test data, model will be taking the input of latitude from test data and predicting richness
            # returns a vector of predicted species richness from the trained model and the training dataset 
    
    #e_i = prediction error (MSE); now we want to calculate the error for those predicted values 
    e[i] <- mean((test_data$richness - pred_richness)^2) # we want to keep the value of the error for this repetition 
        # to calc precitions error = diff between obs and predicted mean squared error 
        # remember partitions are random so these numbers will look different 
    
}
 # e is now we have the mean squared error for each fold 

# CV_error = mean(e)
cv_error = mean(e) #the units on this are the mean squared richness 
sqrt(cv_error) # richness will be off by this amount 
# but if you run this over and over you will get different cv_errors 
# this is how we get out prediction error 
# depends on k, the order

# now we can use this alg to measure the prediction error for different values of the order "what degree of wiggliness" 
# so you could wrap this in another loop 

# Turn into a function ______ 
cv_poly_ants <- function(forest_ants, k, order){
    forest_ants$partition <- random_partitions(nrow(forest_ants), k) 
    e <- rep(NA, k)# initate a vector that is k values long full of NA values 
    for ( i in 1:k){ # repetition structure 
        test_data <- subset(forest_ants, partition == i ) #subset is a base R function, grab only the rows where partition is equal to i, the fold you are on 
        train_data <- subset(forest_ants, partition != i ) # all data where partition does not equal i 
        f_trained <- lm(richness ~ poly(latitude, order), data= train_data ) # train model on training data 
        pred_richness <- predict(f_trained, newdata=test_data) # predict on the test data, model will be taking the input of latitude from test data and predicting richness
        e[i] <- mean((test_data$richness - pred_richness)^2) # we want to keep the value of the error for this repetition 
        
    }
    cv_error = mean(e) #the units on this are the mean squared richness 
    return(cv_error)
}

cv_poly_ants(forest_ants, k=2, order= 2)

#Now we want to be able to look at different values for the order

set.seed(1193)
grid <- expand.grid(k = c(5,10, nrow(forest_ants)), order = 1:8) # makes a grid for you of all the possible combinations 
cv_error <- rep(NA, nrow(grid)) # make a vector to hold the output 
for(i in 1:nrow(grid) ){
    cv_error[i] <- cv_poly_ants(forest_ants, k = grid$k[i], order = grid$order[i]) #fun indexing 
}
result1 <- cbind(grid, cv_error)
result1

# plot 
result1 %>% 
    ggplot() + 
    geom_line(aes(x = order, y = cv_error, col = factor(k))) + 
    coord_cartesian(ylim= c(10, 25)) #this shows the values that are off the grid of your ylims 

# see that 5 fold is super variable and all ofer the place 
# at all three k folds it generally looks like the smaller ks are going to give you better predictive accuracy 
# folds do change if we would choose order 2 or 3 
# there is variability because the cross validation alg is a stochastic alg 
# could add on to do multiple random partitions 

# n fold cv is also called leave one out alg 



