#simulate data points
set.seed(2019) #Set the random seed (ensures results are reproducible)

# Training Data 
n = 60 #number of  observations
x_train = sort(runif(n, -2, 2))
y_train = x_train^3 + 4*sin(4*x_train) + rnorm(n) # noisy responses
df_train = data.frame(cbind(y_train,x_train))

# Testing Data 
x_test = seq(-2, 2, by = 0.1)  #out-of-sample predictors
y_test = x_test^3 + 4*sin(4*x_test) 


#######################################################################
################ NEURAL NETWORK WITH neuralnet ########################
#######################################################################
library(neuralnet)
hidden = c(8) #number of hidden neurons in each layer

#####neural network calibration
#linout specifies regression outputs are produced (otherwise a logistic activation is applied
#at the end for classification)
theformula = as.formula("y_train ~ x_train")
Neunet = neuralnet(formula=theformula, hidden=hidden, data = df_train, learningrate = 0.01)


# neuralnet predictions 
ptm <- proc.time()
PredictNeunet = predict(Neunet, newdata=matrix(x_train,ncol=1)) # on training data
PredictNeunet2 = predict(Neunet, newdata = matrix(x_test,ncol=1)) # on test data 
proc.time() - ptm


testSSE = sum((y_test - PredictNeunet2)^2)
testSSE

# Plotting results 
plot(x_train, y_train, main='Predictions with neuralnet \n Test SSE: 19.038', xlab = "x", ylab = "y")
#lines(x_train, PredictNeunet, col='blue', type="p", pch=1)
lines(x_test, PredictNeunet2, col='red', type="l", pch=1)
#legend(y=7.5,x=-1.8, legend=c('Data points','Fitted Values','Predictions new points'), col=c('black','blue','red') , lty=c(0,0,0), pch=c(1,1,1))






#######################################################################
################ NEURAL NETWORK WITH nnet ########################
#######################################################################

###### NEURAL NETWORK WITH nnet package ########

#install.packages("nnet")
#install.packages("NeuralNetTools")
library(nnet)
library(NeuralNetTools)
library(neuralnet)


#number of units in the hidden layer
nunits = 8

# The training data 
set.seed(1)
n = 60 # nb of observations 
x = sort(runif(n,-2,2))
x_train = x
y = x^3 + 4*sin(4*x) + rnorm(n)
y_train = y
df_train = data.frame(cbind(y, x))

# The testing data 
x = seq(-2, 2, by = 0.1)
x_test = x
y = x^3 + 4*sin(4*x) 
y_test = y 
df_test = data.frame(cbind(y,x))


# Graphical representation of the neural network


# nnet predictions 
ptm <- proc.time()
NNET = nnet(formula = y ~ x , size = nunits, linout=TRUE, data = df_train) 
PredictNeunet = predict(NNET, data = x_test) #predictions for the x's insample
PredictNeunet2 = predict(NNET, newdata = df_test$x) #predictions for new examples
proc.time() - ptm



testSSE = sum((y_test - PredictNeunet2)^2)
testSSE

# Plotting results 
plot(x_train, y_train, main='Predictions with nnet \n Test SSE: 5.145', xlab = "x", ylab = "y")
#lines(x_train, PredictNeunet, col='blue', type="p", pch=1)
lines(x_test, PredictNeunet2, col='red', type="l", pch=1)
# #legend(y=7.5,x=-1.8, legend=c('Data points','Fitted Values','Predictions new points'), col=c('black','blue','red') , lty=c(0,0,0), pch=c(1,1,1))
# 
# 
# 
# # performance of the two neural networks.                             #
# #######################################################################
# 
# 
# #simulate data points
# set.seed(2019) #Set the random seed (ensures results are reproducible)
# 
# # Training Data 
# n = 60 #number of  observations
# x_train = sort(runif(n, -2, 2))
# y_train = x_train^3 + 4*sin(4*x_train) + rnorm(n) # noisy responses
# df_train = data.frame(cbind(y_train,x_train))
# 
# # Testing Data 
# x_test = seq(-2, 2, by = 0.1)  #out-of-sample predictors
# y_test = x_test^3 + 4*sin(4*x_test) 
# 
# 
# 
# # In sample predictions 
# plot(x = df_train$x, y = df_train$y, col = "blue", main = 'Neural network predictions with nnet', xlab = "x", ylab = "y")
# lines(df_train$x, PredictNeunet, col='red', type="p", pch=1)
# #legend(y=12.5,x=-1.8, legend=c('Data points','Fitted Values','Predictions new points'), col=c('black','blue','red') , lty=c(0,0,0), pch=c(1,1,1))
# 
# # Out of sample predictions 
# plot(df_test$x, df_test$y, col='purple', type = "l", main = "nnet Predictions", xlab = "x", ylab = "y")
# points(df_test$x,PredictNeunet2, col='green4', pch=1, cex = 0.5)
# 
# 
# 
# sum((PredictNeunet2 - df_test$y)^2)
# 
# 
# 
# 
# 
# # Weights of the neuralnet 
# neuralweights(Neunet)
# nnet.struct = neuralweights(Neunet)$struct
# wmat_hid = matrix(data = unlist(neuralweights(Neunet)$wts)[1:(2*nunits)], nrow = nunits, byrow = TRUE)
# wmat_out = neuralweights(Neunet)$wts$out
# Wmat = list(wmat_hid, wmat_out)
# 
# # Validating weight usage
# y_hat = applyFFNN(x, Wmat, sigmoid)
# plot(x, y_hat, type = 'p', col = 'red', cex = 0.5)
# points(x, PredictNeunet2, col = "green4", cex = 0.5)
# 
# # Organizing wector of initial weights (both of the following work)
# init.weights = unlist(neuralweights(Neunet)$wts)
# #init.weights = c(as.vector(t(wmat_hid)), wmat_out)
# 
# # Using initial weights vectro
# Neunet = nnet(formula = y~x, 
#               data = df_train,
#               size = nunits,
#               linout=TRUE,
#               weights = c(0.5, 0.5, 0.5, rep(1, length(x) - 6), 0.5, 0.5, 0.5),
#               #Wts = init.weights,
#               #subset = sample(35),
#               Hess = TRUE,
#               summ = 2, 
#               na.omit = T 
#               ) 
# 
# Neunet = nnet(formula = y~x, 
#               data = df_train,
#               size = nunits,
#               linout=TRUE,
#               #weights = rep(1/nrow(df_train), nrow(df_train)),
#               #Wts = init.weights,
#               #subset = sample(35),
#               Hess = TRUE,
#               summ = 2, 
#               na.omit = T 
# ) 
# 
# 
# 
# 
# plot(Neunet)
# 
# 
# # Using initial weights vectro
# Neunet = nnet(formula = y~x, 
#               data = df_train,
#               size = nunits,
#               linout= FALSE,
#               entropy = FALSE, 
#               #Wts = init.weights,
#               #subset = sample(35),
#               Hess = TRUE,
#               summ = 2, 
#               na.action = ifelse(x = NA, 0, NULL)
# ) 
# 
# 
# 
# # performance of the two neural networks.                             #
# #######################################################################
# 
# 
# # Plotting results 
# plot(x_train, y_train, main='Predictions with neuralnet \n SSE: 11.562', xlab = "x", ylab = "y")
# #lines(x_train, PredictNeunet, col='blue', type="p", pch=1)
# lines(x_test, PredictNeunet2, col='red', type="l", pch=1)
# legend(y=7.5,x=-1.8, legend=c('Data points','Fitted Values','Predictions new points'), col=c('black','blue','red') , lty=c(0,0,0), pch=c(1,1,1))
# 

