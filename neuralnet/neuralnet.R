#######################################################################
# Here we train two neural networks; (1) using neuralnet package      #
#                                    (2) using first principles       #
# We generate the training and testing data below, then compare the   #
# performance of the two neural networks.                             #
#######################################################################


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

# Graphical representation of the neural network
plot(Neunet) 

# Neural network predictions 
PredictNeunet = predict(Neunet, newdata=matrix(x_train,ncol=1)) # on training data
PredictNeunet2 = predict(Neunet, newdata = matrix(x_test,ncol=1)) # on test data 


# Plotting results 
plot(x_train, y_train, main='Predictions with neuralnet \n SSE: 11.562', xlab = "x", ylab = "y")
#lines(x_train, PredictNeunet, col='blue', type="p", pch=1)
lines(x_test, PredictNeunet2, col='red', type="l", pch=1)
legend(y=7.5,x=-1.8, legend=c('Data points','Fitted Values','Predictions new points'), col=c('black','blue','red') , lty=c(0,0,0), pch=c(1,1,1))



### Example showing how starting weights can be used and can take List argument. This is not used anywhere.
sw = Neunet$weights 
NN = neuralnet(formula=theformula, data = df_train, hidden = 8, threshold = 700,
          rep = 1, stepmax = 2000000,
          learningrate = 0.01, lifesign = "full",
          lifesign.step = 1000, algorithm = "backprop", err.fct = "sse",
          act.fct = "logistic", linear.output = TRUE, startweights = sw)


#######################################################################
########### FEEDFORWARD NEURAL NETWORK by Spyros Orfanos   ############
#######################################################################

# The data (same as above)
set.seed(2019)
n = 60 # nb of observations 
x_data = sort(runif(n,-2,2))
y_data = x_data^3 + 4*sin(4*x_data) + rnorm(n)

# FFNN parameters 
K = max(1, nrow(x_data))  # dimension of y
p = max(1, nrow(x_data))  # dimension of x
M = 8 # nb of hidden units 
L = 2 # nb layers
alpha = 0.01 #learning rate 
nb_iter = 20000  # nb of iterations to update weights 

# Initializing weight matrices randomly
Wmat = list()
Wmat[1:(L-1)] = lapply(seq_len(L-1), function(x) matrix(data = rnorm(M*(p+1)), nrow = M, ncol = p+1))
Wmat[[L]] = matrix(data = rnorm(K*(M+1)), nrow = K, ncol = M+1)   # output layer 
Wmat

# Logistic sigmoid as activation function
sigmoid = function(x){
  1/(1+exp(-x))
}

# Derivative of logistic sigmoid 
deriv.sigmoid = function(x){
  sigmoid(x)*(1-sigmoid(x))
}

# Sum of Squared Error as cost function
cost.fun = function(y_data, y_hat){
  sum((y_hat - y_data)^2)/2 
}

# Apply Feedforward Neural Network 
applyFFNN = function(x_data, Wmat, activ.fun){
  Z_vec = rbind(rep(1,length(x_data)/p), x_data)
  layer = 1
  # inner layers 
  while (layer < L){
    Z_vec = rbind(rep(1, ncol(Z_vec)), activ.fun(Wmat[[layer]] %*% Z_vec) ) 
    layer = layer + 1 
  }
  # output layer 
  y_hat = Wmat[[L]] %*% Z_vec 
  return(y_hat)    
}

# Backpropogation algorithm 
nnetGradient = function(x_data, y_data, Wmat, activ.fun, deriv.activ.fun){
  GradientList = matrix(0, M*(p+1)+K*(M+1), 1) #vector with length of all wmat1 + wmat2 elements
  GradientList1 = matrix(0,nrow = M, ncol= 2)
  f = applyFFNN(x_data, Wmat, activ.fun)
  m = length(y_data) #*****Takes into account the number of observations you want your gradient to be computed on
  #partial derivative for wmat1[i,j]
  
  for (i in 1:M){  #loop over all rows of wmat1
    for (j in 1:2){ #loop over each column of a single row
      
      x = matrix(append(x_data,1,after=0),2,1) #(p+1) by 1 vector containing [1, observation]
      dh = deriv.activ.fun(Wmat[[1]][i,]%*%x)
      
      if (j==1){A = Wmat[[2]][1,i]*dh}
      else if (j==2){A = Wmat[[2]][1,i]*dh*x_data}
      
      GradientList1[i,j] = -2*t(y_data-f)*A   #calculation of gradient for wmat1[i,j]
    }
  }
  
  #partial derivative for wmat2[i,j]
  GradientList2 = matrix(0, nrow= M+1, ncol = 1)
  for (j in 1:M+1){  #loop over all columns of wmat2 (1 row)
    
    GradientList2[j,1] = -2*t(y_data-f)*activ.fun(Wmat[[1]][j-1,]%*%x)    #calculation of gradient for wmat2[1,j]
    GradientList2[1,1] = -2*t(y_data-f)
  }
  GradientList = list(GradientList1, t(GradientList2))
  return(GradientList)
}

# Applying FFNN using backpropagation 
SSE = rep(0,nb_iter)
for (j in 1:nb_iter){
  for (i in 1:n){
    y_obs = y_data[i]
    x = x_data[i]
    #computing the gradient for a single observation
    PartialGradient = nnetGradient(x, y_obs, Wmat, sigmoid, deriv.sigmoid)
    Wmat[[1]] = Wmat[[1]] - alpha*PartialGradient[[1]]
    Wmat[[2]] = Wmat[[2]] - alpha*PartialGradient[[2]]
  }
  # SSE after each iteration 
  SSE[j] = cost.fun(y_data, applyFFNN(x_data, Wmat, sigmoid))
}

# Predictions of the neural net on full data 
x_true = seq(-2, 2, by = 0.1)
pred = applyFFNN(x_true, Wmat, sigmoid)

# Plot the predictions 
plot(x_true, y=x_true^3 + 4*sin(4*x_true), col = "green3", type = "l", xlab = "", ylab = "") 
lines(x_true, pred, col = "purple") 
abline(h = 0, v = 0, col = "gray")
legend("bottomright", legend = c("actual", "predicted"), text.col = c("green4", "purple4"), lty = 1:1, col = c("green3", "purple"), cex = 0.75)

# Plot SSE after each iteration 
plot(SSE, xlab = "iteration", type = 'l', col = 'red3')



#######################################################################
#### COMPARING/VALIDATING THE RESULTS OF THE TWO NEURAL NETWORKS  ####
#######################################################################

# (1) Are the weights in the same format and do they give ~same predictions?
# Extracting Weights matrices 
WW = list()
WW[[1]] = matrix(Neunet$result.matrix[4:(3+hidden[1]*2),1], ncol = 2, byrow = TRUE)
WW[[2]] = matrix(Neunet$result.matrix[(4+hidden[1]*2):(4+hidden[1]*2 + hidden[1]),1], nrow = 1)

# The predictions 
y_hat = applyFFNN(x_train, WW, sigmoid)
y_pred = applyFFNN(x_test, WW, sigmoid)

# Plotting 
plot(x_train, y_train, xlab = "", ylab = "", main = "neunet Predictions")
#lines(x_train, y_hat, col = "red") #, type = "p")
lines(x_test, y_pred, col = "red")

plot(x_train, y_train, xlab = "", ylab = "", main = "My Predictions")
#lines(x_train, as.vector(applyFFNN(x_train, Wmat, sigmoid)), col = "red") #, type = "p")
lines(x_test, as.vector(applyFFNN(x_test, Wmat, sigmoid)), col = "red") 

# GOOD! 

# (2) Comparing training SSE after approximately the same number of steps 
Neunet$result.matrix["steps",]     # ~20,000 steps
Neunet$result.matrix["error",]     #  10.21606 is train SSE
sum((y_train - applyFFNN(x_train, WW, sigmoid))^2)/2    # neuralnet: 10.21606 
sum((y_train - applyFFNN(x_train, Wmat, sigmoid))^2)/2  # myFFNN:    11.85833

# GOOD! 

# (3) Comparing test SSE 
sum((y_test - applyFFNN(x_test, WW, sigmoid))^2)/2    # neuralnet: 9.519012
sum((y_test - applyFFNN(x_test, Wmat, sigmoid))^2)/2  # myFFNN:    7.828971

# GOOD! 

# (4) Is the threshold condition satisfied in myFFNN? 
Neunet$result.matrix["reached.threshold",]  
SSE[20000] / SSE[19999] - 1

# GOOD!
