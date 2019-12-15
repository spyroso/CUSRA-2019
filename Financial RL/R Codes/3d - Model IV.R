##### MODEL IV #####
set.seed(2019+4)

######################### predictStock ##########################################
###    predict the next stock price based on mem previous stock prices          #
#################################################################################
# The inputs:                                                                   #
#   - mem: memory term (consider the mem previous days when training NN )       #
#   - stock: vector containing the current price of each stock                  #
#   - neurons: the number of hidden units in the hidden layer                   #
#################################################################################
getNNState  = function(mem, aPTF, neurons, trainMODE){
  # training mode only uses train data (starts from scratch)
  if(trainMODE){
    stock = train[,1]
    df = cbind(1:length(stock), stock)
    colnames(df) = c("x","stock")
    y_hat = stock * 0 + 1 
  
    for(i in (mem+1):length(stock)){
      ### Updating Neural Network 
      myNN = nnet(stock ~ x, data = df[(i-mem):(i-1),], size = neurons, linout = TRUE, decay = 0.15)
      ### Making Prediction 
      y_hat[i] = predict(myNN, data.frame(x = i)) 
    }
    
    # predicted returns 
    pred_ret = y_hat / stock - 1 
    
    # Calculating states 
    state = rep(6, mem) # set the first mem states to 6 so that Q for test data is still fine 
    for(t in (mem+1):length(stock)){
      if      (pred_ret[t] < -0.02){state[t] = 1} 
      else if (pred_ret[t] < 0.00){state[t] = 2} 
      else if (pred_ret[t] < 0.02){state[t] = 3}
      else if (pred_ret[t] < 0.04){state[t] = 4} 
      else                        {state[t] = 5}
    }
    return(state)
  }
  
  
  # Test mode uses some of the past training data to predict future prices 
  else {
    stock = aPTF[(1008-mem):nrow(aPTF),1]
    df = cbind(1:length(stock), stock)
    colnames(df) = c("x","stock")
    y_hat = 1:252 * 0 + 1
    
    for(i in 1:252){
      ### Updating Neural Network 
      myNN = nnet(stock ~ x, data = df[i:(i+mem-1),], size = neurons, linout = TRUE, decay = 0.15)
      ### Making Prediction (assume min stock price is $1)
      y_hat[i] = predict(myNN, data.frame(x = (i+mem))) 
    }
    
    # predicted returns 
    pred_ret = y_hat / test[,1] - 1 
    
    # Calculating states 
    for(t in 1:length(pred_ret)){
      if(pred_ret[t] < -0.01){
        state[t] = 1
      } else if (pred_ret[t] < 0){
        state[t] = 2
      } else if (pred_ret[t] < 0.01){
        state[t] = 3
      } else if (pred_ret[t] < 0.03){
        state[t] = 4
      } else {
        state[t] = 5
      }
    }
    
    return(state)
  }
}


# Hyperparameters & Q-Learning parameters
hidden = 8
mem = 8
epsilon = 0.4
stepsize = 0.01 

# THE STATES 
nbStates = 5 + 1 # 1 for training state 
trainSTATES = getNNState(mem, PTF, hidden, TRUE)
testSTATES = getNNState(mem, PTF, hidden, FALSE)


# THE ACTIONS 
nbActions = 6
A = cbind(seq(0,1,1/(nbActions-1)), 1-seq(0,1,1/(nbActions-1)))
rownames(A) = 1:nbActions
colnames(A) = c("S", "B")
A

# initial policy and Q estimate 
initPolicy = matrix(data = 1/nbActions, nrow = nbStates, ncol = nbActions)
rownames(initPolicy) = paste("s", 1:nbStates, sep = "")
colnames(initPolicy) = paste("a", 1:nbActions, sep = "")
initQ = initPolicy * 0 
initPolicy
initQ

QLearning = function(aPTF, mem, stepsize, initQ, initPolicy, epsilon, STATES){
  # Initializing Estimates 
  Qestim = initQ
  Policy = initPolicy
  
  # Initial PTF 
  State = STATES
  Reward = 0000 # initial investment 
  Value = 10000 
  
  # In test mode we make use of our initial prediction 
  if (State[1] == 6){
    Action = 1
    n = c(0, Value / aPTF[1,2]) 
  } else {
    Action = sample(nbActions, 1, prob = Policy[State[1],])
    n = c(0, Value / aPTF[1,2]) 
    n = rebalance(n, aPTF[1,], A[Action[1],])
  }

  # Applying Q-Learning with e-greedy policy 
  for (t in 2:(nrow(aPTF) )){
    # updating portfolio
    Action[t] =  sample(nbActions, 1, prob = Policy[State[t],])
    n = rebalance(n, aPTF[t,], A[Action[t],] )
    # Updating Q 
    Value[t] = as.double(n %*% aPTF[t,])
    Reward[t] = (Value[t] - Value[t-1]) - 0.5 * ifelse(t > mem, sd(Value[(t-mem):t]), 0)^2
    Qestim[State[t-1], Action[t-1]] = Qestim[State[t-1], Action[t-1]] + stepsize*(Reward[t] + max(Qestim[State[t],]) - Qestim[State[t-1], Action[t-1]])
    # updating policy 
    Policy = EGPolicy(epsilon, Qestim)  
  }
  return(list(Qestim, Reward, Action, Value))
}

# TRAINING WITH 4 YEARS OF TRAINING DATA 
Q = initQ
P = initPolicy
PTF_TRAIN = QLearning(train, mem, stepsize, initQ, initPolicy, epsilon, trainSTATES)  

for(i in 1:500){
  PTF_TRAIN = QLearning(train, mem, stepsize, Q, P, epsilon, trainSTATES)
  Q = PTF_TRAIN[[1]]
  P = EGPolicy(epsilon, Q)
  epsilon = epsilon * 0.995
  print(i)
  
  # plot every 100 graphs 
  if(i %% 100 == 0){
    plot(PTF_TRAIN[[4]], type = "l", main = paste("Nb iterations:",i), ylab = "Reward")
  }
  
}

# TESTING ON NEW DATA
MODEL_4 = QLearning(test, mem, 0.0001, Q, P, 0, testSTATES)
plot(MODEL_4[[4]], type = "l", main = "Model IV Performance", ylab = "PTF Value ($)", col = "blue",  ylim = c(6500, 12000))
plot(test[,1], type = "l", main = "Stock performance \n during the same period", ylab = "TSLA ($)", col = "red")







