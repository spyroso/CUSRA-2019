### MODEL ONE ### 
set.seed(20191)
nbStates = 2
nbActions = 2 

# 2 states
statesONE = function(stock){
  ret = diff(stock)/ stock[-length(stock)]
  States = c(1, cut(ret, breaks = c(-10000, 0, 10000))) # set the first state to 1 
}
  
  
# 2 actions 
A = rbind(0:1, 1:0)
#A[1,] - invest all in Bond
#A[2,] - invest all in Stock

# initial policy and Q estimate 
initPolicy = matrix(data = 0.5, nrow = 2, ncol = 2)
rownames(initPolicy) = c("s1", "s2")
colnames(initPolicy) = c("a1", "a2")
initQ = initPolicy * 0 

# Q learning parameters
epsilon = 0.4
stepsize = 0.01 

# Q-Learning Algorithm 
QLearning = function(aPTF, stepsize, initQ, initPolicy, epsilon){
  # Initializing Estimates 
  Qestim = initQ
  Policy = initPolicy
  
  # Initialization 
  State = statesONE(aPTF[,1])
  Action = 1 # everyting in the bond 
  Reward = 0000 # initial investment 
  Value = 10000 
  n = c(0, Value / aPTF[1,2]) 
  
  # Applying Q-Learning with e-greedy policy 
  for (t in 2:nrow(aPTF)){
    # updating portfolio
    Action[t] =  sample(nbActions, 1, prob = Policy[State[t],])
    n = rebalance(n, aPTF[t,], A[Action[t],] )
    # Updating Q 
    Value[t] = as.double(n %*% aPTF[t,])
    Reward[t] = Value[t] / Value[t-1] - 1 
    Qestim[State[t-1], Action[t-1]] = Qestim[State[t-1], Action[t-1]] + stepsize*(Reward[t] + max(Qestim[State[t],]) - Qestim[State[t-1], Action[t-1]])
    # updating policy 
    Policy = EGPolicy(epsilon, Qestim)  
  }
  return(list(Qestim, Reward, Action, Value))
}

# TRAINING WITH 4.5 YEARS OF TRAINING DATA 
Q = initQ
P = initPolicy
PTF_TRAIN = QLearning(train, stepsize, initQ, initPolicy, epsilon)  

# Applying Q-Learning repeatedly 
for(i in 1:500){
  PTF_TRAIN = QLearning(train, stepsize, Q, P, epsilon)
  Q = PTF_TRAIN[[1]]
  P = EGPolicy(epsilon, Q)
  epsilon = epsilon * 0.995
  print(i)
  #plot every 100 graphs
  if(i %% 100 == 0){
    plot(PTF_TRAIN[[4]], type = "l", main = paste("Model One \n Nb iterations:",i), ylab = "Reward")
  }
}

# TESTING ON NEW DATA
MODEL_1 = QLearning(test, 0.001, Q, P, 0)
plot(MODEL_1[[4]], type = "l", main = "Model I Performance", ylab = "PTF Value ($)", col = "blue",  ylim = c(6500, 12000))
plot(test[,1], type = "l", main = "Stock performance \n during the same period", ylab = "TSLA ($)", col = "red")

# Avg. ptf composition 
sum(MODEL_1[[3]] == 1) / 252

# mean and volatility of returns 
mean(MODEL_1[[2]])
sd(MODEL_1[[2]])

# cummulative return 
MODEL_1[[4]][252] / 10000 - 1  # ptf
test[252,1] / test[1,1] - 1   # stock 

# Sharpe Ratio 
sharpe(MODEL_1[[2]], r = 0.02)


