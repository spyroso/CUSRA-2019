### MODEL TWO ### 
set.seed(20192)
mem = 5 
nbStates = 3

# State Calculator 
getRetStates = function(Stock, mem){
  ret = diff(Stock) / Stock[-length(Stock)]
  # if not enough previous return 
  state = 1
  for (t in 2:(mem-1)){
    state[t] = cut(rank(ret[1:t]), breaks = c(0, mem/3, 2*mem/3, mem+1))[t]
  }
  # the rest of the data
  for (t in mem:length(ret)){
    state[t] = cut(rank(ret[(t-mem+1):t]), breaks = c(0, mem/3, 2*mem/3, mem+1))[mem]
  }
  return(c(1, state))
}


# THE STATES 
trainSTATES = getRetStates(train[,1], mem)
testSTATES = getRetStates(test[,1], mem)
hist(testSTATES) 

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

# Q-Learning parameters
epsilon = 0.4
stepsize= 0.01 


QLearning = function(aPTF, STATES, mem, stepsize, initQ, initPolicy, epsilon){
  # Initializing Estimates 
  Qestim = initQ
  Policy = initPolicy
  
  # Initial PTF 
  Value = 10000 # initial investment 
  Action = 1 # everyting in the bond
  Reward = 0000 
  n = c(0, Value / aPTF[1,2]) 
  State = STATES
  
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

# TRAINING WITH 2 YEARS OF TRAINING DATA 
Q = initQ
P = initPolicy
PTF_TRAIN = QLearning(train, trainSTATES, mem, stepsize, initQ, initPolicy, epsilon)  

for(i in 1:500){
  PTF_TRAIN = QLearning(train, trainSTATES, mem, stepsize, Q, P, epsilon)
  Q = PTF_TRAIN[[1]]
  P = EGPolicy(epsilon, Q)
  epsilon = epsilon * 0.995
  
  print(i)
  # plot every 100 graphs 
  if(i %% 100 == 0){
    plot(PTF_TRAIN[[2]], type = "l", main = paste("Nb iterations:",i), ylab = "PTF Value (10k @ t0)")
  }
  
}


# TEST DATA
MODEL_2 = QLearning(test, testSTATES, mem, 0.0001, Q, P, 0.0)
plot(MODEL_2[[4]], type = "l", main = "Model II Performance", ylab = "PTF Value ($)", col = "blue",  ylim = c(6500, 12000))
plot(test[,1], type = "l", main = "TSLA Performance", ylab = "TSLA ($)", col = "red")


# Average composition of ptf 
sum(MODEL_2[[3]] == 1)* A[1,1] + 
sum(MODEL_2[[3]] == 2)* A[2,1] + 
sum(MODEL_2[[3]] == 3)* A[3,1] + 
sum(MODEL_2[[3]] == 4)* A[4,1] + 
sum(MODEL_2[[3]] == 5)* A[5,1] + 
sum(MODEL_2[[3]] == 6)


# mean and volatility of returns 
mean(MODEL_2[[2]])
sd(MODEL_2[[2]])

# cummulative mean 
MODEL_2[[4]][251] / 10000 - 1  # ptf
test[252,1] / test[1,1] -1   # stock 

# Sharpe Ratio 
sharpe(MODEL_2[[2]], r = 0.02)








sharpe(MODEL_2[[2]], r = 0.02)
library('tseries')
sharpe(MODEL_2[[2]], r = 0.02)
sharpe(test[,1], r = 0.02)

cor(test[-1,1], MODEL_2[[4]])


# 
# plot(PTF_TRAIN[[2]][-1] / PTF_TRAIN[[2]][1] - 1 , type = "l")
# lines(PTF[-1,1]/PTF[1,1] - 1, type = "l", col = "red")
# lines(bond[1:252]/100 -1, col = "green")
# 
# # plotting with colour 
# plotActions = matrix(data = NA, ncol = nbActions, nrow = 250)
# someColours = c("red", "blue", "green", "black", "grey", "purple")
# 
# for (i in 1:nbActions){
#   plotActions[,i] = (PTF_TRAIN[[3]] %in% i)[-(length(PTF_TRAIN[[3]]))] * PTF_TRAIN[[2]]
# }
# 
# plot(plotActions[,1], col = someColours[1], ylim = c(8000, 12000))
# for (i in 2:nbActions){
#   points(plotActions[,i], col = someColours[i])
# }
# 
