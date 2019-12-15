#### MODEL III ####
set.seed(20193)

#States calculator (mem of them)
getMemStates = function(Stock, mem){
  ret = diff(Stock) / Stock[-length(Stock)]
  # When there is less than mem observations.... 
  state = ifelse(Stock[2] > Stock[1], 2, 1)
  for (t in 2:(mem-1)){
    state[t] = round(rank(ret[1:t])[t])
  }
  # For the remaining observations....  
  for (t in mem:length(ret)){
    state[t] = round(rank(ret[(t-mem+1):t])[mem])
  }
  
  return(c(1,state))
}


# THE STATES 
mem = 6
nbStates = mem
States = getMemStates(TSLA, nbStates)
hist(States)
trainSTATES = getMemStates(train[,1], mem)
testSTATES = getMemStates(test[,1], mem)

# THE ACTIONS 
nbActions = 6
A = cbind(seq(0,1,1/(nbActions-1)), 1-seq(0,1,1/(nbActions-1)))
A[1,] = c(0.01, 0.99)
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
stepsize = 0.01 

QLearning = function(aPTF, STATES, mem, stepsize, initQ, initPolicy, epsilon){
  # Initializing Estimates 
  Qestim = initQ
  Policy = initPolicy
  
  # Initial PTF 
  State = STATES 
  Action = 1 # everyting in the stock 
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
    Reward[t] = (Value[t] / Value[t-1] - 1) - 0.5 * ifelse(t > 10, sd(diff(Value[(t-mem):t])/Value[(t-mem):(t-1)]), 0.05)^2
    Qestim[State[t-1], Action[t-1]] = Qestim[State[t-1], Action[t-1]] + stepsize*(Reward[t] + max(Qestim[State[t],]) - Qestim[State[t-1], Action[t-1]])
    # updating policy 
    Policy = EGPolicy(epsilon, Qestim)  
  }
  return(list(Qestim, Reward, Action, Value))
}

#Q-Learning Algorithm 
# QLearning = function(aPTF, mem, stepsize, initQ, initPolicy, epsilon){
#   # Calculating States 
#   States = getMemStates(aPTF[,1], mem)
#   
#   # Initializing Estimates 
#   Qestim = initQ
#   Policy = initPolicy
#   
#   # Initial PTF 
#   State = c(1,States[1])
#   Action = 1 # everyting in the stock 
#   Reward = 10000 # initial investment 
#   n = c(Reward / PTF[1,1], 0) 
#   
#   # Applying Q-Learning with e-greedy policy 
#   for (t in 2:(nrow(aPTF) - 1)){
#     
#     State[t] = States[t]
#     Action[t] =  sample(nbActions, 1, prob = Policy[State[t],])
#     Reward[t-1] = as.double(n %*% PTF[t,]) 
#     
#     Qestim[State[t-1], Action[t-1]] = Qestim[State[t-1], Action[t-1]] + stepsize*(Reward[t-1] + max(Qestim[State[t],]) - Qestim[State[t-1], Action[t-1]])
#     
#     # updating portfolio
#     n = updatePTF(n, PTF[t,], A[Action[t],] )
#     
#     # updating policy 
#     Policy = EGPolicy(epsilon, Qestim)  
#     #print(Action[t])
#     #print(Policy)
#     #print(n)
#   }
#   
#   return(list(Qestim, Reward, Action))
#   
# }


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
    plot(PTF_TRAIN[[2]], type = "l", main = paste("Nb iterations:",i), ylab = "Reward")
  }
  
}

# TESTING ON NEW DATA
MODEL_3 = QLearning(test, testSTATES, mem, 0.001, Q, P, 0)
plot(MODEL_3[[4]], type = "l", main = "Model III Performance", ylab = "PTF Value ($)", col = "blue", ylim = c(6500, 12000))
plot(test[,1], type = "l", main = "Stock performance \n during the same period", ylab = "TSLA ($)", col = "red")



sum(MODEL_3[[3]] == 1) * 0.01 + sum(MODEL_3[[3]] == 2) * 0.2 + sum(MODEL_3[[3]] == 3) * 0.4 + sum(MODEL_3[[3]] == 4) * 0.6 + sum(MODEL_3[[3]] == 5) * 0.8 + sum(MODEL_3[[3]] == 6) * 1   


MODEL_3[[4]][251] / MODEL_3[[4]][1] - 1


library('tseries')
sharpe(diff(MODEL_3[[4]]), r = 0.02)
sharpe(test[,1], r = 0.02)

sd(diff(MODEL_3[[4]]) / MODEL_3[[4]][-251])
mean(diff(MODEL_3[[2]]) / MODEL_3[[2]][-1]) - 0.02*199/252

plot(QL[[2]][-1] / QL[[2]][1] - 1 , type = "l")
lines(PTF[-1,1]/PTF[1,1] - 1, type = "l", col = "red")
lines(bond[1:252]/100 -1, col = "green")

# plotting with colour 
plotActions = matrix(data = NA, ncol = nbActions, nrow = 250)
someColours = c("red", "blue", "green", "black", "grey", "purple")

for (i in 1:nbActions){
  plotActions[,i] = (QL[[3]] %in% i)[-(timeHorizon-1)] * QL[[2]]
}

plot(plotActions[,1], col = someColours[1], ylim = c(8000, 12000))
for (i in 2:nbActions){
  points(plotActions[,i], col = someColours[i])
}


