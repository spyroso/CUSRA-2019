######################### QLearning ###################################################
###  trains an agent using the Q-Learning algotithm                              ######                           
#######################################################################################
# The inputs:                                                                    ######
#   - aPTF: a mxn portfolio of asset prices where m is Time, n is Nb Assets      ######  
#   - mem: e in (0,1). A random action is chosen 100*(1-e)% of the time          ######                     
#   - stepsize: e in (0,1). A random action is chosen 100*(1-e)% of the time     ######                     
#   - initQ: e in (0,1). A random action is chosen 100*(1-e)% of the time        ######                     
#   - initPolicy: e in (0,1). A random action is chosen 100*(1-e)% of the time   ######                     
#   - epsilon: e in (0,1). A random action is chosen 100*(1-e)% of the time      ######                     
#######################################################################################
QLearning = function(aPTF, mem, stepsize, initQ, initPolicy, epsilon){
  # Calculating States 
  States = get3States(aPTF[,1], mem)
  #States = get3States(aPTF[,1], mem)
  
  # Initializing Estimates 
  Qestim = initQ
  Policy = initPolicy
  
  # Initial PTF 
  State = States[1]
  Action = 1 # everyting in the stock 
  Reward = 10000 # initial investment 
  n = c(Reward / PTF[1,1], 0) 
  
  # Applying Q-Learning with e-greedy policy 
  for (t in 2:(nrow(aPTF) - 1)){
    
    State[t] = States[t]
    Action[t] =  sample(nbActions, 1, prob = Policy[State[t],])
    Reward[t-1] = as.double(n %*% PTF[t,]) 
    
    Qestim[State[t-1], Action[t-1]] = Qestim[State[t-1], Action[t-1]] + stepsize*(Reward[t-1] + max(Qestim[State[t],]) - Qestim[State[t-1], Action[t-1]])
    
    # updating portfolio
    n = updatePTF(n, PTF[t,], A[Action[t],] )
    
    # updating policy 
    Policy = EGPolicy(epsilon, Qestim)  
    #print(Action[t])
    #print(Policy)
    #print(n)
  }
  
  return(list(Qestim, Reward, Action))
  
}


######################### EGPolicy ####################################################
###   calculates the epsilon-greedy policy based on value function Q and a given e  ###                           
#######################################################################################
# The inputs:                                                                    ######
#   - e: Epsilon. Random action is chosen 100*(1-e)% of the time where e in (0,1). ####                    
#   - Q: the (estimated) value function                                          ######
#######################################################################################
EGPolicy = function(e, Q){
  nbActions = ncol(Q)
  nbStates = nrow(Q)
  ImprovedEGPolicy = matrix(data = e/nbActions, nrow = nbStates, ncol = nbActions)
  GreedyPolicy = max.col(Q)
  for(i in 1:nbStates){
    ImprovedEGPolicy[i, GreedyPolicy[i]] = 1 - e + ImprovedEGPolicy[i, GreedyPolicy[i]]
  }
  return(ImprovedEGPolicy)
}


######################### pftVAL #################################################
###    This function calculates the portfolio value                              #
##################################################################################
# The inputs:                                                                    #
#   - n: vector containing quantity of each stock                                #
#   - s: vector containing the current price of each stock                       #
##################################################################################
ptfVAL = function(n, s){
  as.double(t(n) %*% s)
}


######################### rebalance #############################################
###### This function rebalances a given portfolio to a user-specified one. ######
#################################################################################
# The Inputs:                                                                   #
#   - n: vector containing quantity of each stock                               #
#   - s: vector containing the current price of each stock                      #
#   - wts: the desired portfolio allocation amounts                             #
#################################################################################
rebalance = function(n, s, wts){
  as.double(t(n) %*% s) * wts / s
}


######################### investRank ############################################
###### This function rebalances a given portfolio by ranking the returns.  ######
#################################################################################
# The Inputs:                                                                   #
#   - n: vector containing quantity of each stock                               #
#   - s: vector containing the current price of each stock                      #
#   - r: the predicted return of each stock                                     #
#   - wts: * if 1 --> stocks will be purchased equally                          #
#          * if 2 --> purchase more of the stocks with higher projected return  #
#   - change: How much stock to sell (%)                                        #
#################################################################################
investRank = function(n, s, r, wts, change){
  # Looking at the portfilio 
  ptfVal = t(n) %*% s
  theBest = rank(r, ties.method = "first")
  toSell =  (theBest <= floor(length(n)/2))
  toBuy = !toSell
  
  # Selling the 'bad' stocks
  proceeds = sum(n[toSell] * s[toSell] * change)
  n[toSell] = n[toSell] * (1 - change)
  
  # Buying the 'good' stocks according to wts vector 
  if(wts == 1){wts = theBest[toBuy] / sum(theBest[toBuy])}
  else if(wts == 2){wts = theBest[toBuy]^2/sum(theBest[toBuy]^2)}
  else{wts = 1 / rep(sum(toBuy), sum(toBuy))} #evenly distributed 
  
  # Updating good stock count 
  n[toBuy] = n[toBuy] + proceeds * wts / s[toBuy]
  
  # Return output 
  if (abs(ptfVal - t(n) %*% s) < 0.1){ return(n) }
  else {return("ERROR")}
  #return(list(n, wts, proceeds, ptfVal, t(n) %*% s))
}



######################### investRandom ################################################
###### This function randomly rebalances a given portfolio. Used as baseline. ####
##################################################################################
# The Inputs:                                                                    #
#   - n: vector containing quantity of each stock                                #
#   - s: vector containing the current price of each stock                       #
#   - r: the predicted return of each stock                                      #
#   - wts: * if 1 --> stocks will be purchased equally                           #
#          * if 2 --> purchase more of the stocks with higher projected return   #
#   - change: How much stock to sell (%)                                         #
##################################################################################
investRandom = function(n, s, r, wts, change){
  # Randomly choosing which to buy/sell
  if (runif(1) > 0.5){
    toSell = 1
    toBuy = 2
  } else {
    toSell = 2
    toBuy = 1
  }
  
  # Selling the 'bad' stock
  proceeds = sum(n[toSell] * s[toSell] * change)
  n[toSell] = n[toSell] * (1 - change)
  
  # Buying the 'good' stock
  n[toBuy] = n[toBuy] + proceeds / s[toBuy]
  
  # Return output 
  return(n)
}



######################### predictStock ###########################################
###    predict the next stock price based on k previous stock prices             #
##################################################################################
# The inputs:                                                                    #
#   - k: memory term (consider the k previous days when training NN )            #
#   - y: vector containing the current price of each stock                       #
##################################################################################
predictStock = function(k, y){
  df = cbind(1:length(y), y)
  colnames(df) = c("x","y")
  y_hat = rep(0, length(y)-k)
  
  for(i in (k+1):length(y)){
    ### Updating Neural Network 
    myNN = nnet(y ~ x, data = df[(i-k):(i-1),], size = 15, linout = TRUE, decay = 0.15)
    ### Making Prediction (assume min stock price is $1)
    y_hat[i-k] =  max(1, predict(myNN, data.frame(x = i) ))
  }
  
  returns = diff(y_hat) / y_hat[-length(y_hat)]
  volatility = sd(returns)
  sharpeRatio = (returns - 0.02)/volatility
  
  df = data.frame(y[(k+1):length(y)], y_hat, c(returns,NA))
  colnames(df) = c("trueS","predS", "predR")
  return(df)
}

######################### predictVolNN ###########################################
###    predict the next stock price based on k previous stock prices using ANN   #
##################################################################################
# The inputs:                                                                    #
#   - k: memory term (consider the k previous days when training NN )            #
#   - y: vector containing the current price of each stock                       #
##################################################################################
predictVolNN = function(k, y){
  df = cbind(1:length(y), y)
  colnames(df) = c("x","y")
  y_hat = rep(0, length(y)-k)
  
  for(i in (k+1):length(y)){
    ### Updating Neural Network 
    myNN = nnet(y ~ x, data = df[(i-k):(i-1),], size = 15, linout = TRUE, decay = 0.15)
    ### Making Prediction (assume min stock price is $1)
    y_hat[i-k] =  max(1, predict(myNN, data.frame(x = i) ))
  }
  
  returns = diff(y_hat) / y_hat[-length(y_hat)]
  volatility = sd(returns)
  sharpeRatio = (returns - 0.02)/volatility
  
  df = data.frame(y[(k+1):length(y)], y_hat, c(returns,NA))
  colnames(df) = c("trueS","predS", "predR")
  return(df)
}



######################### The Inputs #############################################
#   - n: vector containing quantity of each stock                                #
#   - s: vector containing the current price of each stock                       #
#   - r: the predicted return of each stock                                      #
#   - wts: * if 1 --> stocks will be purchased equally                           #
#          * if 2 --> purchase more of the stocks with higher projected return   #
#   - change: How much stock to sell (%)                                         #
##################################################################################
investM = function(n, s, r, wts, change){
  # Randomly choosing which to buy/sell
  if (runif(1) > 0.5){
    toSell = 1
    toBuy = 2
  } else {
    toSell = 2
    toBuy = 1
  }
  # Selling the 'bad' stock
  proceeds = sum(n[toSell] * s[toSell] * change)
  n[toSell] = n[toSell] * (1 - change)
  # Buying the 'good' stock
  n[toBuy] = n[toBuy] + proceeds / s[toBuy]
  # Return output 
  return(n)
}











