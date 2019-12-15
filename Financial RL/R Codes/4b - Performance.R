#### PERFORMANCE OF VARIOUS MODELS #### 

# 1 -- CUMULATIVE RETURN 
CR_S = as.double(test[252, 1] / test[1, 1] - 1)
CR_B = 0.02
CR_1 = MODEL_1[[4]][252] / 10000 - 1; CR_1
CR_2 = MODEL_2[[4]][252] / 10000 - 1; CR_2
CR_3 = MODEL_3[[4]][252] / 10000 - 1; CR_3
CR_4 = MODEL_4[[4]][252] / 10000 - 1; CR_4

# 2 -- VOLATILITY OF RETURNS 
VR_S = sd(diff(test[,1]) / test[-252,1]) * sqrt(252); VR_S
VR_B = sd(diff(test[,2]) / test[-252,2]) * sqrt(252); VR_B
VR_1 = sd(diff(MODEL_1[[4]]) / MODEL_1[[4]][-252]) * sqrt(252); VR_1
VR_2 = sd(diff(MODEL_2[[4]]) / MODEL_2[[4]][-252]) * sqrt(252); VR_2
VR_3 = sd(diff(MODEL_3[[4]]) / MODEL_3[[4]][-252]) * sqrt(252); VR_3
VR_4 = sd(diff(MODEL_4[[4]]) / MODEL_4[[4]][-252]) * sqrt(252); VR_4

# 3 -- SHARPE RATIO 
library("tseries")
# SR_S = sharpe(diff(test[,1]) / test[-252,1], r = 0.02, scale = sqrt(252)); SR_S
# SR_B = sharpe(diff(test[,2]) / test[-252,2], r = 0.02, scale = sqrt(252)); SR_B
# SR_1 = NaN; SR_1
# SR_2 = sharpe(diff(MODEL_2[[4]]) / MODEL_2[[4]][-252], r = 0.02, scale = sqrt(252)); SR_2
# SR_3 = sharpe(diff(MODEL_3[[4]]) / MODEL_3[[4]][-252], r = 0.02, scale = sqrt(252)); SR_3
# SR_4 = sharpe(MODEL_4[[4]][-1] / MODEL_4[[4]][-252] - 1, r = 0.02, scale = sqrt(252)); SR_4

SR_S = (CR_S - 0.02) / VR_S; SR_S
SR_B = NaN; SR_B; 
SR_1 = NaN; SR_1; 
SR_2 = (CR_2 - 0.02) / VR_2; SR_2
SR_3 = (CR_3 - 0.02) / VR_3; SR_3
SR_4 = (CR_4 - 0.02) / VR_4; SR_4




# 4 -- AVERAGE PORTFOLIO COMPOSITION 


# 5 -- CORRELATION WITH STOCK 
COR_S = cor(test[,1], test[,1]); COR_S; 
COR_B = 0; COR_B
COR_1 = 0; COR_1; 
COR_2 = cor(test[,1], MODEL_2[[4]]); COR_2
COR_3 = cor(test[,1], MODEL_3[[4]]); COR_3
COR_4 = cor(test[,1], MODEL_4[[4]]); COR_4
