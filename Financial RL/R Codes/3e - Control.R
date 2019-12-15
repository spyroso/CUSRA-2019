#### STATIC POSITIONS --- for reference ### 
# Hold bond entire time 
BOND_ONLY = 10000*(1+0.02/252)^(0:251)

# Hold stock entire time 
n = c(10000 / test[1,1], 0)
STOCK_ONLY = 0
for(t in 1:252){
  STOCK_ONLY[t] = as.double(n %*% test[t,])
}