#### PLOTS #### 
plot(STOCK_ONLY, type = "l", col = "red4", main = "Model Comparison", ylab = "Portfolio Value ($)")
lines(BOND_ONLY, type = "l", col = "deepskyblue")
lines(MODEL_1[[4]], type = "l", col = "green2", lty = 3)
lines(MODEL_2[[4]], type = "l", col = "deeppink")
lines(MODEL_3[[4]], type = "l", col = "purple")
lines(MODEL_4[[4]], type = "l", col = "orange")
legend(20, 8050, legend=c("Stock", "Bond", "Model I", "Model II", "Model III", "Model IV"), col=c("red4", "deepskyblue", "green2", "deeppink", "purple", "orange"),
       lty=1:1, cex=0.5)
