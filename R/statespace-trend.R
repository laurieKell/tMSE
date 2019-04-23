# if (FALSE){
#   
# sink("cpueAVG.jags")
# cat("
#     model {
#     
#     # Prior specifications 
#     eps <- 0.0000000000001 # small constant 
#     
#     iq[1] ~ dgamma(1000,1000)
#     q[1] <- pow(iq[1],-1)
#     logq[1] <- log(1)
#     for(i in 2:nI){
#     iq[i] ~ dgamma(0.001,0.001)
#     q[i] <- pow(iq[i],-1)
#     logq[i] <- log(q[i])
#     }
#   ")
# 
# if(sigma.proc==TRUE){
#   cat("
#       # Process variance
#       isigma2 <- isigma2.est 
#       sigma2 <- pow(isigma2,-1)
#       sigma <- sqrt(sigma2)
#       fakesigma.fixed <- sigma.fixed # Prevent unused variable error msg 
#       ",append=TRUE) 
#   }else{ cat(" 
#       isigma2 <- pow(sigma.fixed+eps,-2) 
#       sigma2 <- pow(isigma2,-1)
#       sigma <- sqrt(sigma2)
#       ",append=TRUE)}
# 
# if(sigma.est==TRUE){
#   cat("
#       # Obsevation variance
#       # Observation error
#       itau2~ dgamma(0.001,0.001)
#       tau2 <- 1/itau2
#       
#       
#       for(i in 1:nI)
#       {
#       for(t in 1:N)
#       {
#       var.obs[t,i] <- SE2[t,i]+tau2
#       ivar.obs[t,i] <- 1/var.obs[t,i]
#       # note total observation error (TOE) 
#       TOE[t,i] <- sqrt(var.obs[t,i])
#       
#       }}
#       ",append=TRUE) 
# }else{ cat(" 
#  # Obsevation variance
#            # Observation error
#            itau2~ dgamma(2,2)
#            tau2 <- 1/itau2
#            
#            
#            for(i in 1:nI)
#            {
#            for(t in 1:N)
#            {
#            var.obs[t,i] <- SE2[t,i] # drop tau2
#            fake.tau[t,i] <- tau2
#            
#            ivar.obs[t,i] <- 1/var.obs[t,i]
#            # note total observation error (TOE) 
#            TOE[t,i] <- sqrt(var.obs[t,i])
#            
#            }}
#            
#            ",append=TRUE)}
# 
# # Run rest of code 
# cat(" 
#     # Process variance prior
#     isigma2.est ~ dgamma(0.001,0.001)
#     
#     
#     # Priors and constraints
#     logY.est[1] ~ dnorm(logY1, 1) # Prior for initial population size
#     
#     mean.r ~ dnorm(1, 0.001) # Prior for mean growth rate
#     
#     # Likelihood
#     # State process
#     for (t in 1:(N-1)){
#     r[t] ~ dnorm(mean.r, isigma2)
#     logY.est[t+1] <- logY.est[t] + r[t] }
#     
#     # Observation process
#     for (t in 1:N) {
#     for(i in 1:nI){
#     y[t,i] ~ dnorm(logY.est[t]+logq[i], ivar.obs[t,i])
#     }}
#     
#     # Population sizes on real scale
#     for (t in 1:N) {
#     Y.est[t] <- exp(logY.est[t])
#     }
#     
# } 
#     ",fill = TRUE)
# sink()
# }
# 
