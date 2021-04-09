
##NUMERICAL EXAMPLE CHAPTER 3
## define model parameters
r0 <- 0.01
b <- 0.02
alpha <- 0.2
sigma <- 0.015 #regular sigma


## simulate short rate paths
n <- 1    # MC simulation trials
T <- 10   # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval

#simulate short rate
r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- alpha*(b-r[i-1,j])*dt + sigma*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 
t <- seq(0, T, dt)
matplot(t, r[,1:1], type="l", lty=1, main="Short Rate Path", ylab="Interest rate", xlab = "Time") 



bondprice <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice[t] <- exp(-a.vas-b.vas*r[t,1])
}

gamma <- 0.153
bondprice_tax <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice_tax[t] <- exp(-a.vas-b.vas*(-gamma*r[t,1] + r[t,1]))
}

#Plotting the asset value
t <- seq(0, T, dt) 
matplot(t, bondprice, type="l", lty=1, main="Asset value", ylab="value", xlab="Time", col='blue') 
matplot(t, bondprice_tax, type="l", lty=1, main="Asset value", ylab="value",xlab="Time", add=TRUE, col = 'red') 
legend("topleft", legend=c("Benefits + tax ", "Benefits"),
       col=c("red", "blue"), lty=1:1, cex=0.6)


#plotting the hedging strategy
h1 <- bondprice_tax/bondprice 
h0 <- rep(0,201)
one <- rep(1,201)
t <- seq(0, T, dt)
plot(t, h1, type="l", lty=1, main="Hedging strategy", ylab="", xlab="Time") 
lines(t,one, type='l', lty=2, col = "blue")




##NUMERICAL EXAMPLE CHAPTER 5
## function to find ZCB price using Vasicek model
## define model parameters
r0 <- 0.01
#hvis renten starter med at være negativ
#r0 <- -0.03*0.5
b <- 0.02
#b <- 0.02*1.5 #scaled up
#b <- 0.02*0.5 #scaled down
alpha <- 0.2
#alpha <- 0.2*1.5 #scaled up
#alpha <- 0.2*0.5 #scaled down
sigma <- 0.015 #regular sigma
#sigma <-0.015*1.5 #scaled up
#sigma <- 0.015*0.5 #scaled down


## simulate short rate paths
n <- 1    # MC simulation trials
T <- 10   # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval



#simulate short rate
r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- alpha*(b-r[i-1,j])*dt + sigma*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 
t <- seq(0, T, dt)
matplot(t, r[,1:1], type="l", lty=1, main="Short Rate Path", ylab="Interest rate", xlab = "Time") 


bondprice <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice[t] <- exp(-a.vas-b.vas*r[t,1])
}

gamma <- 0.153
bondprice_tax <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice_tax[t] <- exp(-a.vas-b.vas*(-gamma*r[t,1] + r[t,1]))
}

#Plotting the asset value
t <- seq(0, T, dt) 
matplot(t, bondprice, type="l", lty=1, main="Asset value", ylab="value", xlab="Time", col='blue') 
matplot(t, bondprice_tax, type="l", lty=1, main="Asset value", ylab="value",xlab="Time", add=TRUE, col = 'red') 
legend("topleft", legend=c("Benefits + tax ", "Benefits"),
       col=c("red", "blue"), lty=1:1, cex=0.6)


PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
palaktiv <- matrix(NA,5,10)
skyggekonto_overskud <- rep(NA,10)




#Calculating the PAL tax
PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
afkast <- matrix(NA,15,17)
skyggekonto_overskud <- rep(NA,10)


#Calculate the returns every year, recalling the 5 year projection rule 
afkast[5,1] <- 0
afkast[5,2] <- 0
afkast[5,3] <- 0
afkast[5,4] <- 0
afkast[5,5] <- 0

for (i in 1:10){
  if(PALt[i]<0){
    afkast[i+5,i+5] <-PALt[i]
  }
  else{ afkast[i+5,i+5] <- 0}
  afkast[i+5,17] <- PALt[i]
}
for (i in 1:10){
  if(PALt[i]>0){
   if(PALt[i] + afkast[i+4, i]<0){
     afkast[i+1,16] <- PALt[i] + afkast[i+4,i]
     afkast[i+5, i] <- 0
     afkast[i+5, i+1] <- afkast[i+4, i+1]
     afkast[i+5, i+2] <- afkast[i+4, i+2]
     afkast[i+5, i+3] <- afkast[i+4, i+3]
     afkast[i+5, i+4] <- afkast[i+4, i+4]
   }
    else afkast[i+5,i] <- 0
    if(PALt[i] + afkast[i+4, i] + afkast[i+4, i+1]>0){
      afkast[i+5, i+1] <- 0
      if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2]>0){
        afkast[i+5, i+2] <-0
        if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] >0){
          afkast[i+5, i+3] <-0
          if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4] >0){
            afkast[i+5, i+4] <-0
            afkast[i+5,16] <- PALt[i] + afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4]
          }
          else{
            afkast[i+5,i+4] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3] + afkast[i+4,i+4] 
            afkast[i+5,16] <- 0
          }
        }
        else {
          afkast[i+5,i+3] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3]
          afkast[i+5,i+4] <- afkast[i+4,i+4]
          afkast[i+5,16] <- 0
        }
      }
    else {
      afkast[i+5,i+2] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2]
      afkast[i+5,i+3] <- afkast[i+4,i+3]
      afkast[i+5,i+4] <- afkast[i+4,i+4]
      afkast[i+5,16] <- 0
     }
    }
    else {
      afkast[i+5,i+1] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1]
      afkast[i+5,i+2] <- afkast[i+4,i+2]
      afkast[i+5,i+3] <- afkast[i+4,i+3]
      afkast[i+5,i+4] <- afkast[i+4,i+4]
      afkast[i+5,16] <- 0
    }
  }
  else {
    afkast[i+5,i] <- 0 
    afkast[i+5,i+1] <- afkast[i+4,i+1]
    afkast[i+5,i+2] <- afkast[i+4,i+2]
    afkast[i+5,i+3] <- afkast[i+4,i+3]
    afkast[i+5,i+4] <- afkast[i+4,i+4]
    afkast[i+5,16] <- afkast[i+4,i]
  }
}



#Calculates paid PAL every year and shadow account surplus
for (i in 1:10){
betalt_skat[i] <- gamma*afkast[i+5,16]
skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
}


#COST FUNKTION
#define the insurance payment function
A_b <- function(t) ifelse(t>=10,1,0)
#Paid tax in the dt intervals 
t <- seq(0, T, dt)
A_t <- rep(0,200)
for (i in 1:10){
  A_t[20*i] <- betalt_skat[i]
}

#value of portfolio
V <- rep(NA,200)
for (i in 1:200){
  if(i<200)
  V[i] <- bondprice_tax[i]
  else 
    V[i] <- 0
}


#sum of trading gains
trading_gains_sum <- rep(NA,201)
trading_gains <- rep(NA,201)
trading_gains[1] <- 0
trading_gains_sum[1]<-0
for (t in 1:200){
  trading_gains[t+1] <- bondprice_tax[t]/bondprice[t]*(bondprice[(t+1)]-bondprice[t])
  trading_gains_sum[t+1] <- sum(trading_gains[1:(t+1)])
}


cost <- rep(NA,200)
for (t in 1:201){
  cost[t] <- V[t] - trading_gains_sum[t] + A_b(dt*t) + A_t[t] 
}

#Plotting the cost function
t <- seq(0, T, dt)
matplot(t, cost, type="l", lty=1, main="Cost function", ylab="value", xlab="Time") 


## plotting the cost function and interest rate in same plot

par(mfrow=c(1,1))
par(mar = c(5,5,2,5))
plot(t, r, type = "l", main="Cost function", ylab="Interest rate", xlab="Time", col= "lightgray", lwd=2)
par(new = T)
plot(t, cost, type="l", axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Cost function value')
#legend("topleft",
 #      legend=c("Cost function", "Short rate"),
  #     lty=c(1,1), pch=c(NA, NA), col=c("black", "grey"), cex=0.6, inset=c(-0.2,0))



#Calculating the integral under the cost function
#area under graph
#cost_int_1 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_1[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_1<- sum(cost_int_1)
#diff_1 <- cost_int_calc_1-cost[1]*10
#cost <- cost_int_calc_1 -diff_1
#p_1 <- (cost_int_calc_1 - cost  )/cost_int_calc_1*100




###### SIMULATION OF LOSS DISTRIBUTION ######

### USING VASICEK
## function to find ZCB price using Vasicek model
## define model parameters
r0 <- 0.01
b <- 0.02
alpha <- 0.2
sigma <- 0.015 #regular sigma



## simulate short rate paths
n <- 1    # MC simulation trials
T <- 10   # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval



loss <- rep(NA,10000)
p_l <- rep(NA,10000)
for (k in 1:10000){ 
#simulate short rate
r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- alpha*(b-r[i-1,j])*dt + sigma*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 



bondprice <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice[t] <- exp(-a.vas-b.vas*r[t,1])
}

gamma <- 0.153
bondprice_tax <- rep(NA,200)
for (t in 1:201){
  b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
  a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
  bondprice_tax[t] <- exp(-a.vas-b.vas*(-gamma*r[t,1] + r[t,1]))
}



PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
palaktiv <- matrix(NA,5,10)
skyggekonto_overskud <- rep(NA,10)


PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
afkast <- matrix(NA,15,17)
skyggekonto_overskud <- rep(NA,10)


afkast[5,1] <- 0
afkast[5,2] <- 0
afkast[5,3] <- 0
afkast[5,4] <- 0
afkast[5,5] <- 0

for (i in 1:10){
  if(PALt[i]<0){
    afkast[i+5,i+5] <-PALt[i]
  }
  else{ afkast[i+5,i+5] <- 0}
  afkast[i+5,17] <- PALt[i]
}
for (i in 1:10){
  if(PALt[i]>0){
    if(PALt[i] + afkast[i+4, i]<0){
      afkast[i+1,16] <- PALt[i] + afkast[i+4,i]
      afkast[i+5, i] <- 0
      afkast[i+5, i+1] <- afkast[i+4, i+1]
      afkast[i+5, i+2] <- afkast[i+4, i+2]
      afkast[i+5, i+3] <- afkast[i+4, i+3]
      afkast[i+5, i+4] <- afkast[i+4, i+4]
    }
    else afkast[i+5,i] <- 0
    if(PALt[i] + afkast[i+4, i] + afkast[i+4, i+1]>0){
      afkast[i+5, i+1] <- 0
      if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2]>0){
        afkast[i+5, i+2] <-0
        if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] >0){
          afkast[i+5, i+3] <-0
          if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4] >0){
            afkast[i+5, i+4] <-0
            afkast[i+5,16] <- PALt[i] + afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4]
          }
          else{
            afkast[i+5,i+4] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3] + afkast[i+4,i+4] 
            afkast[i+5,16] <- 0
          }
        }
        else {
          afkast[i+5,i+3] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3]
          afkast[i+5,i+4] <- afkast[i+4,i+4]
          afkast[i+5,16] <- 0
        }
      }
      else {
        afkast[i+5,i+2] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2]
        afkast[i+5,i+3] <- afkast[i+4,i+3]
        afkast[i+5,i+4] <- afkast[i+4,i+4]
        afkast[i+5,16] <- 0
      }
    }
    else {
      afkast[i+5,i+1] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1]
      afkast[i+5,i+2] <- afkast[i+4,i+2]
      afkast[i+5,i+3] <- afkast[i+4,i+3]
      afkast[i+5,i+4] <- afkast[i+4,i+4]
      afkast[i+5,16] <- 0
    }
  }
  else {
    afkast[i+5,i] <- 0 
    afkast[i+5,i+1] <- afkast[i+4,i+1]
    afkast[i+5,i+2] <- afkast[i+4,i+2]
    afkast[i+5,i+3] <- afkast[i+4,i+3]
    afkast[i+5,i+4] <- afkast[i+4,i+4]
    afkast[i+5,16] <- afkast[i+4,i]
  }
}


for (i in 1:10){
  betalt_skat[i] <- gamma*afkast[i+5,16]
  skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
}


#COST FUNKTION
A_b <- function(t) ifelse(t>=10,1,0)
t <- seq(0, T, dt)
A_t <- rep(0,200)
for (i in 1:10){
  A_t[20*i] <- betalt_skat[i]
}

#value of portfolio
V <- rep(NA,200)
for (i in 1:200){
  if(i<200)
    V[i] <- bondprice_tax[i]
  else 
    V[i] <- 0
}



#sum of trading gains
trading_gains_sum <- rep(NA,201)
trading_gains <- rep(NA,201)
trading_gains[1] <- 0
trading_gains_sum[1]<-0
for (t in 1:200){
  trading_gains[t+1] <- bondprice_tax[t]/bondprice[t]*(bondprice[(t+1)]-bondprice[t])
  trading_gains_sum[t+1] <- sum(trading_gains[1:(t+1)])
}


cost <- rep(NA,200)
for (t in 1:201){
  cost[t] <- V[t] - trading_gains_sum[t] + A_b(dt*t) + A_t[t] 
}

cost_int_loss <- rep(NA,200)
for (t in 1:200){
  cost_int_loss[t] <- (cost[(t)]*dt)
}
cost_int_calc_loss<- sum(cost_int_loss)
diff_loss <- cost_int_calc_loss-cost[1]*10
cost <- cost_int_calc_loss -diff_loss
p_loss <- (cost_int_calc_loss - cost )/cost_int_calc_loss*100

loss[k] <- diff_loss
p_l[k] <- p_loss
}

#Sorting the losses
loss_S <- sort(loss, decreasing = TRUE)
p_l_S <- sort(p_l, decreasing = TRUE)


#Calc VaR
alpha_v <- 0.99
VaR_L <- loss_S[floor(10000*(1-alpha_v))+1]
#Calc ES
OS_L <- loss_S[1:(floor(10000*(1-alpha_v))+1)]
ES_L <- 1/(floor(10000*(1-alpha_v))+1)*sum(OS_L)

#Calc VaR
VaR_p <- p_l_S[floor(10000*(1-alpha_v))+1]
#Calc ES
OS_p <- p_l_S[1:(floor(10000*(1-alpha_v))+1)]
ES_p <- 1/(floor(10000*(1-alpha_v))+1)*sum(OS_p)

#plotting, a negative loss means that the cost of cont. tax is more expensive and positive means discrete is more expensive
par(mfrow=c(1,1))
hist(loss_S, breaks = 100, main = 'Losses', xlab = 'Loss size', xlim = c(-0.21,0.15))
abline(v=VaR_L, col="red")
abline(v=ES_L, col="blue")

hist(p_l_S, breaks = 100, main = 'Losses %', xlab = 'Percentage loss size',  xlim = c(-2.5,2.5))
abline(v=VaR_p, col="red")
abline(v=ES_p, col="blue")

mean(loss_S)
mean(p_l_S)


### USING CIR
## define model parameters
r0 <- 0.01
b <- 0.02
alpha <- 0.2
sigma <- 0.015 #regular sigma


## simulate short rate paths
n <- 1    # MC simulation trials
T <- 10   # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval


loss <- rep(NA,10000)
p_l <- rep(NA,10000)
for (k in 1:10000){ 
  #simulate short rate
  r <- matrix(0,m+1,n)  # matrix to hold short rate paths
  r[1,] <- r0
  
  for(j in 1:n){
    for(i in 2:(m+1)){
      dr <- alpha*(b-r[i-1,j])*dt + sigma*sqrt(r[i-1,j]*dt)*rnorm(1,0,1)
      r[i,j] <- r[i-1,j] + dr
    }
  } 
  
  
  
  bondprice <- rep(NA,200)
  for (t in 1:201){
    b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
    a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
    bondprice[t] <- exp(-a.vas-b.vas*r[t,1])
  }
  
  gamma <- 0.153
  bondprice_tax <- rep(NA,200)
  for (t in 1:201){
    b.vas <- (1/alpha)*(1-exp(-(T-t*dt)*alpha)) 
    a.vas <- (b-sigma^2/(2*alpha^2))*((T-t*dt)-b.vas)+(sigma^2)/(4*alpha)*b.vas^2
    bondprice_tax[t] <- exp(-a.vas-b.vas*(-gamma*r[t,1] + r[t,1]))
  }
  
  
  
  PALt <- rep(NA,10)
  for (i in 1:10){
    PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
  }
  
  betalt_skat <- rep(NA,10)
  palaktiv <- matrix(NA,5,10)
  skyggekonto_overskud <- rep(NA,10)
  
  
  
  
  PALt <- rep(NA,10)
  for (i in 1:10){
    PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
  }
  
  betalt_skat <- rep(NA,10)
  afkast <- matrix(NA,15,17)
  skyggekonto_overskud <- rep(NA,10)
  
  
  afkast[5,1] <- 0
  afkast[5,2] <- 0
  afkast[5,3] <- 0
  afkast[5,4] <- 0
  afkast[5,5] <- 0
  
  for (i in 1:10){
    if(PALt[i]<0){
      afkast[i+5,i+5] <-PALt[i]
    }
    else{ afkast[i+5,i+5] <- 0}
    afkast[i+5,17] <- PALt[i]
  }
  for (i in 1:10){
    if(PALt[i]>0){
      if(PALt[i] + afkast[i+4, i]<0){
        afkast[i+1,16] <- PALt[i] + afkast[i+4,i]
        afkast[i+5, i] <- 0
        afkast[i+5, i+1] <- afkast[i+4, i+1]
        afkast[i+5, i+2] <- afkast[i+4, i+2]
        afkast[i+5, i+3] <- afkast[i+4, i+3]
        afkast[i+5, i+4] <- afkast[i+4, i+4]
      }
      else afkast[i+5,i] <- 0
      if(PALt[i] + afkast[i+4, i] + afkast[i+4, i+1]>0){
        afkast[i+5, i+1] <- 0
        if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2]>0){
          afkast[i+5, i+2] <-0
          if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] >0){
            afkast[i+5, i+3] <-0
            if(PALt[i]+afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4] >0){
              afkast[i+5, i+4] <-0
              afkast[i+5,16] <- PALt[i] + afkast[i+4, i] + afkast[i+4, i+1] + afkast[i+4, i+2] + afkast[i+4, i+3] + afkast[i+4, i+4]
            }
            else{
              afkast[i+5,i+4] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3] + afkast[i+4,i+4] 
              afkast[i+5,16] <- 0
            }
          }
          else {
            afkast[i+5,i+3] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2] + afkast[i+4,i+3]
            afkast[i+5,i+4] <- afkast[i+4,i+4]
            afkast[i+5,16] <- 0
          }
        }
        else {
          afkast[i+5,i+2] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1] + afkast[i+4,i+2]
          afkast[i+5,i+3] <- afkast[i+4,i+3]
          afkast[i+5,i+4] <- afkast[i+4,i+4]
          afkast[i+5,16] <- 0
        }
      }
      else {
        afkast[i+5,i+1] <- PALt[i] +afkast[i+4, i] + afkast[i+4,i+1]
        afkast[i+5,i+2] <- afkast[i+4,i+2]
        afkast[i+5,i+3] <- afkast[i+4,i+3]
        afkast[i+5,i+4] <- afkast[i+4,i+4]
        afkast[i+5,16] <- 0
      }
    }
    else {
      afkast[i+5,i] <- 0 
      afkast[i+5,i+1] <- afkast[i+4,i+1]
      afkast[i+5,i+2] <- afkast[i+4,i+2]
      afkast[i+5,i+3] <- afkast[i+4,i+3]
      afkast[i+5,i+4] <- afkast[i+4,i+4]
      afkast[i+5,16] <- afkast[i+4,i]
    }
  }
  
  

  for (i in 1:10){
    betalt_skat[i] <- gamma*afkast[i+5,16]
    skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
  }
  
  
  #COST FUNKTION
  A_b <- function(t) ifelse(t>=10,1,0)
  t <- seq(0, T, dt)
  A_t <- rep(0,200)
  for (i in 1:10){
    A_t[20*i] <- betalt_skat[i]
  }
  
  #value of portfolio
  V <- rep(NA,200)
  for (i in 1:200){
    if(i<200)
      V[i] <- bondprice_tax[i]
    else 
      V[i] <- 0
  }
  
  
  
  #sum of trading gains
  trading_gains_sum <- rep(NA,201)
  trading_gains <- rep(NA,201)
  trading_gains[1] <- 0
  trading_gains_sum[1]<-0
  for (t in 1:200){
    trading_gains[t+1] <- bondprice_tax[t]/bondprice[t]*(bondprice[(t+1)]-bondprice[t])
    trading_gains_sum[t+1] <- sum(trading_gains[1:(t+1)])
  }
  
  
  cost <- rep(NA,200)
  for (t in 1:201){
    cost[t] <- V[t] - trading_gains_sum[t] + A_b(dt*t) + A_t[t] 
  }
  
  cost_int_loss <- rep(NA,200)
  for (t in 1:200){
    cost_int_loss[t] <- (cost[(t)]*dt)
  }
  cost_int_calc_loss<- sum(cost_int_loss)
  diff_loss <- cost_int_calc_loss-cost[1]*10
  cost <- cost_int_calc_loss -diff_loss
  p_loss <- (cost_int_calc_loss - cost )/cost_int_calc_loss*100
  
  loss[k] <- diff_loss
  p_l[k] <- p_loss
}

#Sorting the losses
loss_S <- sort(loss, decreasing = TRUE)
p_l_S <- sort(p_l, decreasing = TRUE)


#Calc VaR
alpha_v <- 0.99
VaR_L <- loss_S[floor(10000*(1-alpha_v))+1]
#Calc ES
OS_L <- loss_S[1:(floor(10000*(1-alpha_v))+1)]
ES_L <- 1/(floor(10000*(1-alpha_v))+1)*sum(OS_L)

#Calc VaR
VaR_p <- p_l_S[floor(10000*(1-alpha_v))+1]
#Calc ES
OS_p <- p_l_S[1:(floor(10000*(1-alpha_v))+1)]
ES_p <- 1/(floor(10000*(1-alpha_v))+1)*sum(OS_p)

#plotting, a negative loss means that the cost of cont. tax is more expensive and positive means discrete is more expensive
par(mfrow=c(1,1))
hist(loss_S, breaks = 100, main = 'Losses CIR model', xlab = 'Loss size', xlim = c(-0.03,0.03))
abline(v=VaR_L, col="red")
abline(v=ES_L, col="blue")

hist(p_l_S, breaks = 100, main = 'Losses % CIR model', xlab = 'Percentage loss size',  xlim = c(-0.35,0.35))
abline(v=VaR_p, col="red")
abline(v=ES_p, col="blue")

mean(loss_S)
mean(p_l_S)
