## Simulate Sample Paths ##
#https://www.r-bloggers.com/2010/04/fun-with-the-vasicek-interest-rate-model/
## define model parameters
r0 <- 0.01
theta <- 0.02
k <- 0.2
beta <- 0.015

## simulate short rate paths
n <- 1    # MC simulation trials
T <- 1    # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval


#simulate short rate
r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- k*(theta-r[i-1,j])*dt + beta*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 

#simulate the stock price process (assuming geometric BM)
S <- matrix(0,m+1,n)  # matrix to hold short rate paths
S[1,] <- 100
volatility <- 0.015

for(j in 1:n){
  for(i in 2:(m+1)){
    dS <- r[i-1,j]*dt + volatility*S[i-1,j]*sqrt(dt)*rnorm(1,0,1)
    S[i,j] <- S[i-1,j] + dS
  }
} 


## plot paths
t <- seq(0, T, dt)
rT.expected <- theta + (r0-theta)*exp(-k*t)
rT.stdev <- sqrt( beta^2/(2*k)*(1-exp(-2*k*t)))
matplot(t, r[,1:1], type="l", lty=1, main="Short Rate Paths", ylab="rt") 
abline(h=theta, col="red", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2) 
points(0,r0)

matplot(t, S[,1:1], type="l", lty=1, main="Asset price path", ylab="rt") 






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



#IKKE OVERSKRIV!!
#rente_matrix <- matrix(NA,50,201)
#for (i in 1:201){
#rente_matrix[4,i] <- r[i]}
#r <- rente_matrix[3,]
#r<- rente_matrix[1,]
#r <- t(r)
#r <- t(r)
# rente matrix for scaled up sigma 
#rente_matrix_up <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_up[4,i] <- r[i]}
# rente matrix for scaled down sigma 
#rente_matrix_down <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_down[4,i] <- r[i]}
# rente matrix for meget negativ rente
#rente_matrix_neg <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_neg[1,i] <- r[i]
#}
# rente matrix for scaled up b
#rente_matrix_b_up <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_b_up[4,i] <- r[i]}
# rente matrix for scaled down b
#rente_matrix_b_down <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_b_down[4,i] <- r[i]}
# rente matrix for scaled up alpha
#rente_matrix_alpha_up <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_alpha_up[4,i] <- r[i]}
# rente matrix for scaled down alpha
#rente_matrix_alpha_down <- matrix(NA,20,201)
#for (i in 1:201){
#rente_matrix_alpha_down[4,i] <- r[i]}

r <- t(r)
r <- t(r)



#når vi hele tiden justerer portefølgen efter renten = overskud!

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




#Fars program
PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
afkast <- matrix(NA,15,17)
skyggekonto_overskud <- rep(NA,10)


#Beregner afkastet hvert år, når der tages højde for 5 års fremskrivningsreglen
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



#udregner betalt PAL skat pr år og skyggekonto overskud
for (i in 1:10){
betalt_skat[i] <- gamma*afkast[i+5,16]
skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
}


#COST FUNKTIOM
#definerer først insurance payment funktionen
A_b <- function(t) ifelse(t>=10,1,0)
#definerer betalt skat i dt tidsintervallerne
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

#sum af trading gains FORKERT: SKAL HAVE t+1! se nedenfor 
#trading_gains_sum <- rep(NA,200)
#trading_gains <- rep(NA,200)
#for (t in 1:200){
#  trading_gains[t] <- bondprice_tax[t]/bondprice[t]*(bondprice[(t+1)]-bondprice[t])
#  trading_gains_sum[t] <- sum(trading_gains[1:t])
#}


#sum af trading gains
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
## OBS MÅL TIL PLOT ER 610 x 350
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


#cost_int_2 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_2[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_2<- sum(cost_int_2)
#diff_2 <- cost_int_calc_2-cost[1]*10
#cost <- cost_int_calc_2 -diff_2
#p_2 <- (cost_int_calc_2 - cost )/cost_int_calc_2*100



#cost_int_3 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_3[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_3<- sum(cost_int_3)
#diff_3 <- cost_int_calc_3-cost[1]*10
#cost <- cost_int_calc_3 -diff_3
#p_3 <- (cost_int_calc_3 - cost  )/cost_int_calc_3*100



#cost_int_4 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_4[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_4<- sum(cost_int_4)
#diff_4 <- cost_int_calc_4-cost[1]*10
#cost <- cost_int_calc_4 -diff_4
#p_4 <- (cost_int_calc_4 -cost)/cost_int_calc_4*100



#cost_int_5 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_5[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_5<- sum(cost_int_5)
#diff_5 <- cost_int_calc_5-cost[1]*10
#cost <- cost_int_calc_5 -diff_5
#p_5 <- (cost_int_calc_5 - cost)/cost_int_calc_5*100


#cost_int_6 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_6[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_6<- sum(cost_int_6)
#diff_6 <- cost_int_calc_6-cost[1]*10
#cost <- cost_int_calc_6 -diff_6
#p_6 <- (cost_int_calc_6 - cost )/cost_int_calc_6*100


#cost_int_7 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_7[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_7<- sum(cost_int_7)
#diff_7 <- cost_int_calc_7-cost[1]*10
#cost <- cost_int_calc_7 -diff_7
#p_7 <- (cost_int_calc_7-cost)/cost_int_calc_7*100


#cost_int_8 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_8[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_8<- sum(cost_int_8)
#diff_8 <- cost_int_calc_8-cost[1]*10
#cost <- cost_int_calc_8 -diff_8
#p_8 <- (cost_int_calc_8 - cost  )/cost_int_calc_8*100


#cost_int_9 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_9[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_9<- sum(cost_int_9)
#diff_9 <- cost_int_calc_9-cost[1]*10
#cost <- cost_int_calc_9 -diff_9
#p_9 <- (cost_int_calc_9 - cost )/cost_int_calc_9*100


#cost_int_10 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_10[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_10<- sum(cost_int_10)
#diff_10 <- cost_int_calc_10-cost[1]*10
#cost <- cost_int_calc_10 -diff_10
#p_10 <- (cost_int_calc_10 - cost )/cost_int_calc_10*100


#cost_int_11 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_11[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_11<- sum(cost_int_11)
#diff_11 <- cost_int_calc_11-cost[1]*10
#cost <- cost_int_calc_11 -diff_11
#p_11 <- (cost_int_calc_11 - cost  )/cost_int_calc_11*100


#cost_int_12 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_12[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_12<- sum(cost_int_12)
#diff_12 <- cost_int_calc_12-cost[1]*10
#cost <- cost_int_calc_12 -diff_12
#p_12 <- (cost_int_calc_12 - cost  )/cost_int_calc_12*100


#cost_int_13 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_13[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_13<- sum(cost_int_13)
#diff_13 <- cost_int_calc_13-cost[1]*10
#cost <- cost_int_calc_13 -diff_13
#p_13 <- (cost_int_calc_13 - cost )/cost_int_calc_13*100


#cost_int_14 <- rep(NA,200)
#for (t in 1:200){
#  cost_int_14[t] <- (cost[(t)]*dt)
#}
#cost_int_calc_14<- sum(cost_int_14)
#diff_14 <- cost_int_calc_14-cost[1]*10
#cost <- cost_int_calc_14 -diff_14
#p_14 <- (cost_int_calc_14 - cost )/cost_int_calc_14*100



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




#Fars program
PALt <- rep(NA,10)
for (i in 1:10){
  PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
}

betalt_skat <- rep(NA,10)
afkast <- matrix(NA,15,17)
skyggekonto_overskud <- rep(NA,10)


#Beregner afkastet hvert år, når der tages højde for 5 års fremskrivningsreglen
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



#udregner betalt PAL skat pr år og skyggekonto overskud
for (i in 1:10){
  betalt_skat[i] <- gamma*afkast[i+5,16]
  skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
}


#COST FUNKTIOM
#definerer først insurance payment funktionen
A_b <- function(t) ifelse(t>=10,1,0)
#definerer betalt skat i dt tidsintervallerne
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



#sum af trading gains
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
  
  
  
  
  #Fars program
  PALt <- rep(NA,10)
  for (i in 1:10){
    PALt[i] <- bondprice_tax[i/0.05+1]-  bondprice_tax[i/0.05-20+1]
  }
  
  betalt_skat <- rep(NA,10)
  afkast <- matrix(NA,15,17)
  skyggekonto_overskud <- rep(NA,10)
  
  
  #Beregner afkastet hvert år, når der tages højde for 5 års fremskrivningsreglen
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
  
  
  
  #udregner betalt PAL skat pr år og skyggekonto overskud
  for (i in 1:10){
    betalt_skat[i] <- gamma*afkast[i+5,16]
    skyggekonto_overskud[i] <- (1-gamma)*afkast[i+5,16]
  }
  
  
  #COST FUNKTIOM
  #definerer først insurance payment funktionen
  A_b <- function(t) ifelse(t>=10,1,0)
  #definerer betalt skat i dt tidsintervallerne
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
  
  
  
  #sum af trading gains
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

#mcir <- mean(loss_S)
#mpcir <- mean(p_l_S)
