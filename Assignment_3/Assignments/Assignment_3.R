# Assignment_3.1
posterior <- function(sigma,sigma_0,n,miu_0,x_bar){
  miu_posterior <- sigma^2*miu_0/(n*sigma_0^2+sigma^2) + n*sigma_0^2*x_bar/(n*sigma_0^2+sigma^2)
  sigma_pos <- sqrt(1/(n/sigma^2 + 1/sigma_0^2))
  return(c(miu_posterior,sigma_pos))
}

posterior(12000,9000,6,115000,121000)
miu <- 119628.571
sigma_miu <- 4302.823

sigma_cost <- sqrt(sigma_miu^2+12000**2)

probability <- pnorm(120000,miu,sigma_cost)
(1-probability)/probability

0.9545682/0.585


# Assignment_3.2
theta <- matrix(c(0.7,0.3))
p_theta <- matrix(c(0.4,0.6))

ER_y <-function(y){
  numerator <- (theta^y)*((1-theta)^(10-y))*p_theta
  denominator <- t(theta^y*((1-theta)^(10-y)))%*%p_theta
  p_theta_give_y <- numerator/sum(denominator)
  Rmat <- matrix(c(5,-3,-3,5),2,2)
  result <- Rmat%*%p_theta_give_y
  return(result)
} 
ER_y(10)

p_y <- function(y){
  p_y_given_H0 <- choose(10,y)*0.7**y*0.3**(10-y)*0.4
  p_y_given_H1 <- choose(10,y)*0.3**y*0.7**(10-y)*0.6
  return(p_y_given_H0+p_y_given_H1)
}
p_y(9)

VSI <- c()
P <- c()
for(y in c(6:10)){
  VSI <- c(VSI,ER_y(y)[1]-ER_y(10)[2])
  P <- c(P,p_y(y))
}
sum(VSI*P)# print the EVSI
