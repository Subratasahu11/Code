##### bayesian


##practical 1
library(stats)
dataset<-c(9.48,17.15,11.68,10.00,11.88,12.13,14.93,9.73,7.61,7.21,10.28,7.03,11.01,12.36,13.28,6.58,11.07,12.56,8.18,9.17)
prior_mu<-0
prior_sd<-sqrt(2)
calculate_posterior<-function(dataset,prior_mu,prior_sd,t){
  n<-length(dataset)
  dataset_mean<-mean(dataset)
  posterior_sd<-1/sqrt((1/prior_sd^2)+(n/t^2))
  posterior_mu<-posterior_sd^2*((prior_mu/prior_sd^2)+(dataset_mean*n/t^2))
  return(list(mu=posterior_mu,sd=posterior_sd))
}
t_values<-c(10,15,20)
posterior_distributions<-list()
posterior_means<-c()
for(t in t_values){
  posterior<-calculate_posterior(dataset,prior_mu,prior_sd,t)
  posterior_distributions[[as.character(t)]]<-posterior
  posterior_means<-c(posterior_means,posterior$mu)
}
par(mfrow=c(1,length(t_values)),mar=c(5,5,2,2))
for(i in 1:length(t_values)){
  t<-t_values[i]
  posterior<-posterior_distributions[[as.character(t)]]
  x<-seq(prior_mu-4*prior_sd,prior_mu+4*prior_sd,length.out=100)
  y_prior<-dnorm(x,prior_mu,prior_sd)
  y_posterior<-dnorm(x,posterior$mu,posterior$sd)
  plot(x,y_prior,type="l",col="blue",xlab="mu",ylab="density",main=paste("t=",t))
  lines(x,y_posterior,col="red")
  legend("topright",legend = c("prior","posterior"),col=c("blue","red"),lty = 1)
}
cat("Posterior Means:\n")
for(i in 1:length(t_values)){
  cat("t=",t_values[i],":",posterior_means[i],"\n")
}


##practical 2
data<-c(3.29,7.53,0.48,2.03,0.36,0,07,4.49,1.05,9.15,3.67,2.22,2.16,4.06,11.62,8.26,1.96,9.13,1.78,3.81,17.02)
alpha<-1
beta<-1
likelihood<-function(theta,data){
  n<-length(data)
  sum(-theta*data)+n*log(theta)
}
posterior<-function(theta,data,alpha,beta){
  likelihood(theta,data)+dgamma(theta,alpha,beta,log = TRUE)
}
theta_values<-seq(0.001,10,by=0.001)
posterior_values<-sapply(theta_values,posterior,data=data,alpha=alpha,beta=beta)
posterior_density<-exp(posterior_values-max(posterior_values))
posterior_density<-posterior_density/sum(posterior_density)
bayes_estimator<-sum(posterior_density*theta_values)
cdf<-cumsum(posterior_density)
lower_index<-min(which(cdf>=0.025))
upper_index<-min(which(cdf>=0.975))
hpd_interval<-c(theta_values[lower_index],theta_values[upper_index])
cat("Bayes Estimator:",bayes_estimator,"\n")
cat("0.95 HPD interval:",hpd_interval[1],"-",hpd_interval[2],"\n")



##practical 3
calculate_posterior<-function(test_score,prior_mean,prior_variance,test_variance){
  posterior_mean<-(prior_mean/prior_variance+test_score/test_variance)/(1/prior_variance+1/test_variance)
  posterior_variance<-1/(1/prior_variance+1/test_variance)
  return(list(mean=posterior_mean,variance=posterior_variance))
}
test_score<-115
prior_mean<-100
prior_variance<-225
test_variance<-100
posterior<-calculate_posterior(test_score,prior_mean,prior_variance,test_variance)
posterior_mean<-posterior$mean
posterior_variance<-posterior$variance
distribution<-paste("N(",posterior_mean,",",posterior_variance,")")
action1_loss<-function(theta){
  if(theta<90){
    return(0)
  }
  else if(theta>=90&&theta<110){
    return(theta-90)
  }
  else{
    return(2*(theta-90))
  }
}

action2_loss<-function(theta){
  if(theta<90){
    return(90-theta)
  }
  else if(theta>=90&&theta<110){
    return(0)
  }
  else{
    return(theta-110)
  }
}

action3_loss<-function(theta){
  if(theta<90){
    return(2*(theta-110))
  }
  else if(theta>=90&&theta<110){
    return(110-theta)
  }
  else{
    return(0)
  }
}
decision<-c(action1_loss(posterior_mean),action2_loss(posterior_mean),action3_loss(posterior_mean))
bayes_decision<-paste("a",which.min(decision),sep="")
cat("Distribution of true IQ:",distribution,"\n")
cat("Bayes decision:",bayes_decision,"\n")




#practical 4
fmu<-82.1
pmu<-82.4
psig<-1.1
fsig<-1.7
ro<-1/((psig)^2)+1/((fsig)^2)
mux<-(fmu/((fsig)^2)+pmu/((psig)^2))/ro
sigx<-(1/ro)^(0.5)
prior<-pnorm(83,pmu,psig)/(1-pnorm(83,pmu,psig))
post<-pnorm(83,mux,sigx)/(1-pnorm(83,mux,sigx))
bayes<-post/prior
if(bayes>1)
  print("we accept H0 i.e mass of electron is less than 83")
if(bayes<1)
  print("We accept H1 i.e mass of electron is greater than 83")
if(bayes==1)
  print("neither H0 nor H1")
variable<-c("Prior_hours","POsterior_hours","bayes_factor")
Estimate<-c(prior,post,bayes)
data.frame(variable,Estimate)



####Cism


##Mid Square Method practical 1
z<-array(0,dim=500)
z[1]=2182   ##four digit number
for(i in 1:500){
  z[i+1]=z[i]^2%/%100%%10000
}
z
u=z/10000
u
hist(u)
#LCG Method
m=15
c=5;a=3
z=array(0,dim = 20)
n=20
z[1]=7
for(i in 1:n){
  z[i+1]=(a*z[i]+c)%%m
}
z
u<-z/m
u
##convert this random numbers from 10 to 20
u1=10*u+10;u1



###Box Muller#practical 2
n<-100
observations<-numeric(n)
for(i in 1:n){
  u1<-runif(100)
  u2<-runif(100)
  z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
  z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
  observations[i]<-z1}
observations
plot(density(observations))
##acceptance-rejection method
target_pdf=function(x)
{
  return(sqrt(2/pi)*exp(-x^2/2))
}
x=seq(0,5,0.1)
pdf_x=target_pdf(x)
plot(x,pdf_x)
lines(x,exp(-x))
n=5000
y=rexp(n,1)
u=runif(n,0,1)
c=sqrt(2/pi)*exp(0.5)
z=c()
j=1
for(i in 1:n)
{  t=target_pdf(y[i])/(c*dexp(y[i],1))
if(u[i]<=t)
{ z[j]=y[i]
j=j+1;
}
}
z
m=length(z)
plot(density(z))
hist(z,freq=F)

##double exponential
u1<-runif(1000)
u2<-runif(1000)
t=array()
for(i in 1:1000){
  if(u1[i]<=1/2){
    t[i]<-log(u2[i])
  }
  else{t[i]=-log(1-u2[i])}
}
plot(density(t))
t
u=runif(1000)
z=array()
j=1
c=sqrt(2*2.71/3.14)
for(i in 1:1000){
  if(u[i]<=((1/sqrt(2*3.14))*exp(-(t[i]^2)/2))/(((1/2)*exp(-abs(t[i]))*c))){
    z[j]=t[i]
    j=j+1
  }
}
plot(density(z))
z
length(z)


##Gamma practical 3
rm(gamma)
scale=10
shape=0.6
rate=1/scale;rate
lambda=rate/shape;lambda
c1=(1/gamma(shape))/(rate/shape)*(((shape-1)/(rate-lambda))^(shape-1))*exp(1-shape);c1
target_pdf<-function(x,shape,scale){
  return((1/(gamma(shape)*(scale^shape)))*(exp(-x/scale)*(x^(shape-1))))
}
x=seq(0,1000,1);length(x)
pdf_x=target_pdf(x,shape,scale)
plot(x,pdf_x)
plot(x,(c1*(1/(shape*scale))*(exp(-1/((shape*scale))*x))))
n=500
y=rexp(n,rate=1/(shape*scale))
u=runif(n,0,1)
z=c()
j=1
for(i in 1:n){
  t=target_pdf(y[i],shape,scale)/(c1*(1/(shape*scale)*exp(-(1/(shape*scale)*y[i]))))
  if(u[i]<=t){
    z[j]=y[i]
    j=j+1
  }
}
p=z[1:100];p
m=length(p);m
plot(density(p))
hist(p,freq = F)





##practical 4
library(MASS)
log_normal<-rlnorm(100,meanlog = 2,sdlog=0.6)
log_normal
fit<-fitdistr(log_normal,densfun="lognormal")
fit$estimate



scale<-10
expo<-rexp(100,rate = 1/scale)
expo
likelihood<-function(scale,data){
  -sum(log(dexp(data,rate = 1/scale))) 
}
mle<-optim(par = 1,fn = likelihood,data=expo)
mle$par

c<-10
obs<-rexp(100,rate = 1)
obs<-obs[obs<=c]
obs
n<-length(obs)
mle<-n/sum(obs)
mle
truncate_weibull<-function(n,c){
  variable<-numeric(n)
  i<-1
  while(i<=n){
    x<-rweibull(1,shape = 1,scale = 1)
    if(x<=c){
      variable[i]<-x
      i<-i+1}
  }
  variable
}
weibull=truncate_weibull(100,c);weibull



library(MASS)
fit<-fitdistr(weibull,densfun = "weibull",start = list(shape=1,scale=1),lower=list(shape=0,scale=0))
fit$estimate




####practical5
n<-20
x<-runif(n,min=-2,max=2)
y<-runif(n,min =-2,max = 2)
for(i in 1:n){
  cat("(",x[i],",",y[i],")\n")
}
count<-sum(x^2+y^2<=4)
estimated_pi<-4*count/n;estimated_pi
plot(n,estimated_pi,type="l",xlab=20,ylab=3.6,main="convergence of estimated pi")


##practical 6
install.packages("survival")
library(survival)
lifetimes<-c(2.231011,3.172750,3.489482,3.564249,3.651828,3.775029,4.128005,4.330958,4.652746,4.694818,4.799658,4.967496,5.156554,5.169422,5.273725,5.420835,5.613455,5.758367)
censoring<-rep(1,length(lifetimes))
surv<-Surv(lifetimes,censoring)
fit<-survreg(surv~1,dist = "gaussian",control = survreg.control(maxiter = 1000))
coef(fit)
fit



#### Regression


data=cbind(c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,1.8369, 1.861, 1.8839),c(59, 60, 62, 56, 63, 59, 62, 60), 
           c(6, 13, 18, 28, 52, 53, 61, 60))
colnames(data) <-c("logarithm ofCS2 concentration ", "no of beetles","no of killed")
data
N<-length(data)
ni<-data[,2]
xi<-data[,1]
yi<-data[,3]
logistic<-glm(cbind(yi,ni-yi) ~ xi, family=binomial(logit), data=data.frame(data))
logistic
propi <- yi/ni

# Fitted logistic model
fit.logis <- Vectorize(function(d)  plogis(logistic$coefficients[1] +logistic$coefficients[2]*d))

curve(fit.logis,1.6,2, xlab = "Dose", ylab = "Proportion", main = "Fitted Model", lwd = 2, cex.axis = 1.5, cex.lab = 1.5)
points(xi,propi,pch=10,col="red",lwd=2)
logistic$coefficients[1]

##practical2
Y<-c(2,3,6,7,8,9,10,12,15)
X<-c(-1,-1,0,0,0,0,1,1,1)
pois<-glm(Y~X,family = "poisson")
summary(pois)
pois$fitted.values
par(mfrow = c(2,3))
plot(pois, which = 1:6)
par(mfrow = c(1,1))



##practical3
if (!requireNamespace("minpack.lm", quietly = TRUE)) {
  install.packages("minpack.lm")
}
library(minpack.lm)

# Define the data
x <- c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42)
y <- c(0.490, 0.475, 0.450, 0.437, 0.433, 0.455, 0.423, 0.407, 0.407, 0.407, 
       0.405, 0.393, 0.405, 0.400, 0.395, 0.400, 0.390, 0.390)

# Define the nonlinear model
model <- function(params, x) {
  a <- params[1]
  beta <- params[2]
  a + exp(-beta * (x - 8))
}


residuals <- function(params, x, y) {
  y - model(params, x)
}

# Initial guesses for parameters
start_params <- c(a = 0.4, beta = 0.1)

# Fit the model using nonlinear least squares
fit <- nls.lm(par = start_params, fn = residuals, x = x, y = y)

# Extract the estimated parameters
params <- coef(fit)
a <- params["a"]
beta <- params["beta"]

# Print the estimated parameters
cat("Estimated parameters:\n")
cat("a =", a, "\n")
cat("beta =", beta, "\n")







####practical 4
#y: Delivery Time(in min)

y<-c(16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8.00,17.83,79.24, 21.50,40.33,21.00, 13.50,19.75,24.00,29.00, 15.35, 19.00,9.50,35.10,17.90,52.32,18.75,19.83, 10.75)

y



#x1: Number of cases

x1<-c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)

x1


#x2: Distance (in ft)

x2<-c(560,220,340,80,150,330, 110,210,1460,605,688,215,255,462,448,776,200,132, 36,770,140,810,450,635,150)

x2



data<-data.frame(x1,x2)

data

# Obtaining Leverage Point

model=lm(y~.,data=data)

model

summary(model)

#use of hatvalues() function to extract the leverage values, leverage values

# ranges b/w 0-1. values closer to 1 indicates high leverages.

leverage<-hatvalues(model)

leverage


threshold<- 2*length(coef(model))/length (model$residuals)

threshold

high_leverage_values<-which (leverage>threshold)

high_leverage_values

# Conclusion: From the leverages obtained, the leverage value of obs.9 and obs.22 # are the two most leveraged values in the model.

# Obtaining the influential Point model

# there are three ways to detect the influential point:

#a) using cook statistic,



cook_dist<-cooks.distance(model)

cook_dist


threshold_cook <-4/(length (model$residuals))

threshold_cook

influential_values<- which(cook_dist>threshold)

influential_values

dfbetas<- dfbetas (model)

dfbetas


dfits<- dffits(model)

dfits

model=lm(y~.,data=data)

model

summary(model)


residuals<-summary(model) $residuals

residuals

predicted_values<-predict(model)

predicted_values


press_residuals<-residuals(model,type="response")/(1-hatvalues(model))

press_residuals


raw_residuals<-residuals(model)

raw_residuals


studentized_residuals<-rstudent(model)

studentized_residuals




####practical 5
y<-c(8.01,9.06,10.31,11.78,12.43,13.31,13.10,14.94,16.17,14.71,13.20,13.19,11.70,10.99,10.80,10.66,10.75,9.47,10.31,8.88);y
length(y)
#X2: price at well hear in the previous year

x2<-c(4.89,4.83,4.68,4.42,4.36,4.55,4.66,4.54,4.44,4.75,4.56,4.29,4.19, 4.17,4.11,4.04,3.96,3.85,3.75,3.69)

x2
length(x2)
#x3: domestic output

x3<-c(5.52,5.05,5.41,6.16,6.26,6.34,6.81,7.15,7.17,6.71,7.05,7.04,7.18, 7.33,7.54,7.61,7.80,8.30,8.81,8.66)

x3
length(x3)
#x4: GNP constant

x4<-c(487.67,490.55,533.55,576.57,598.62,621.77,613.57,654.80,668.84,681.02,879.53,720.53,736.86,755.34,799.15,830.70,874.29,925.66,980.98,1007.72)

x4
length(x4)
#x5: trend value/time

x5<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
x5
length(x5)

library(stats)

data<-data.frame(x2,x3,x4,x5)

data

mlr_model<-lm(y~.,data=data)

mlr_model

summary(mlr_model)

loglin_model<-glm(log(y)~.,data=data);loglin_model

summary(loglin_model)

#For MLR, we are looking at the coeficient of determination and adjusted r_2. #higher the values of coeficient of determination and adjusted r_2 it indicates the #better fit. For Log-linear regression, we will examine at the deviance and AIC, lower #the deviance and AIC indicates the better fit.

#Conclusion: The Log linear model provides the better fit with the lower and #negative value of AIC.

#b)

model<-lm(y~., data=data)

model

residuals=model$residuals #extraction of residuals

residuals

n=length(residuals)

n

numerator=sum(diff(residuals)^2)

numerator

denominator=sum(residuals^2)

denominator

durbin_watson=numerator/denominator

durbin_watson

#The durbin watson test ranges from 0-4

#Value close to 2 indicates NO Autocorrelation, less than 2 indicates

#positive autocorrelation, greater than 2 indicates negative correlation

#Conclusion: As the value of durbin watson test is 0.80 which is less than 2 #indicates positive correlation.



godfrey_model=lm(y~.,data=data)

godfrey_model

godfrey_residuals=residuals(godfrey_model)

godfrey_residuals

lagged_residuals=c(NA,head(godfrey_residuals,-1));lagged_residuals

auxiliary_model=lm(godfrey_residuals~lagged_residuals)

auxiliary_model

r_squared=summary(auxiliary_model)$r.squared 
r_squared

n=length(godfrey_residuals)

Breusch_Godfrey<-n*r_squared

Breusch_Godfrey

summary(Breusch_Godfrey)

#by the visual inspecation of acf and pacf we can decide the value of p for #specific lags occuring.

acf=acf(godfrey_residuals)

acf

pacf=pacf(godfrey_residuals)




pacf

p_value=1-pchisq(Breusch_Godfrey,df=1); p_value

#bgtest-bgtest(auxiliary_model.order=p) here p is the order of the #autoregressive error structure we want to calculate.

bgtest=bgtest(auxiliary_model,order=1)



new_model<-lm(y~x4+x5,data=data)

new_model

summary(new_model)



model_e<-lm(y~x3+x5,data-data)

model_e

summary(model_e)

#f)

install.packages("lmtest")

library(lmtest)

bpg_test<-bptest(lm(y~x4),data=data)

bpg_test

kb_test=bptest(lm(y~x4),data=data,studentize=FALSE);kb_test

# Conclusion: Lower the p-value, evidences the presence of heteroscedasticity.

# In this scenario, breusch-pagan-godfrey test indicates the presence

# of heteroscedasticity.

