### Bayesian model for testing the ecological effects of the leaf size-number tradeoff in Amazonian saplings
# Goals:
# 0. Model variation in leaf area within each plant (between leaves; sigma); -> later
# 1. Model covariation between leaf size (area/mass) and leaf number (tradeoff, Sigma);***
# 2. Test the effects of size (D), light (L), and SLA on average leaf mass/area and total leaf number per plant (alphas);***
# 3. Test the effects of leaf type (simple vs. compound vs. bipinnate) on the leaf size/number tradeoff (ANCOVA, thetas); -> later
# 4. Test the effects of Hmax on the tradeoff; -> later

# Load packages and functions
library(rjags) # Bayesian analysis
library(coda) # MCMC

# Load data input for the model
data = read.csv("DATA.csv",as.is=T) # import raw data
table(data$lblade) 
data = subset(data,lblade=="simple") # select only simple-leafed species
head(data) # check

# Load priors for the model (deal with it later...)
refD = log(median(data$dbh)); refM = log(median(data$aldm)) # median DBH and LDM
preD = log(seq(1,4,l=20)); preM = seq(-4.6,2.3,len=20); preSLA = seq(-1,1,len=5) # DBH, LDM, and SLA used for predictions
data$sla = data$larea/data$aldm # calculate SLA
mod2 = lm(log(data$larea)~log(data$aldm)) # leaf area as a function of leaf dry mass
data$sla2 = mod2$residuals
cor.test(log(data$aldm),log(data$larea)) # 0.96
cor.test(log(data$aldm),log(data$sla)) # -0.54
cor.test(log(data$aldm),data$sla2) # 0
cor.test(log(data$sla),data$sla2) # 0.85
plot(sla2~sla,data,log="x")

# Basic model input and parameters
dat = list(
  N = nrow(data), #S = length(unique(data$sp)), sp = as.numeric(factor(data$sp)), # metadata
  C = log(data$tnlc), # number of leaf units (individual level)
  M = log(data$aldm), SLA = data$sla2, # leaf dry mass and SLA
  D = log(data$dbh)-refD, L = data$cii-1, # covariates (individual level)
  preD = preD-refD, preM = preM-refM, preSLA = preSLA, # predictions' parameters
  pripa = diag(rep(.001,5)), prim = rep(0,5)  # priors for sampling error model (alphas)
  )


## MODEL
# JAGS model
cat("model{
    
    ### Likelihood
    ## Individual level
    for(i in 1:N){
      # Size and illumination effects on leaf size/number
      c[i] = alpha[1] + alpha[2]*D[i] + alpha[3]*L[i] + alpha[4]*M[i] + alpha[5]*SLA[i] # size, light, leaf mass, and SLA effects
      C[i] ~ dnorm(c[i],PC) # sampling error for response variables
      }

      ## Posterior predictions
      # Growth curves
      for(d in 1:length(preD)){
        for(m in 1:length(preM)){
          for(s in 1:length(preSLA)){
            pre[d,m,s] = alpha[1] + alpha[2]*preD[d] + alpha[4]*preM[m] + alpha[5]*preSLA[s]
            }
          }
        }

    # Parameters
    alpha[1:5] ~ dmnorm(ma[1:5],pa[1:5,1:5])

    ### Priors
    # Inverse of var-covariance matrices
    PC ~ dunif(0,10000) # variation in number of leaves
    pa ~ dwish(pripa,5+1) # parameters (Sigma)
    
    # Alphas
    ma ~ dmnorm(prim,pripa) # Hmax effects on architectural parameters
    
    ### Statistics
    # Variances and covariances
    VC = inverse(PC) # 
    va = inverse(pa) # Covariances among alphas
      
    }",
    
    file="JAGS_model.txt")

# Run JAGS
mod = jags.model(file="JAGS_model.txt", data=dat, n.chains=3, n.adapt=2000)


## MCMC
# Burnin
update(mod,n.iter=10000)

# JAGS output
params = c("alpha","ma", # effects (size, light)
           "pre", # posterior predictions
           "VC","va") # covariances matrices
res = coda.samples(mod,params,n.iter=6680*4,thin=20)
dim(res[[1]]) # one of the chains
res2 = as.matrix(res)
dim(res2)

### Results
# Parameter estimates (alphas)
a1 = res2[,"alpha[1]"]; m1 = median(a1); m1; sum(a1>0)/length(a1>0) # intercept
a2 = res2[,"alpha[2]"]; m2 = median(a2); m2; sum(a2>0)/length(a2>0) # size effect
a3 = res2[,"alpha[3]"]; m3 = median(a3); m3; sum(a3>0)/length(a3>0) # light effect
a4 = res2[,"alpha[4]"]; m4 = median(a4); m4; sum(a4>0)/length(a4>0) # leaf mass effect
a5 = res2[,"alpha[5]"]; m5 = median(a5); m5; sum(a5>0)/length(a5>0) # SLA effect

# Posterior distribution for alphas
hist(exp(a1),main="alpha[1]",xlab="Leaf number (intercept)"); abline(v=0,col=2,lwd=2,lty=3)
hist(a2,main="alpha[2]",xlab="D effect on LN"); abline(v=0,col=2,lwd=2,lty=3)
hist(a3,main="alpha[3]",xlab="L effect on LN"); abline(v=0,col=2,lwd=2,lty=3)
hist(a4,main="alpha[4]",xlab="M effect on LN"); abline(v=0,col=2,lwd=2,lty=3)
hist(a5,main="alpha[5]",xlab="SLA effect on LN"); abline(v=0,col=2,lwd=2,lty=3)

## Checking data fit
# Growth curves (as a function of light)
curve(exp(m1 + m2*(log(x)-refD)),from=1,to=4,ylab="Number of leaves per plant",xlab="DBH (cm)",
      ylim=range(data$tnlc),log="xy",lwd=.5) # L = 1
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-refD) + m3*1),add=T,from=1,to=4,lwd=1) # L = 2
curve(exp(m1 + m2*(log(x)-refD) + m3*2),add=T,from=1,to=4,lwd=2) # L = 3
legend("topleft",c("1","2","3"),title="L",bty="n",lwd=c(.5,1,2))

# Growth curves (as a function of leaf mass)
curve(exp(m1 + m2*(log(x)-refD)),from=1,to=4,ylab="Number of leaves per plant",xlab="DBH (cm)",
      ylim=range(data$tnlc),log="xy") # leaf mass = 1 g
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-refD) + m4*2.3),add=T,from=1,to=4,lwd=2) # leaf mass = 10 g
curve(exp(m1 + m2*(log(x)-refD) + m4*-2.3),add=T,from=1,to=4,lwd=.5) # leaf mass = 0.1 g
legend("topleft",c("0.1","1.0","10"),title="Leaf mass (g)",bty="n",lwd=c(.5,1,2))

# Growth curves (as a function of SLA)
curve(exp(m1 + m2*(log(x)-refD)),from=1,to=4,ylab="Number of leaves per plant",xlab="DBH (cm)",
      ylim=range(data$tnlc),log="xy") # SLA = 0 (?)
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-refD) + m5*1),add=T,from=1,to=4,lwd=2) # SLA = 1 (?)
curve(exp(m1 + m2*(log(x)-refD) + m5*-1),add=T,from=1,to=4,lwd=0.5) # SLA = -1 (?)
legend("topleft",c("-1","0","1"),title="SLA",bty="n",lwd=c(.5,1,2))

# Observed tradeoff (as a function of D)
curve(exp(m1 + m4*log(x) + m2*(log(2)-refD)),from=.05,to=10,ylab="Number of leaves per plant",xlab="Leaf mass (g)",
      ylim=range(data$tnlc),log="xy") # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*log(x) + m2*(log(1)-refD)),add=T,from=.05,to=10,lwd=.5) # D = 
curve(exp(m1 + m4*log(x) + m2*(log(3)-refD)),add=T,from=.05,to=10,lwd=2) # D = 
legend("topright",c("1","2","3"),title="DBH (cm)",bty="n",lwd=c(.5,1,2))

# Observed tradeoff (as a function of L)
curve(exp(m1 + m4*log(x)),from=.05,to=10,ylab="Number of leaves per plant",xlab="Leaf mass (g)",
      ylim=range(data$tnlc),log="xy",lwd=.5) # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*log(x) + m3*1),add=T,from=.05,to=10,lwd=1) # L = 2
curve(exp(m1 + m4*log(x) + m3*2),add=T,from=.05,to=10,lwd=2) # L = 3
legend("topright",c("1","2","3"),title="L",bty="n",lwd=c(.5,1,2))

# Observed tradeoff (as a function of SLA)
curve(exp(m1 + m4*log(x)),from=.05,to=10,ylab="Number of leaves per plant",xlab="Leaf mass (g)",
      ylim=range(data$tnlc),log="xy") # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*log(x) + m5*1),add=T,from=.05,to=10,lwd=2) # SLA = 1 (?)
curve(exp(m1 + m4*log(x) + m5*-1),add=T,from=.05,to=10,lwd=.5) # SLA = -1 (?)
legend("topright",c("-1","0","1"),title="SLA",bty="n",lwd=c(.5,1,2))

## Predicted tradeoffs
# For plants with the same size (D = 1.79 cm) growing in the shade (L = 1) and with a median SLA (SLA = 0)
tmp = exp(res2[,paste0("pre[",6,",",1:length(preM),",",3,"]")]) # medians
tt = apply(tmp,2,quantile,c(.025,.975)) # credible intervals
plot(apply(tmp,2,median),x=exp(preM),log="xy",type="l",xlab="Average mass per leaf (g)",ylab="Number of leaves per plant")
polygon(y=c(tt[1,],rev(tt[2,])),x=exp(c(preM,rev(preM))),col=grey(.5,.5),border=NA)

## Predicted vs. observed (model fit)
pred = exp(m1 + m2*(log(data$dbh)-refD) + m3*(data$cii-1) + m4*log(data$aldm) + m5*data$sla2)
plot(pred~data$tnlc,log="xy");abline(a=0,b=1)
R2 = 1-sum((log(pred)-log(data$tnlc))^2)/sum((log(data$tnlc)-mean(log(data$tnlc)))^2); R2
res = numeric()
for(i in 1:nrow(tmp)){
  pred = exp(a1[i] + a2[i]*(log(data$dbh)-refD) + a3[i]*(data$cii-1) + a4[i]*log(data$aldm) + a5[i]*data$sla2)
  res[i] = 1-sum((log(pred)-log(data$tnlc))^2)/sum((log(data$tnlc)-mean(log(data$tnlc)))^2)
  }
hist(res)  
quantile(res,c(.025,.975)) # credible interval

## Total leaf surface area
parea = exp(mod2$coef[1]+mod2$coef[2]*log(data$aldm))
plot(parea,data$larea,log="xy")
# as a function of leaf size, but constant SLA
curve(exp(m1+m2*(log(x)-refD))*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      ylab="Total leaf surface (m2)",xlab="DBH (cm)")
curve(exp(m1+m2*(log(x)-refD)+m4*log(10))*exp(mod2$coef[1]+mod2$coef[2]*log(10))/10000,
      lwd=2,add=T)
curve(exp(m1+m2*(log(x)-refD)+m4*log(.1))*exp(mod2$coef[1]+mod2$coef[2]*log(.1))/10000,
      lwd=.5,add=T)
legend("topleft",title="Leaf size (g)",c("0.1","1.0","10"),lwd=c(.5,1,2),bty="n")
# as a function of SLA, but constant leaf size
curve(exp(m1+m2*(log(x)-refD))*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      ylab="Total leaf surface (m2)",xlab="DBH (cm)")
curve(exp(m1+m2*(log(x)-refD)+m5*-1)*(exp(mod2$coef[1]+mod2$coef[2]*log(1))+exp(-1))/10000,
      lwd=.5,add=T)
curve(exp(m1+m2*(log(x)-refD)+m5*1)*(exp(mod2$coef[1]+mod2$coef[2]*log(1))+exp(1))/10000,
      lwd=2,add=T)
legend("topleft",title="SLA (?)",leg=c(-1,0,1),lwd=c(.5,1,2),bty="n")
# as a function of light
curve(exp(m1+m2*(log(x)-refD))*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      ylab="Total leaf surface (m2)",xlab="DBH (cm)",lwd=.5)
curve(exp(m1+m2*(log(x)-refD)+m3*1)*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      lwd=1,add=T)
curve(exp(m1+m2*(log(x)-refD)+m3*2)*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      lwd=2,add=T)
legend("topleft",title="Light",leg=1:3,lwd=c(.5,1,2),bty="n")
# as a function of leaf size and SLA
curve(exp(m1+m2*(log(x)-refD))*exp(mod2$coef[1]+mod2$coef[2]*log(1))/10000,
      ylab="Total leaf surface (m2)",xlab="DBH (cm)")
curve(exp(m1+m2*(log(x)-refD)+m4*log(10)+m5*-1)*(exp(mod2$coef[1]+mod2$coef[2]*log(10))+exp(-1))/10000,
      lty=2,add=T)
curve(exp(m1+m2*(log(x)-refD)+m4*log(.1)+m5*1)*(exp(mod2$coef[1]+mod2$coef[2]*log(.1))+exp(1))/10000,
      lty=3,add=T)
legend("topleft",leg=c("M = 1; SLA = 0","M = 10; SLA = -1","M = 0.1; SLA = 1"),lty=1:3,bty="n")



