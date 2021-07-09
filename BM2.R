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
head(data) # check

# Load priors for the model -> ##FIX!## (deal with it later...)
refD = median(data$dbh); refM = median(data$aldm) # median DBH and LDM
preD = seq(1,4,l=20); preM = exp(seq(log(0.01),log(10),len=20)); preSLA = seq(-1,1,len=5) # DBH, LDM, and SLA used for predictions

# Basic model input and parameters
dat = list(
  N = nrow(data), #S = length(unique(data$sp)), sp = as.numeric(factor(data$sp)), # metadata
  C = log(data$tnlc), # number of leaf units (individual level)
  M = log(data$aldm)-log(refM), SLA = data$sla2, # predictors (leaf dry mass and SLA)
  D = log(data$dbh)-log(refD), L = data$cii-1, # covariates (DBH and CII)
  preD = log(preD)-log(refD), preM = log(preM)-log(refM), preSLA = preSLA, # predictions' parameters
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
save(res2,file="POST.RData")


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

## Graphical parameters
labSLA = expression(paste("SLA (",mm^2,".",mg^-1,")"))
labTLA = expression(paste("Total leaf area (",m^2,")"))
labMLA = expression(paste("Mean leaf area (",cm^2,")"))
labD = "Stem diameter (cm)"
labL = "Crown illumination index"
labLDM = "Leaf dry mass (g)"
labLN = "Leaf number"

### Checking data fit
## GROWTH CURVES
# Growth curves (as a function of light)
curve(exp(m1 + m2*(log(x)-log(refD))),from=1,to=4,ylab="Number of leaves per plant",xlab="",
      ylim=range(data$tnlc),log="xy",lwd=.5) # L = 1
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-log(refD)) + m3*1),add=T,from=1,to=4,lwd=1) # L = 2
curve(exp(m1 + m2*(log(x)-log(refD)) + m3*2),add=T,from=1,to=4,lwd=2) # L = 3
legend("topleft",c("1","2","3"),title="L",bty="n",lwd=c(.5,1,2))

# Growth curves (as a function of leaf mass)
curve(exp(m1 + m2*(log(x)-log(refD))),from=1,to=4,ylab="",xlab="DBH (cm)",
      ylim=range(data$tnlc),log="xy") # leaf mass = 1 g
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-log(refD)) + m4*(log(10)-log(refM))),add=T,from=1,to=4,lwd=2) # leaf mass = 10 g
curve(exp(m1 + m2*(log(x)-log(refD)) + m4*(log(.1)-log(refM))),add=T,from=1,to=4,lwd=.5) # leaf mass = 0.1 g
legend("topleft",c("0.1","1.0","10"),title="Leaf mass (g)",bty="n",lwd=c(.5,1,2))

# Growth curves (as a function of SLA)
curve(exp(m1 + m2*(log(x)-log(refD))),from=1,to=4,ylab="",xlab="",
      ylim=range(data$tnlc),log="xy") # SLA = 0 (?)
points(tnlc~dbh,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m2*(log(x)-log(refD)) + m5*1),add=T,from=1,to=4,lwd=2) # SLA = 1 (?)
curve(exp(m1 + m2*(log(x)-log(refD)) + m5*-1),add=T,from=1,to=4,lwd=0.5) # SLA = -1 (?)
legend("topleft",c("5","13","35"),title=labSLA,bty="n",lwd=c(.5,1,2))


## TRADE-OFF
# Observed tradeoff (as a function of D)
curve(exp(m1 + m4*(log(x)-log(refM)) + m2*(log(2)-log(refD))),from=.05,to=10,ylab="Number of leaves per plant",xlab="",
      ylim=range(data$tnlc),log="xy") # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*(log(x)-log(refM)) + m2*(log(1)-log(refD))),add=T,from=.05,to=10,lwd=.5) # D = 
curve(exp(m1 + m4*(log(x)-log(refM)) + m2*(log(3)-log(refD))),add=T,from=.05,to=10,lwd=2) # D = 
legend("topright",c("1","2","3"),title="DBH (cm)",bty="n",lwd=c(.5,1,2))

# Observed tradeoff (as a function of L) -> n.s.
curve(exp(m1 + m4*(log(x)-log(refM))),from=.05,to=10,ylab="",xlab="Leaf mass (g)",
      ylim=range(data$tnlc),log="xy",lwd=.5) # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*(log(x)-log(refM)) + m3*1),add=T,from=.05,to=10,lwd=1) # L = 2
curve(exp(m1 + m4*(log(x)-log(refM)) + m3*2),add=T,from=.05,to=10,lwd=2) # L = 3
legend("topright",c("1","2","3"),title="L",bty="n",lwd=c(.5,1,2))

# Observed tradeoff (as a function of SLA)
curve(exp(m1 + m4*(log(x)-log(refM))),from=.05,to=10,ylab="",xlab="",
      ylim=range(data$tnlc),log="xy") # L = 1; D = 1.79 cm; SLA = 0
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)
curve(exp(m1 + m4*(log(x)-log(refM)) + m5*1),add=T,from=.05,to=10,lwd=2) # SLA = 1 (?)
curve(exp(m1 + m4*(log(x)-log(refM)) + m5*-1),add=T,from=.05,to=10,lwd=.5) # SLA = -1 (?)
legend("topright",c("5","13","35"),title=labSLA,bty="n",lwd=c(.5,1,2))


## Predicted tradeoffs
# For plants with the same size (D = 1.79 cm) growing in the shade (L = 1) and with a median SLA (SLA = 0)
tmp = exp(res2[,paste0("pre[",6,",",1:length(preM),",",3,"]")]) # medians
tt = apply(tmp,2,quantile,c(.025,.975)) # credible intervals
plot(apply(tmp,2,median),x=(preM),log="xy",type="l",xlab="Average mass per leaf (g)",ylab="Number of leaves per plant")
polygon(y=c(tt[1,],rev(tt[2,])),x=(c(preM,rev(preM))),col=grey(.5,.5),border=NA)
points(tnlc~aldm,data,col=gray(.5,.5),pch=16)

## Predicted vs. observed (model fit)
pred = exp(m1 + m2*(log(data$dbh)-log(refD)) + m3*(data$cii-1) + m4*(log(data$aldm)-log(refM)) + m5*data$sla2)
plot(pred~data$tnlc,log="xy");abline(a=0,b=1)
R2 = 1-sum((log(pred)-log(data$tnlc))^2)/sum((log(data$tnlc)-mean(log(data$tnlc)))^2); R2
res = numeric()
for(i in 1:nrow(tmp)){
  pred = exp(a1[i] + a2[i]*(log(data$dbh)-log(refD)) + a3[i]*(data$cii-1) + a4[i]*(log(data$aldm)-log(refM)) + a5[i]*data$sla2)
  res[i] = 1-sum((log(pred)-log(data$tnlc))^2)/sum((log(data$tnlc)-mean(log(data$tnlc)))^2)
  }
hist(res,main="CI of R^2")  
quantile(res,c(.025,.5,.975)) # credible interval


## Total leaf surface area
data$tla = data$mla*data$tnlc # observed total leaf area
plot(tla/10000~dbh,data,log="xy")
plot(tla/10000~cii,data,log="xy")
plot(tla/10000~aldm,data,log="xy")
plot(tla/10000~sla,data,log="xy")
plot(tla/10000~sla2,data,log="y")
plot(tla/10000~mla,data,log="xy")
mod = lm(log(data$mla)~log(data$aldm)) # SLA model
mod2 = lm(log(data$sla)~(data$sla2)) # SLA2->SLA model
exp(mod2$coef[1]+mod2$coef[2]*1) # SLA when SLA2 = 1
exp(mod2$coef[1]+mod2$coef[2]*0) # SLA when SLA2 = 0 
exp(mod2$coef[1]+mod2$coef[2]*-1) # SLA when SLA2 = -1
pretla = exp(m1+m2*(log(data$dbh)-log(refD))+m3*(data$cii-1)+m4*(log(data$aldm)-log(refM))+m5*data$sla2)*data$aldm/exp(mod2$coef[1]+mod2$coef[2]*data$sla2)  # predicted TLA
plot(data$tla,pretla,log="xy")
summary(lm(log(pretla)~log(data$tla)))
tfun = function(d=refD,l=1,m=refM,s=0){
  exp(m1+m2*(log(d)-log(refD))+m3*(l-1)+m4*(log(m)-log(refM))+m5*s)* # total number of leaves (predicted)
    exp(mod$coef[1]+mod$coef[2]*log(m))/10000
  }

# as a function of leaf mass
plot(tla/10000~(dbh),data,col=gray(.5,.5),pch=16,ylab=labTLA,xlab="",ylim=range(data$tla/10000),xlim=c(.5,4.5),log="y")
curve(tfun(d=x,m=.1),lwd=.5,add=T)
curve(tfun(d=x,m=10),lwd=2,add=T)
curve(tfun(d=x,m=1),lwd=1,add=T)
legend("bottomright",title="Leaf mass (g)",c("0.1","1.0","10"),lwd=c(.5,1,2),bty="n")

# as a function of SLA
plot(tla/10000~(dbh),data,col=gray(.5,.5),pch=16,ylab=labTLA,xlab="",ylim=range(data$tla/10000),xlim=c(.5,4.5),log="y")
curve(tfun(d=x,s=-1),lwd=2,add=T)
curve(tfun(d=x,s=1),lwd=.5,add=T)
curve(tfun(d=x,s=0),lwd=1,add=T)
legend("bottomright",c("5","13","35"),title=labSLA,bty="n",lwd=c(.5,1,2))

# as a function of light
plot(tla/10000~(dbh),data,col=gray(.5,.5),pch=16,ylab=labTLA,xlab="",ylim=range(data$tla/10000),xlim=c(.5,4.5),log="y")
curve(tfun(d=x,l=1),lwd=.5,add=T)
curve(tfun(d=x,l=2),lwd=2,add=T)
curve(tfun(d=x,l=3),lwd=1,add=T)
legend("bottomright",title="Light",leg=1:3,lwd=c(.5,1,2),bty="n")


### Predictions
## interspecific tradeoff with predicted values (standardized saplings: same DBH, CII, and SLA)
tmp = aggregate(cbind(aldm,sla,sla2,mla)~sp,data,median) # median leaf size (mass and area) and SLA per species
#tmp$aldm = log(tmp$aldm); tmp$mla = log(tmp$mla) # log-transform leaf area and SLA
head(tmp); nrow(tmp) # check
tmp2 = matrix(NA,nrow=length(a1),ncol=nrow(tmp)); colnames(tmp2) = tmp$sp # create empty matrix
for(j in 1:nrow(tmp)){
  for(i in 1:length(a1)){
    tmp2[i,j] = exp(a1[i]+a4[i]*(log(tmp[j,"aldm"])-log(refM))+a5[i]*tmp[j,"sla2"]) # estimated number of leaves (ENL) for median-sized saplings growing in the shade
    }
  }
tmp2[1:5,1:3] # check
tmp$enl = apply(tmp2,2,median) # estimated number of leaves

# ELN vs. LDM
plot(enl~aldm,tmp,log="xy",xlab=labLDM,ylab="Predicted leaf number",pch=16,col=grey(.5,.5),main="Interspecific trade-off")
cor.test(log(tmp$enl),log(tmp$aldm)) # median correlation R=-0.98
tmp3 = numeric(); for(i in 1:length(a1)){ tmp3[i] = cor(log(tmp2[i,]),log(tmp$aldm)) } # simulate correlations
quantile(tmp3,c(.025,.975)) # CI for R
legend("topright","R = -0.98***",bty="n")

# ELN vs. SLA and SLA2
plot(enl~sla,tmp,log="xy"); cor.test(log(tmp$enl),log(tmp$sla)) # SLA
plot(enl~sla2,tmp,log="y"); cor.test(log(tmp$enl),tmp$sla2) # SLA2

# ELN vs. MLA
plot(enl~mla,tmp,log="xy"); cor.test(log(tmp$enl),log(tmp$mla)) 
tmp3 = numeric(); for(i in 1:length(a1)){ tmp3[i] = cor(log(tmp2[i,]),log(tmp$mla)) }
quantile(tmp3,c(.025,.975))


## Total leaf area: large leaf advantage
tmp4 = matrix(NA,nrow=length(a1),ncol=nrow(tmp)); colnames(tmp2) = tmp$sp # create empty matrix
for(i in 1:nrow(tmp)){tmp4[,i] = tmp2[,i]*tmp$mla[i]/10000} # simulate total leaf area 
tmp$etla = apply(tmp4,2,median) # estimated total leaf area (m^2)
head(tmp) # check

# TLA vs. LDM
plot(etla~aldm,tmp,log="xy",xlab=labLDM,ylab=labTLA,pch=16,col=grey(.5,.5),main="Large leaf advantage")
cor.test(log(tmp$etla),log(tmp$aldm)) # median correlation R = 0.42***
tmp3 = numeric(); for(i in 1:length(a1)){ tmp3[i] = cor(log(tmp4[i,]),tmp$aldm) } # simulate correlations
quantile(tmp3,c(.025,.975)) # CI95% = [-0.18;0.62]
sum(tmp3>0)/length(tmp3) # P(R>0) = 91%

# TLA vs. SLA and SLA2
plot(etla~sla,tmp,log="xy"); cor.test(log(tmp$etla),log(tmp$sla)) # SLA
plot(etla~sla2,tmp,log="y"); cor.test(log(tmp$etla),tmp$sla2) # SLA2

# ELN vs. MLA
plot(etla~mla,tmp,log="xy",xlab=labMLA,ylab=labTLA,pch=16,col=grey(.5,.5),main="Large leaf advantage"); cor.test(log(tmp$etla),log(tmp$mla))
legend("topleft","R = 0.65***",bty="n")
tmp3 = numeric(); for(i in 1:length(a1)){ tmp3[i] = cor(log(tmp4[i,]),log(tmp$mla)) }
quantile(tmp3,c(.025,.975))


## trade-off between low leaf overshading (LAI) and total leaf area?
# observed
data$lai = data$tla/data$cpa # leaf area index
plot(lai~mla,data,log="xy",xlab=labMLA,ylab="LAI",main="Small leaf advantage",pch=16,col=grey(.5,.5))
cor.test(log(data$lai),log(data$mla))
legend("bottomright","R = 0.32***",bty="n")
cor.test(log(data$lai),log(data$aldm))
cor.test(log(data$lai),(data$sla2))


