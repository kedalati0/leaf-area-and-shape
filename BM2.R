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
data = subset(data,lblade=="simple") # select only simple-leafed species
head(data); table(data$lblade) # check

# Load priors for the model (deal with it later...)
refD = log(median(data$dbh)) # median DBH
preD = log(seq(1,4,l=20)) # DBH used for predictions
data$sla = data$area.cm2/data$aldm # calculate SLA
mod = lm(log(data$area.cm2)~log(data$aldm)) # leaf area as a function of leaf dry mass
cor.test(log(data$aldm),log(data$area.cm2)) # 0.95
cor.test(log(data$aldm),log(data$sla)) # -0.54
cor.test(log(data$aldm),mod$residuals) # 0

# Basic model input and parameters
dat = list(
  N = nrow(data), #S = length(unique(data$sp)), sp = as.numeric(factor(data$sp)), # metadata
  C = log(data$tnlc), # number of leaf units (individual level)
  M = log(data$aldm), SLA = mod$residuals, # leaf dry mass and SLA
  D = log(data$dbh)-refD, L = data$cii-1, # covariates (individual level)
  preD=preD-refD, # predictions' parameters
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
      
      ## Posterior predictions
      # Growth curves
      for(k in 1:length(preD)){
        pre[i,k] = alpha[1] + alpha[2]*D[k] + alpha[3]*L[i] + alpha[4]*4.67
        }
      }

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


hist(res2[,"alpha[2]"],main="D effect on LN"); abline(v=0,col=2,lwd=2,lty=3); 
median(res2[,"alpha[2]"]); sum(res2[,"alpha[2]"]>0)/length(res2[,"alpha[2]"]>0)





hist(res2[,"alpha[5]"],main="D effect on LDM"); abline(v=0,col=2,lwd=2,lty=3)
median(res2[,"alpha[5]"]); sum(res2[,"alpha[5]"]>0)/length(res2[,"alpha[5]"]>0)

hist(res2[,"alpha[6]"],main="D effect on LA"); abline(v=0,col=2,lwd=2,lty=3)
median(res2[,"alpha[6]"]); sum(res2[,"alpha[6]"]>0)/length(res2[,"alpha[6]"]>0)

hist(res2[,"VA[1,2]"],main="trade-off between LN and LDM")








