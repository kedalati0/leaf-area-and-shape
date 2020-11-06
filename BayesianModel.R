### Bayesian model for testing the ecological effects of the leaf size-number tradeoff in Amazonian saplings
# Goals:
# 0. Model variation in leaf area within each plant (between leaves; sigma);
# 1. Model covariation between leaf size (area/mass) and leaf number (tradeoff, Sigma);
# 2. Test the effects of size (D) and light (L) on average leaf mass/area and total leaf number per plant (alphas);
# 3. Test the effects of leaf type (simple vs. compound vs. bipinnate) on the leaf size/number tradeoff (ANCOVA, thetas);
# 4. Test the effects of Hmax on the tradeoff;
# 5. Test effects leaf size/number on RGR and components (new paper?).

# Load packages and functions
library(rjags) # Bayesian analysis
library(coda) # MCMC

# Load data input for the model
data = read.csv("DATA.csv",as.is=T)
data = subset(data,lblade=="simple")
head(data); table(data$lblade)

# Load priors for the model (deal with it later...)
sinds = cbind(log(data[,c("nl","ldm","area.cm2")]))
refD = median(data$dbh/1000) # median DBH


# Basic model input and parameters
dat = list(
  N = nrow(data), #S = length(unique(data$sp)), sp = as.numeric(factor(data$sp)), # metadata
  A = sinds, # leaf data (individual level)
  D = log(data$dbh/1000)-refD, L = data$cii-1, # covariates (individual level)
  priPA = diag(rep(.001,3)), pripa = diag(rep(.001,9)), prim = rep(0,9)  # priors for sampling error model (alphas)
  #Hmax=hmax$Hmax-refHmax, # predictor (species level)
  #prit=prit, priPt=solve(priVt), # priors for Hmax effects
  #preD=preD2-refD, preL=preL, preHmax=preHmax-refHmax, presp=presp # predictions' parameters
  )

## MODEL
# JAGS model
cat("model{
    
    ### Likelihood
    ## Individual level
    for(i in 1:N){
      # Size and illumination effects on leaf size/number
      MA[i,1:3] = alpha[1:3] + alpha[4:6]*D[i] + alpha[7:9]*L[i] # size and light effects
      A[i,1:3] ~ dmnorm(MA[i,1:3],PA[1:3,1:3]) # sampling error for response variables
      }

    alpha[1:9] ~ dmnorm(ma[1:9],pa[1:9,1:9])

    ### Priors
    # Inverse of var-covariance matrices
    PA ~ dwish(priPA,3+1) # individual traits (Sigma)
    pa ~ dwish(pripa,9+1) # parameters (Sigma)
    
    # Alphas
    ma ~ dmnorm(prim,pripa) # Hmax effects on architectural parameters
    
    ### Statistics
    # Variances and covariances
    VA = inverse(PA) # Covariances among response variables (after accounting for size and light effects)
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
           "VA","va") # covariances matrices
res = coda.samples(mod,params,n.iter=6680*4,thin=20)
dim(res[[1]]) # one of the chains
res2 = as.matrix(res)
dim(res2)
hist(res2[,"alpha[4]"],main="D effect on LN"); abline(v=0,col=2,lwd=2,lty=3)
hist(res2[,"alpha[5]"],main="D effect on LDM"); abline(v=0,col=2,lwd=2,lty=3)
hist(res2[,"alpha[6]"],main="D effect on LA"); abline(v=0,col=2,lwd=2,lty=3)
sum(res2[,"alpha[6]"]>0)/length(res2[,"alpha[6]"]>0)

plot(res2[,"alpha[1]"],res2[,"alpha[3]"])


