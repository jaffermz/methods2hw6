##Need to first load dataset
##load("NorthCarolina_data.dat")

kingdb <- infants
kingdb <- kingdb[,names(kingdb)%in%c("weight","smoker","sex","weeks","dTime")]
##of this I only need low birth weight (derived from weight), the gender
##variable, and weeks(gestation period)

kingdb$lbw <-0
kingdb$lbw[kingdb$weight<2.5]<-1
kingdb$Y <-0
kingdb$Y[kingdb$dTime <=365] <- 1
kingdb$weeksm <- kingdb$weeks-median(kingdb$weeks)
kingdb2 <- kingdb[,names(kingdb)%in%c("Y","sex","smoker","weeksm","lbw")]
kingdb2noY <- kingdb2[,names(kingdb2)%in%c("sex","smoker","weeksm","lbw")]
store <- 0

mod1totdat <- glm(Y~., data=kingdb2, family ="binomial")
vectorcoeffs <- coef(mod1totdat)
datawithYq2b <- kingdb2
datanoYq2b <- kingdb2noY

simul <- function(n,vectorcoeffs,dataset,datasetnoY)
{
  samp <- sample(1:dim(dataset)[1],size=n,replace=TRUE)
  samplednoY <- datasetnoY[samp,]
  samplednoY <- cbind(1,samplednoY)
  
  logitphat <- as.matrix(samplednoY)%*%vectorcoeffs
  phat <- exp(logitphat)/(1+exp(logitphat))
  randprob <- runif(n,0,1)
  samplednoY$y <- 0
  samplednoY$y[phat>randprob] <- 1
  ##samplednoY$phat <- phat
  ##samplednoY$randprob <- randprob
  ##print(names(samplednoY))
  fit0 <- glm(y~lbw+smoker+sex+weeksm,family="binomial",data=samplednoY[,names(samplednoY)%in%c("1","lbw","smoker","sex","weeksm","y")])
  ##print(summary(fit0))
  pvalue <- coef(summary(fit0))[c("lbw","smoker","sex","weeksm"),4]
  simul <- pvalue
  simul
}

replicatessimul <- function(R,n,beta0,betaw)
{
  for(i in 1:R)
  {
    onerunpval <- simul(n,beta0,betaw)
    if(onerunpval <= 0.05)
    {
      store <- store+1
    }
  }
  replicatessimul <- store/R
}

##construct function to do all the simulations

seq2 <- seq(3000,8000,by=500)

parametersset <- function(alpha, beta)
{
  thing <- rep(0,length(seq2))
  for(i in 1:length(seq2))
  {
    thing[i] <- replicatessimul(10000,seq2[i],alpha,beta)
  }
  parametersset <- thing
  parametersset
}