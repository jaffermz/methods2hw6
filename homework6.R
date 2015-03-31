kingdb <- KingCounty2001_data[,names(KingCounty2001_data)%in%c("welfare","married","smoker","education","age","wpre","bwt")]
kingdb$smoker2 <- 0
kingdb$smoker2[kingdb$smoker=="Y"] <- 1
kingdb$college <- 0
kingdb$college[kingdb$education>=17]<-1
kingdb$lbw <-0
kingdb$lbw[kingdb$bwt<2500]<-1
kingdb$agec <- (kingdb$age-30)/5
kingdb$wprec <- (kingdb$wpre-140)/20
kingdb$welfare <- as.numeric(kingdb$welfare)
kingdb$married <- as.numeric(kingdb$married)

kingdb2 <- kingdb[,names(kingdb)%in%c("welfare","married","smoker2","college","agec","wprec","lbw")]
kingdb2noY <- kingdb2[,-5]
store <- 0


simul <- function(n,beta0,betaw)
{
  samp <- sample(1:dim(kingdb2)[1],size=n,replace=TRUE)
  sampledbnoY <- kingdb2noY[samp,]
  sampledbnoY <- cbind(1,sampledbnoY)
  ##sampledb <- kingdb2[samp,]
  vectorcoeffs <- c(beta0,-0.71,betaw,0.69,-0.47,0.11,-0.22)
  ##print(sampledbnoY)
  ##print(vectorcoeffs)
  ##print(dim(sampledbnoY))
  ##print(length(vectorcoeffs))
  logitphat <- as.matrix(sampledbnoY)%*%vectorcoeffs
  phat <- exp(logitphat)/(1+exp(logitphat))
  randprob <- runif(n,0,1)
  sampledbnoY$y <- 0
  sampledbnoY$y[phat>randprob] <- 1
  ##sampledbnoY$phat <- phat
  ##sampledbnoY$randprob <- randprob
  fit0 <- glm(y~.,family="binomial",data=sampledbnoY[,names(sampledbnoY)%in%c("welfare","married","smoker2","college","agec","wprec","y")])
  pvalue <- coef(summary(fit0))["welfare",4]
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