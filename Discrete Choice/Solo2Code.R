##################
##Chapter 1 #######
##################
load("stc-cbc-respondents-v3(1).RData")

ls()
str(resp.data.v3)
head(resp.data.v3)
df_task <- read.csv("stc-dc-task-cbc -v3(1).csv", sep="\t")
str(df_task)
head(df_task)
require(dummies)
load("efCode.RData")
ls()
str(efcode.att.f)
str(efcode.attmat.f)

apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))}) 
task.mat <- as.matrix(df_task[, c("screen", "RAM", "processor", "price", "brand")])
dim(task.mat)
head(task.mat)
task.mat
X.mat=efcode.attmat.f(task.mat)  # Here is where we do effects coding
dim(X.mat)
head(X.mat)
pricevec=df_task$price-mean(df_task$price)
head(pricevec)
str(pricevec)
X.brands=X.mat[,9:11]
dim(X.brands)
str(X.brands)
X.BrandByPrice = X.brands*pricevec
dim(X.BrandByPrice)
str(X.BrandByPrice)
X.matrix=cbind(X.mat,X.BrandByPrice)
dim(X.matrix)
str(X.matrix)
head(X.matrix)
X2.matrix=X.matrix[,1:2]
dim(X2.matrix)
det(t(X.matrix) %*% X.matrix)
ydata=resp.data.v3[,4:39]
names(ydata)
str(ydata)
head(ydata)
ydata=na.omit(ydata)
ydatadf <- ydata
head(ydatadf)
ydata=as.matrix(ydata)
dim(ydata)
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) )
head(zowner)
lgtdata = NULL
for (i in 1:424) { lgtdata[[i]]=list( y=ydata[i,],X=X.matrix )}
length(lgtdata)
str(lgtdata)

zgender = resp.data.v3$D3 = ifelse(resp.data.v3$D3 == 1, 1, 0)



##################
##Model 1#######
###################
require(bayesm)
mcmctest=list(R=5000, keep=5)
Data1=list(p=3,lgtdata=lgtdata)
testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
names(testrun1)
betadraw1=testrun1$betadraw

dim(betadraw1)
plot(1:length(betadraw1[1,1,]),betadraw1[1,1,])
plot(density(betadraw1[1,1,701:1000],width=2))
abline(v=0) ## vertical line
plot(density(betadraw1[1,2,701:1000],width=2))
abline(v=0) ## vertical line
abline(h=0.010) ## horizontal line
#### Compute the probability that person 1 beta 1 > 0 ####
#### Assumes Normal distribution of beta 1 ######
mn <- mean(betadraw1[1,1,701:1000])
sd <- sd(betadraw1[1,1,701:1000])
mn
sd
plot(density(betadraw1[1,1,701:1000],width=2))
abline(v=0) ## vertical line
abline(v=mn) ## vertical line
prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob
#### Compute the empirical probability based on sample values ###
p1b1 <- betadraw1[1,1,701:1000]
quantile(p1b1, probs = seq(0, 1, by= 0.05))

summary(betadraw1[1,1,701:1000]) ##log(odds ratio)=0.7833; odds ratio = exp(0.7833)=2.18;
summary(betadraw1[1,2,701:1000]) ##log(odds ratio)=-0.264; odds ratio = exp(-0.264)=0.76;

betameansoverall <- apply(betadraw1[,,701:1000],c(2),mean)
betameansoverall

perc <- apply(betadraw1[,,701:1000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc

plot(betadraw1[3, 2,])
abline(h=0)

df_loglike1 <- data.frame(draw=1:length(testrun1$loglike), 
                          loglike=testrun1$loglike)

ggplot(df_loglike1) + 
  geom_line(aes(y=loglike, x=draw)) +
  labs(x='Draw', y='Log Likelihood')



##################
##Chapter 3#######
##################
zownertest=matrix(scale(zowner,scale=FALSE),ncol=1)
Data2=list(p=3,lgtdata=lgtdata,Z=zownertest)
testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2)
dim(testrun2$Deltadraw)
apply(testrun2$Deltadraw[701:1000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
deltadraw1 <- apply(testrun2$Deltadraw[701:1000,],c(2),mean)
deltadraw1
betadraw2=testrun2$betadraw
dim(betadraw2)


plot(1:length(betadraw2[1,1,]),betadraw2[1,1,])
plot(density(betadraw2[1,1,701:1000],width=2))
abline(v=0) ## vertical line
plot(density(betadraw2[1,2,701:1000],width=2))
abline(v=0) ## vertical line
abline(h=0.010) ## horizontal line
#### Compute the probability that person 1 beta 1 > 0 ####
#### Assumes Normal distribution of beta 1 ######
mn <- mean(betadraw2[1,1,701:1000])
sd <- sd(betadraw2[1,1,701:1000])
mn
sd
plot(density(betadraw2[1,1,701:1000],width=2))
abline(v=0) ## vertical line
abline(v=mn) ## vertical line
prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob
#### Compute the empirical probability based on sample values ###
p1b1 <- betadraw2[1,1,701:1000]
quantile(p1b1, probs = seq(0, 1, by= 0.1))

summary(betadraw2[1,1,701:1000]) ##log(odds ratio)=0.7833; odds ratio = exp(0.7833)=2.18;
summary(betadraw2[1,2,701:1000]) ##log(odds ratio)=-0.264; odds ratio = exp(-0.264)=0.76;
betameansoverall <- apply(betadraw2[,,701:1000],c(2),mean)
betameansoverall
perc <- apply(betadraw2[,,701:1000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc

plot(betadraw2[3, 2,])
abline(h=0)

df_loglike2 <- data.frame(draw=1:length(testrun2$loglike), 
                          loglike=testrun2$loglike)

ggplot(df_loglike2) + 
  geom_line(aes(y=loglike, x=draw)) +
  labs(x='Draw', y='Log Likelihood')

############GENDER################
zgender

zgendertest=matrix(scale(zgender,scale=FALSE),ncol=1)
Data2=list(p=3,lgtdata=lgtdata,Z=zgendertest)
testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2)
dim(testrun2$Deltadraw)
apply(testrun2$Deltadraw[701:1000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
deltadraw1 <- apply(testrun2$Deltadraw[701:1000,],c(2),mean)
deltadraw1
betadraw2=testrun2$betadraw
dim(betadraw2)

plot(1:length(betadraw2[1,1,]),betadraw2[1,1,])
plot(density(betadraw2[1,1,701:1000],width=2))
abline(v=0) ## vertical line
plot(density(betadraw2[1,2,701:1000],width=2))
abline(v=0) ## vertical line
abline(h=0.010) ## horizontal line
#### Compute the probability that person 1 beta 1 > 0 ####
#### Assumes Normal distribution of beta 1 ######
mn <- mean(betadraw2[1,1,701:1000])
sd <- sd(betadraw2[1,1,701:1000])
mn
sd
plot(density(betadraw2[1,1,701:1000],width=2))
abline(v=0) ## vertical line
abline(v=mn) ## vertical line
prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob
#### Compute the empirical probability based on sample values ###
p1b1 <- betadraw2[1,1,701:1000]
quantile(p1b1, probs = seq(0, 1, by= 0.1))

summary(betadraw2[1,1,701:1000]) ##log(odds ratio)=0.7833; odds ratio = exp(0.7833)=2.18;
summary(betadraw2[1,2,701:1000]) ##log(odds ratio)=-0.264; odds ratio = exp(-0.264)=0.76;
betameansoverall <- apply(betadraw2[,,701:1000],c(2),mean)
betameansoverall
perc <- apply(betadraw2[,,701:1000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc

plot(betadraw2[3, 2,])
abline(h=0)

df_loglike2 <- data.frame(draw=1:length(testrun2$loglike), 
                          loglike=testrun2$loglike)

ggplot(df_loglike2) + 
  geom_line(aes(y=loglike, x=draw)) +
  labs(x='Draw', y='Log Likelihood')




##################
##Chapter 4 ######
##################
betameans <- apply(betadraw1[,,701:1000],c(1,2),mean)
str(betameans)
head(betameans)
dim(betameans)
dim(t(betameans))
dim(X.matrix)
xbeta=X.matrix%*%t(betameans)
dim(xbeta)

xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)

expxbeta2=exp(xbeta2)

rsumvec=rowSums(expxbeta2)

pchoicemat=expxbeta2/rsumvec
head(pchoicemat)
dim(pchoicemat)

custchoice <- max.col(pchoicemat)
head(custchoice)
str(custchoice)

ydatavec <- as.vector(t(ydata))
str(ydatavec)
head(ydatavec)
table(custchoice,ydatavec)
require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)
auc(roctest)
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC)
logliketest <- testrun1$loglike
str(logliketest)
mean(logliketest)
hist(logliketest)
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
### Predicted frequencies for the 36 choice sets using individual models
apply(m2, 2, function(x){tabulate(na.omit(x))})

##betadraw2##
betameans2 <- apply(betadraw2[,,701:1000],c(1,2),mean)
str(betameans2)
head(betameans2)
dim(betameans2)
dim(t(betameans2))
dim(X.matrix)
xbeta=X.matrix%*%t(betameans2)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
head(pchoicemat)
dim(pchoicemat)
custchoice <- max.col(pchoicemat)
head(custchoice)
str(custchoice)
ydatavec <- as.vector(t(ydata))
str(ydatavec)
head(ydatavec)
table(custchoice,ydatavec)

require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)
auc(roctest)
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC)
logliketest2 <- testrun2$loglike
str(logliketest2)
mean(logliketest2)
hist(logliketest2)
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)

### Predicted frequencies for the 36 choice sets using individual models
apply(m2, 2, function(x){tabulate(na.omit(x))})


###########################################################
##Chapter 5 - evaluation of extra scenarios ######
###################################################################
ex_scen <- read.csv("extra-scenarios(1).csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])

### predict extra scenarios using the overall model  ###################
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
dim(betavec)
betavec
dim(Xextra.matrix)
xextrabeta=Xextra.matrix%*%(betavec)
xextrabeta
dim(xextrabeta)

xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
xbetaextra2
dim(xbetaextra2)

expxbetaextra2=exp(xbetaextra2)
expxbetaextra2

rsumvec=rowSums(expxbetaextra2)

pchoicemat=expxbetaextra2/rsumvec
pchoicemat
head(pchoicemat)
dim(pchoicemat)


betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
xbeta=X.matrix%*%(betavec)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
pchoicemat

betavec

pchoicemat2 <- round(pchoicemat*424,digits=0)
pchoicemat 



### predict extra scenarios based on individual models #############################
xextrabetaind=Xextra.matrix%*%(t(betameans))
xbetaextra2ind=matrix(xextrabetaind,ncol=3,byrow=TRUE)
xbetaextra2ind

#xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
#dim(xextrabetaind)
#xbetaextra2ind=rbind(matrix(xextrabetaind[1:3,],ncol=3,byrow=TRUE),
#                     matrix(xextrabetaind[4:6,],ncol=3,byrow=TRUE))
dim(xbetaextra2ind)


expxbetaextra2ind=exp(xbetaextra2ind)
rsumvecind=rowSums(expxbetaextra2ind)
pchoicematind=expxbetaextra2ind/rsumvecind
dim(pchoicematind)
head(pchoicematind)
pchoicematind

custchoiceind <- max.col(pchoicematind)
head(custchoiceind)
str(custchoiceind)
extra1 <- custchoiceind[1:424]
extra2 <- custchoiceind[425:848]
table(extra1)
table(extra2)


#accuracy based on confusion matrix for each of the 424 respondents using individual models
#for all the 36 choice sets - actual response vs predicte response ###############

resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoice[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
} 
plot(resp_accuracy, main = "Model Accuracy by Respondent")
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)
head(ydatadf)
rn <- rownames(ydatadf)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)


str(resp_all)
hist(resp_all$resp_accuracy)
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]





#####

