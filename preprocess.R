
library('ggplot2') # plots... not used at the moment
library('polynom') # what is this for?
library('plyr') # data scrub
#library(rjags)
#library(R2jags) # bayes model not seen in this file
library('glmnet') # for glm model
library('dplyr') #for data scrub
library("caret") #for dummy cariables
#library(VIM) # imputation, not used here
#library(mice) # na plot
library(corrplot) #cor plot
library('pROC') #roc calc
library(ROCR) # for bagging
library(foreach) # for bagging
library('e1071') # for svm
#-------------------
Logit <- function(p) log(p/(1-p))
InvLogit <- function(x) 1/(1+exp(-x))

DerivativeInvLogit <- function(x) 1/(1+exp(-x))^2*exp(-x)
DerivativeInvLogit <- function(x, beta0=0, beta1 =1) 1/(1+exp(-(beta0+beta1*x)))^2*beta1*exp(-(beta0+beta1*x))



# Import Data, Merge Data
Validation <- read.csv("test.csv")
states <- read.csv("free-zipcode-database-Primary.csv")
MergedTestData <- merge(Validation,states, by.x = "zip.code", by.y = "Zipcode", all.x = TRUE)
trav_test <- MergedTestData

# To train model
trav_orig <- read.csv('train.csv') # keep original set for NA analysis
MergedTestData <- merge(trav_orig,states, by.x = "zip.code", by.y = "Zipcode", all.x = TRUE)
trav_complete <- MergedTestData


## Data Cleaning

# NA imputation, DO THIS BEFORE OMIT.NA
mice_plot <- aggr(trav_orig, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(trav_orig), cex.axis=.7,
                  gap=1, ylab=c("Missing data","Pattern"))



### Imputation

# As you saw in the plot above the NAs were created in a pattern (artificial data)
# This is extra reason to impute the NAs
# Because of the pattern we no overlap among NAs
# This allows us to use other correlated variables to predict what the NAs might be
# Unfortunately for this part, very few of the variables are correlated

# Sales.Channel is correlated with our repsonse cancelation
# Here we see NA's have about the same rate of cancelation as, phone, and online, 
# we will impute these NAs by binning all of the above groups (this is sometiems bad practice, consider changing this later)
sum(is.na(trav_complete$sales.channel))

channel<-ifelse(trav_complete$sales.channel=="Broker", 1, 0)
channel[(is.na(channel))]<-0
#sum(is.na(channel)) #just to check
trav_complete$channel<-channel

### Erroneous data

# fixing erroneous cancel entry of -1
summary(trav_complete$cancel)
neg <- trav_complete$cancel==-1
trav_complete$cancel[neg] <- NA
# fixing erroneous age entry
# Lets assume age was typed in wrong if over 117 for example 159 could be 59 or 225 could be 25
# chose 117 because someone who is 18 might have retners insurance if 118=18 this is room for possible typo
a<-trav_complete$ni.age>117
trav_complete$ni.age[a]<-NA

## Removal
# We need to remove the merged data first because variables such as City had 1000s of NAs
names(trav_complete)
# OK now we can finally omit the remaining NAs, amros merged set has a bunch of nas so we need to remove all those columns first


trav_complete<-trav_complete[,c("cancel","credit","channel","claim.ind","ni.age","tenure","State","year","ni.marital.status",
                                "n.children","len.at.res","n.adults","dwelling.type","house.color","coverage.type",'premium','zip.code')]
#"premium","City","len.at.res","dwelling.type","n.adults","EstimatedPopulation" for use later
sum(is.na(trav_complete))
colnames(trav_complete)[7]<-"state"
trav_complete<-na.omit(trav_complete)
## Variable Selection
#The end goal is to dummy encode in order to select reference categories
#We must start by making data categorical
#We will use out-of-fold likelihood method
#Making continuous variables discrete in order to reveal trends


### Adults

# cancellation rate for each category
rate

# Lets bin 5+ adults to make a decent size bin
trav_complete <- trav_complete %>% mutate(n.adults=cut(n.adults, breaks=c(-Inf,3,4,5, Inf), labels=c(1,2,3,4)))

### Length of Residence
trav_complete <- trav_complete %>% mutate(len.at.res=cut(len.at.res, breaks=c(-Inf,10,18,Inf), labels=c(seq(1,3,1))))

### Dwelling Type

# Dwelling type ~ tenant
tenant<-ifelse(trav_complete$dwelling.type=="Tenant", 1, 0)
trav_complete$tenant<-tenant

# Our intuition took us here to look for a relationship between dwelling and length at residence
# our intuition says length at residence wont matter if they are a renter
trav_complete <- trav_complete %>% mutate(length2=cut(len.at.res, breaks=c(-Inf, seq(7,21,1), Inf), labels=c(seq(1,16,1))))

### Children

trav_complete <- trav_complete %>% mutate(n.children=cut(n.children, breaks=c(-Inf,2,5,9,Inf), labels=c(1,2,3,4)))

### Age

trav_complete <- trav_complete %>% mutate(ni.age=cut(ni.age, breaks=c(-Inf, 35, 55, Inf), labels=c(1,2,3)))


### Tenure


trav_complete <- trav_complete %>% mutate(tenure=cut(tenure, breaks=c(-Inf,8, 11, Inf), labels=c(1,3,2)))

### state
state<-trav_complete$state
zip.code <- trav_complete$zip.code
trav_complete$state<-as.character(trav_complete$state)



b<-trav_complete$state=="WA"
c<-trav_complete$state=="AZ"
trav_complete$state[b|c] <- "WA/AZ"



a<-trav_complete$state=="IA"
b<-trav_complete$state=="CO"
trav_complete$state[a|b] <- "IA/CO"


trav_complete$state <- as.factor(trav_complete$state)



### Premium


trav_complete <- trav_complete %>% mutate(premium=cut(premium, breaks=c(-Inf,852,966,Inf), labels=c(seq(1,3,1))))
out<-ifelse(trav_complete$premium==2,1,0)
trav_complete$premium<-out





trav_complete$zip.code <- NULL
# Dummy set for frequentist methods
cats<-sapply(trav_complete, factor)
dummy<-dummyVars(~.,data=cats, fullRank = TRUE)
dummyset<-as.data.frame(predict(dummy, cats))


# Dummy set for baysian methods
bay <- trav_complete
bay$state <- NULL
bay$year <- NULL
cats<-sapply(bay, factor)
dummy<-dummyVars(~.,data=bay, fullRank = TRUE)
dummyset_bayes<-as.data.frame(predict(dummy, bay))
dummyset_bayes$state <- state
dummyset_bayes$zip_code <- zip.code






cats <- trav_complete
cats$credit<-factor(as.numeric(cats$credit))
levels(cats$credit)<-c(0,1,2)
levels(cats$n.children)<-c(0,1,2,3)
levels(cats$ni.age)<-c(0,1,2)





