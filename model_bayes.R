# jagsUI
# autojags(parallel = T, n.cores = 4)
# if jags.parallel -> progress.bar = "gui"
library(jagsUI)

#include code that removes a few obs from chosen data in order to make even folds before this code runes, this will all be added to pre proc
chosen_data <- read.csv("dummyset_bayes.csv") # choose the data for our model script
chosen_data <- chosen_data[-c(1:8),]

##
area <- read.csv("free-zipcode-database-Primary.csv")

select <- which(is.element(area$Zipcode, chosen_data$zip_code))
area <- area[select, ]

area$zip_factor <- as.numeric(as.factor(area$Zipcode))
area<-area[order(area$zip_factor),]
gets.z <- as.numeric(factor(area$State))

name.z <- area$Zipcode
Z <- length(name.z)
getz.i <- as.numeric(as.factor(chosen_data$zip_code))
N <- nrow(chosen_data)
S <- length(unique(gets.z))
jagsx <- chosen_data[,is.element(names(chosen_data), c('n.adults.4',          'claim.ind.1',         'len.at.res.2',                    'len.at.res.3', 
                               'ni.age.3',            'year.2014',         'n.children.3',        'credit.medium',            'channel.1',
                               'n.children.4',           'credit.low'))]
jagsx <- as.matrix(jagsx)

#to get the training data.i
cvfold <- 10
fold_size <-nrow(chosen_data)/cvfold
set.seed(690)
chosen_data <- mutate(chosen_data, index = sample(rep(1:cvfold, each = fold_size)))
chosen_data$ind <- seq(1:nrow(chosen_data))
#i=1
#normally we have a for loop here
train <- subset(subset(filter(chosen_data, index != i), select = -index),select=ind)
getitrain.j <- train$ind
test.cancel <- filter(chosen_data, index == i)$cancel.1
ntrain = length(getitrain.j)
jagsdata <- list(gets.z = gets.z, getz.i = getz.i,
                 N=N, S=S, Z=Z, y.i=chosen_data$cancel,
                 x=jagsx, getitrain.j = getitrain.j,
                 ntrain=ntrain)
parnames <- c("alpha.s","sigma.s","B1","B2","yrep.i")
mod0 <-jags.parallel(data = jagsdata, 
                     parameters.to.save=parnames,
                     n.burnin = 1000, n.iter= 4000, n.thin = 3,
                     n.chains = 3,
                     model.file = "model_val.txt")

# point estimates of the county means






max(mod0$BUGSoutput$summary[, "Rhat"]) # not yet ok for final inference but ok for first checking
which.max(mod0$BUGSoutput$summary[, "Rhat"])

mcmc.array <- mod0$BUGSoutput$sims.array
PlotTrace("sigma.s", mcmc.array)
PlotTrace("B1", mcmc.array)
PlotTrace("B2", mcmc.array)

mod0$BUGSoutput$summary[, ""]

# indices of test set
getitest <- setdiff(seq(1,N), getitrain.j)
# errors
errors <- y.i[getitest] - mod0$BUGSoutput$summary[paste0("yrep.i[",getitest,"]"), "mean"]
hist(errors)
mean(errors)  
median(errors)  
mean(abs(errors))
median(abs(errors))

