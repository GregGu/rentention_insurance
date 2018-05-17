#---------------------
# Miscellaneous functions
# Leontine Alkema and Sanqian Zhang, 2015
#---------------------

#----
# function for binned residual plots from GH
#-----
binned.resids <- function (x, # what to bin over? 
                           y, # what to bin, eg. residuals
                           nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}

#####REQUIRES#######
#yhat1.i <- mod1$BUGSoutput$summary[paste0("p.i[", seq(1,n), "]"), "mean"]
#yhat2.i <- mod2$BUGSoutput$summary[paste0("p.i[", seq(1,n), "]"), "mean"]


#------
GetSplines <- function( # Get B-splines
  x.i, ##<< Vector of x-values (without NAs) for which splines need to be calculated (determines the number of rows of B.ik)
  x0 = NULL, ##<< x-value which determines knot placement. By default, knot is placed half-interval before last observation
  I = 2.5, ##<< Interval length between two knots during observation period
  degree = 3 # currently tested only with degree 3
) {
  if (is.null(x0)) {
    x0 <- max(x.i)-0.5*I 
  } 
  # get knots, given that one knot needs to be in year0
  knots <- seq(x0-1000*I, x0+1000*I, I) 
  while (min(x.i) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
  while (max(x.i) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I, 
                                                                knots[length(knots)]+1000*I, I)) 
  Btemp.ik <- bs(x.i, knots = knots[-c(1, length(knots))],  degree = degree,
                 Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.ik, 2, sum) > 0)
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.ik <- Btemp.ik[,startnonzerocol:endnonzerocol]
  colnames(B.ik) <- paste0("spline", seq(1, dim(B.ik)[2]))
  knots.k <- knots[startnonzerocol:endnonzerocol]
  names(knots.k) <- paste0("spline", seq(1, dim(B.ik)[2]))
  ##value<< List of B-splines containing:
  return(list(B.ik = B.ik, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = knots.k ##<< Vector of knots.
  ))
}
#---------------
# add CIs to a plot
AddCIs <- function(CI.low.t, # lower bound for seq.years.t
                   CI.up.t, # upper bound for seq.years.t
                   seq.years.t, col = 1){
  # add CIs to a plot.
  col = adjustcolor(col, alpha.f = 0.1)
  for (t in 2:length(seq.years.t))
    polygon(c(seq.years.t[t-1], seq.years.t[t-1], seq.years.t[t], seq.years.t[t],seq.years.t[t-1]),
            c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
            col=col, border = NA)
}

#---------------
PlotTrace <- function(#Traceplot for one parameter
  ### Trace plot for one parameter and add loess smoother for each chain
  parname, mcmc.array,##<< needs to be 3-dimensional array!
  n.chains= NULL, n.sim= NULL, main = NULL){
  if (is.null(main)) main <- parname
  if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
  if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
       ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
  for (chain in 1:n.chains){
    lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
  }
  for (chain in 1:n.chains){
    curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x), lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
}
#---------------
PlotPostOnly <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample
  post.samp, parname = NULL##<<used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
}

PlotPostWithNormalPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priormean, priorsd, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")

PlotPostWithUnifPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priorlow, priorup, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "",xlim = c(minx, maxx))
  h <- 1/(priorup-priorlow)
  segments(priorlow, h, priorup, h, col = 2)
}
#PlotPostWithUnifPrior(post.samp = rnorm(100,0), priorlow = -2, priorup = 10, parname = "bla")

#------------------
# WAIC
# adapted from Vehtari and Gelman 2014
waic <- function(log_lik){ # log-like should be the log(p(y_i|theta^(s)) with i=1,..,n rows and s=1,2,..,S columns
  log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}

colVars <- function(a){ 
  n <-   dim(a)[[1]]; 
  c <- dim(a)[[2]]; 
  return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, 
                                ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}

InvLogit <- function(x) 1/(1+exp(-x))

#' Read Covariates
#' 
#' function to read covariates
#' 
#' @param x: from MMEIG 2014 (but changed name to reflect it refers to 1 per ..)
#'
#' @section Note: LR input file should be in natural scale (i.e., not "1 in xxxx")
#'
#' @section Details: 
#' LR.1in is rounded as follows:
#'
#'    < 100       rounded to nearest 1
#'
#'    100-999.99  rounded to nearest 10
#'
#'    >= 1000     rounded to nearest 100
#'
#' @return z, the rounded number
#' 
#' @examples
#' 
roundandmake1in.LR <- function(x) {# from MMEIG 2014 (but changed name to reflect it refers to 1 per ..)
  # LR input file should be in natural scale (i.e., not "1 in xxxx")
  # LR.1in rounded as follows:
  #    < 100       rounded to nearest 1
  #    100-999.99  rounded to nearest 10
  #    >= 1000     rounded to nearest 100
  y <- 1/x
  z <- ifelse(y<1e2, round(y), ifelse(y>=1e2 & y<1e3, 10*round(y/10), 100*round(y/100)))
  return(z) }

round.LR <- function(x) { # and times 1000
  # LR input file should be in natural scale (i.e., not "1 in xxxx")
  y <- x*1000
  z <- ifelse(y<1, round(y,2), round(y))
  return(z) 
}

round.MMR <- function(x) {
  # MMR input file should be in natural scale (i.e., not x 100,000)
  # 1e5*MMR rounded as follows:
  #    < 100       rounded to nearest 1
  #    100-999.99  rounded to nearest 1
  #    >= 1000     rounded to nearest 10
  y <- 1e5*x
  z <- ifelse(y<1e2, round(y), ifelse(y>=1e2 & y<1e3, round(y), 10*round(y/10)))
  return(z) }

round.MatDth <- function(y) {
  z <- ifelse(y<1e2, round(y), ifelse(y>=1e2 & y<1e3, 10*round(y/10), ifelse(y>=1e3 & y<1e4, 100*round(y/100), 1000*round(y/1000))))
  return(z) 
}

#--------------
GetStartAndEnd <- function(startsall, endsall){
  # startsall and endsall are the start and end indices for (potentially overlapping) inquiries
  # WITH LENGTH > 1!!!
  # get unique starts and for each unique start, the last year that is observed within an inquiry,
  # before another unique start date
  yearsincluded <- startsall[1]:endsall[1]
  for (i in 2:length(startsall)){
    yearsincluded <- c(yearsincluded, startsall[i]:endsall[i])
  }
  starts <- sort(unique(startsall))
  ends <- NULL
  for (i in 1:(-1+length(starts))){
    ends <- c(ends, 
              seq(starts[i],starts[i+1]-1)[sum(cumprod(is.element(seq(starts[i],starts[i+1]-1),yearsincluded)))])
  }                
  ends[length(starts)] <- max(yearsincluded)
  return(list(starts = starts, ends = ends))
}
#GetStartAndEnd(startsall = c(1,10), endsall = c(12,11))
#GetStartAndEnd(startsall = c(1,10), endsall = c(3,11))
#GetStartAndEnd(startsall = c(1,8,10), endsall = c(12,10,11))
#GetStartAndEnd(startsall = c(1,1,10), endsall = c(12,10,11))
#GetStartAndEnd(startsall = c(1,2,3), endsall = c(1,2,3))
#GetStartAndEnd(startsall = rev(c(1,2,3)), endsall = rev(c(1,2,3)))

#----------
MakeDirs <- function(runname){
  output.dir <- outputdir <- paste0(getwd(), "/output/", runname, "/")
  dir.create("output/", showWarnings = FALSE)
  dir.create(outputdir, showWarnings = FALSE)
#  dir.create(paste0(outputdir, "/temp.JAGSobjects/"), showWarnings = FALSE)
  dir.create(paste0(outputdir, "/fig/"), showWarnings = FALSE)
  return(output.dir)
}

#--------------------------
logit <- function(x){
  log(x/(1-x))
}
inverselogit <- function(y) 1/(1+exp(-y))

#----
LogitMinMax <- function(delta, mindelta = 1, maxdelta = 4){
  log((delta-mindelta)/(maxdelta-delta))
}
InverseLogitMinMax <- function(logtrdelta, mindelta = 1, maxdelta = 4){
  mindelta+(maxdelta - mindelta)/(1+exp(-logtrdelta))
}

#----
GetTrend <- function(startvalues, # vector of start values
                     endvalues, # vector of end values
                     lengths, #vector of length of intervals
                     method="discrete") {
  # Computes annual rate of decline by two methods
  #    "continuous" for average instantaneous rate of decline (annualized)
  #    "discrete"   for average annual rate of decline
  # Continuous method used for 2008 estimates, discrete method for 2010 estimates
  A1 <- startvalues
  A2 <- endvalues
  t.int <- lengths
  if (!method %in% c("continuous", "discrete"))
    if (length(A1)!=length(A2) | length(A1)!=length(t.int)) { print("Error: length of A1, A2, and t.int vectors must be the same"); break }
  r <- if (method=="continuous") { - log(A2/A1) / t.int } else if (method=="discrete"  ) { 1 - (A2/A1)^(1/t.int) }
  return(r) 
}
#GetTrend(startvalues, endvalues, lengths)

#------------------------
GetNumbersPerYear <- function(start, # exact start time e.g 1991.1 
                              end, # exact end time, e.g. 1999.0 
                              tosum.t, # whatever needs to be (partially) added, in vector where years correspond to calyear.t 
                              calyear.t, # cal year thus rounded to 0, e.g. 1990, 1991 etc
                              combiNAandnonNAok = FALSE
){
  years.j <- floor(start):ceiling(end-1)
  J <- length(years.j)
  if (!combiNAandnonNAok){
    # all NA if one is missing!!!
    if (prod(is.element(years.j, calyear.t))!=1) {
      number.j = rep(NA, J)
      return(number.j)
    }
  }
  number.j <- rep(NA, J)
  if (J==1) {
    number.j[1] <- (end - start)*tosum.t[calyear.t==years.j[1]]
    return(number.j)
  } 
  # J>1:
  number.j[1] <- (1-(start - floor(start)))*tosum.t[calyear.t==years.j[1]]
  number.j[J] <- (end- ceiling(end-1))*tosum.t[calyear.t==years.j[J]]
  # note: end - floor(end) doesn't work!!
  if (J>2){
    # perhaps not sorted so don't use number.j[2:(J-1)] <- tosum.t[calyear.t==years.j[2:(J-1)]]
    for (j in 2:(J-1)){
      number.j[j] <- tosum.t[calyear.t==years.j[j]]
    }
  }
  return(number.j)
}
#GetNumbersPerYear(start,end, tosum.t, calyear.t)
# GetNumbersPerYear(1990.5, 1990.7, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990, 1991, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1991, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1992, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1992, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990, 1992.5, seq(1,11), seq(1990,2000))

#-------------------------------
GetSums <- function(start, # exact start time e.g 1991.1 
                    end, # exact end time, e.g. 1999.0 
                    tosum.t, # whatever needs to be added, in vector where years correspond to calyear.t 
                    calyear.t, # cal year thus rounded to 0, e.g. 1990, 1991 etc
                    # LA, Sep 28
                    na.rm = FALSE # when TRUE, does give NA if all inputs are NA (instead of 0)
){
  # to avoid problems, check to make sure it's not all NAs when na.rm = T (then sum is 0 but should be NA)
  if (na.rm){
    if (sum(is.element(calyear.t, floor(start):ceiling(end-1)))==0){
      return(NA)
    }
  }
  
  # first add up all sum.t from start year to last calendar year, floor(start):ceiling(end-1)
  # use ceiling(end-1) for last calendar year because eg end = 1991 means 1990 was last calendar year
  # then subtract partial stuff
  sums <- sum(tosum.t[is.element(calyear.t, floor(start):ceiling(end-1))], na.rm = na.rm)
  if (start > floor(start)) sums <- sums  -   (start - floor(start))*tosum.t[calyear.t==floor(start)]
  if (end > floor(end)) sums <- sums - (ceiling(end) - end)*tosum.t[calyear.t==floor(end)]
  return(sums)
}
#GetSums(start,end, tosum.t, calyear.t)
#GetSums(1990, 1991, rep(1,11), seq(1990,2000))
#GetSums(1990.5, 1991, rep(1,11), seq(1990,2000))
#GetSums(1990.5, 1990.7, rep(1,11), seq(1990,2000))

#-----------------------
# functions related to SEs

#functions used to get stoch error for PM
# does not work (and is not used) for zero entries
GetSEPM <- function(pm, env){
  se <- sqrt(pm*(1-pm)/env)
  return(se)
}
GetSElogPM <- function(pm, env){
  seforlogpm <- sqrt((1-pm)/(pm*env))
  return(seforlogpm)
}
# function used for incorporating the variance of the adjustment parameter 
# into the total error variance for VR obs
GetSElogPMVRinclMultUnc <- function(pm, ## the vr-based (unadjusted) PM
                                    mult = NULL, env){
  se_final <- rep(NA, length(pm)) # for PM
  if (is.null(mult)) mult <- rep(1, length(pm))
  S <- 10000
  for (j in 1:length(pm)){
    set.seed(1234)
    delta.s <- exp(rnorm(S, log(mult[j]), 0.25))
    pmhat.s <- pm[j]*mult[j]/delta.s 
    matdeaths.s <- rbinom(S, size = round(env[j]), prob = pmhat.s)
    se_final[j] <- sd(matdeaths.s/round(env[j]))
  } 
  se_forlog <- GetSElogPMFromSEforPM(se_final, pm)
  return(se_forlog)
}

GetSElogPMFromSEforPM <- function(se, pm){
  return(se/pm)
}

#-----------------------
Gett.i <- function(years.i, year.t ){
  gett.i <- rep(NA, length(years.i))
  for (i in 1:length(years.i)){
    gett.i[i] <- which(year.t == years.i[i])
  }
  return(gett.i)
}


#-----------------------
Getc.i <- function(iso.i, iso.c){
  getc.i <- rep(NA, length(iso.i))
  for (i in 1:length(iso.i)){
    getc.i[i] <- which(iso.c == iso.i[i])
  }
  return(getc.i)
}


#-------------------------------------------
GetMMRate <- function(mmr=NULL,matdeath=NULL,births=NULL,women=NULL){
  if(is.null(matdeath)){
    return(mmr*births/women)
  }else{
    return(matdeaths/women)
  }
  
}
GetLTR <- function(mmrate,t15t50,l15){
  return(mmrate*t15t50/l15)
}

#-----------------------
# SetUpOutputDir <- function(runname = "test",
#                            meta.file = "data/interm/meta.rda",
#                            dat.file = "data/interm/datall.rda"
# ){
#   output.dir <- MakeDirs(runname)
#   load(meta.file)
#   save(meta, file = paste0(output.dir, "meta.rda"))
#   load(dat.file)
#   save(datall, file = paste0(output.dir, "datall.rda"))
# }

#--------
GetPartialTime <- function(start, end, X){
  partialtime.x <- rep(NA, X)
  if (X ==1){
    partialtime.x <- end-start
  } else { # end is at least one calendar year after start
    partialtime.x[1] <- 1-(start - floor(start))
    partialtime.x[X] <- end - ceiling(end-1)
    ### note to self: end.j[j] - floor(end.j[j]) is not correct: e.g. 1995.0-1997.0 
    if (X>2) partialtime.x[2:(X-1)] <- 1
  } 
  return(partialtime.x)
}

#-------------------------------------------------------------------------------------
WriteCountryCsvs <- function(
  CIs, 
  csvdir = output.dir){
  names.Li <- lapply(strsplit(names(CIs), split = "[.]"), function(l) l[1])
  for (i in 1:length(CIs)){
    WriteCsv(res = CIs[[i]], 
             filename = paste0(csvdir, "/", names.Li[[i]], ".csv"))
  }
}

#-------------------------------------------------------------------------------------
WriteCsv <- function(# Save csv's with CIs 
  res, # 3Darray or matrix or vector 
  filename, # where to write the csv to?
  namesdim = c("country", "percentile", "year") #for cqt (not used for dim < 3)
){
  toprint <- res
  if (length(dim(res))>2){
    # array with dimension 3
    # lacking quicker idea right now so slow for-loop
    toprint <- matrix(NA, prod(dim(res)[1:2]), 2+dim(res)[3])
    colnames(toprint) <- seq(1, dim(toprint)[2])
    if (!is.null(namesdim)) colnames(toprint)[1:2] <- namesdim[1:2]
    colnames(toprint)[-c(1,2)] <- dimnames(res)[[3]]
    i <- 0
    for (x in 1:dim(res)[1]){
      for (y in 1:dim(res)[2]){
        i <- i+1
        toprint[i,] <- c(dimnames(res)[[1]][x], dimnames(res)[[2]][y], res[x,y,])
      }}
  }
  write.csv(toprint, file = filename, row.names  = ifelse(length(dim(res))==2, TRUE, FALSE))
  return(invisible())
}

#-----------------------
GetPMfromME<-function(mat,env){#},matconstant=0.05,envconstant=1){
  #pm<-ifelse(mat==0,(mat+matconstant)/(env+envconstant),mat/env)
  pm<-mat/env
  pm
}
#-------------
# The End!