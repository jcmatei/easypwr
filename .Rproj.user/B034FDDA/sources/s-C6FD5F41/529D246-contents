
fcol <- c("sdA0","sdA1", "sdB0", "sdB1", "rho", "alpha")

#'Creates a data frame and plot of grouped observations for missing sample sizes based on power.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by power and groups observations by similar non power-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param power A number.
#'@return A data frame of modeled sample size and plot of sample size vs power.
#'@examples
#'plot.sampfind.p(2,2,2,2,.5,.05,c(57,55,56),54,c(.5,.6,.7,.8))
#'@export
plot.sampfind.p <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power))
  (ldf <- nrow(df))
  (df[1, 10] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, -9:-10], df[i, 1:8])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,10] <- 1 + max(c(df[1:(i-1), 10]))
    } else {
      df[i,10] <- df[match(1, matchvec), 10]
    }
  }

  (nvec <- apply(df,1,sep_solve_np))
  df[11] <- nvec
  scol <- c("m1","m2","power","group","sample")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(n = nvec, power = df[,9], group = df[,10]))
  p1 <- ggplot(plotdf, aes(x=power, y = nvec, color= as.factor(group))) +
    geom_line() + labs(x = "Power", y = "Sample Size", color = "Group")
  return(list(plot=p1, df=df))
}


#'Creates a data frame and plot of grouped observations for missing sample sizes based on difference of means.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by mean difference and groups observations by similar non mean difference-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param power A number.
#'@param m1 A number.
#'@param m2 A number.
#'@returns A data frame of modeled sample size and plot of sample size vs mean difference.
#'@examples
#'plot.sampfind.d(2,2,2,2,.5,.05,c(57,55,56),c(53,56,54),.8)
#'@export
plot.sampfind.d <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,power){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,m1,m2))
  (ldf <- nrow(df))
  (df[1, 10] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, -8:-10], df[i, 1:7])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,10] <- 1 + max(c(df[1:(i-1), 10]))
    } else {
      df[i,10] <- df[match(1, matchvec), 10]
    }
  }

  (nvec <- apply(df,1,sep_solve_nd))
  df[11] <- nvec
  scol <- c("m1","m2","power","group","sample")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(n = nvec, diff = abs(df[,8] - df[,9]), group = df[,10]))
  p1 <- ggplot(plotdf, aes(x=diff, y = nvec, color= as.factor(group))) +
    geom_line() + labs(x = "Difference of Means", y = "Sample Size", color = "Group")
  return(list(plot=p1, df=df))
}


#'Creates a data frame and plot of grouped observations for missing powers based on difference of sample size.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by sample size and groups observations by similar non sample-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param n A number.
#'@returns A data frame of modeled power and plot of power vs sample size.
#'@examples
#'plot.pwrfind.n(2,2,2,2,.5,.05,54,55,c(60,64,68))
#'@export
plot.pwrfind.n <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n))
  (ldf <- nrow(df))
  (df[1, 10] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, -9:-10], df[i, 1:8])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,10] <- 1 + max(c(df[1:(i-1), 10]))
    } else {
      df[i,10] <- df[match(1, matchvec), 10]
    }
  }

  (pvec <- apply(df,1,sep_solve_pn))
  df[11] <- pvec
  scol <- c("m1","m2","sample","group","power")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(power = pvec, sample = df[,9], group = df[,10]))
  p1 <- ggplot(plotdf, aes(x=sample, y = pvec, color= as.factor(group))) +
    geom_line() + labs(x = "Sample Size", y = "Power", color = "Group")
  return(list(plot=p1, df=df))
}

#'Creates a data frame and plot of grouped observations for missing powers based on difference of means.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by mean difference and groups observations by similar non mean difference-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param m1 A number.
#'@param m2 A number.
#'@param n A number.
#'@returns A data frame of modeled power and plot of power vs mean difference.
#'@examples
#'plot.pwrfind.d(2,2,2,2,.5,.05,c(57,55,56),c(53,56,54),64)
#'@export
plot.pwrfind.d <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,m1,m2,n){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,n,m1,m2))
  (ldf <- nrow(df))
  (df[1, 10] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, -8:-10], df[i, 1:7])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,10] <- 1 + max(c(df[1:(i-1), 10]))
    } else {
      df[i,10] <- df[match(1, matchvec), 10]
    }
  }

  (pvec <- apply(df,1,sep_solve_pd))
  df[11] <- pvec
  scol <- c("m1","m2","sample","group","power")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(power = pvec, diff = abs(df[,8] - df[,9]), group = df[,10]))
  p1 <- ggplot(plotdf, aes(x=diff, y = pvec, color= as.factor(group))) +
    geom_line() + labs(x = "Difference of Means", y = "Power", color = "Group")
  return(list(plot=p1, df=df))
}


#'Creates a data frame and plot of grouped observations for missing mean differences based on sample size.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by sample size and groups observations by similar non sample-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param power A number.
#'@param n A number.
#'@returns A data frame of modeled mean difference and plot of sample size vs mean difference.
#'@examples
#'plot.diffind.n(2,2,2,2,.5,.05,.8,c(60,64,68))
#'@export
plot.diffind.n <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,n){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,n))
  (ldf <- nrow(df))
  (df[1, 9] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, -8:-9], df[i, 1:7])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,9] <- 1 + max(c(df[1:(i-1), 9]))
    } else {
      df[i,9] <- df[match(1, matchvec), 9]
    }
  }

  (dvec <- apply(df,1,sep_solve_dn))
  df[10] <- dvec
  scol <- c("power","sample","group","difference")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(diff = dvec, n = df[,8], group = df[,9]))
  p1 <- ggplot(plotdf, aes(x=n, y = diff, color= as.factor(group))) +
    geom_line() + labs(x = "Sample Size", y = "Difference of Means", color = "Group")
  return(list(plot=p1, df=df))
}

#'Creates a data frame and plot of grouped observations for missing mean differences based on power.
#'
#'This function takes numbers and vectors, turns them into a data frame and solves for every combination of input.  It also plots output by power and groups observations by similar non power-inputs.
#'@param sdA0 A number.
#'@param sdA1 A number.
#'@param sdB0 A number.
#'@param sdB1 A number.
#'@param r1 A number.
#'@param alpha A number.
#'@param power A number.
#'@param n A number.
#'@returns A data frame of modeled mean difference and plot of power vs mean difference.
#'@examples
#'plot.diffind.p(2,2,2,2,.5,.05,c(.6,.7,.8),64,)
#'@export
plot.diffind.p <- function(sdA0,sdA1,sdB0,sdB1,r1,alpha,power,n){

  (df <- expand.grid(sdA0,sdA1,sdB0,sdB1,r1,alpha,n,power))
  (ldf <- nrow(df))
  (df[1, 9] <- 1)

  for (i in 2:ldf) {
    fmatchvec <- row.match(df[1:i, 1:7], df[i, 1:7])
    matchvec <- fmatchvec[-i]

    if(all(is.na(matchvec) == TRUE)){
      df[i,9] <- 1 + max(c(df[1:(i-1), 9]))
    } else {
      df[i,9] <- df[match(1, matchvec), 9]
    }
  }

  (dvec <- apply(df,1,sep_solve_dp))
  df[10] <- dvec
  scol <- c("power","sample","group","difference")
  ncol <- append(fcol,scol, after = 6)
  colnames(df) <- ncol
  (plotdf <- data.frame(diff = dvec, power = df[,8], group = df[,9]))
  p1 <- ggplot(plotdf, aes(x=plotdf[,2], y = plotdf[,1], color= as.factor(plotdf[,3]))) +
    geom_line()+ labs(x = "Power", y = "Difference of Means", color = "Group")
  return(list(plot=p1, df=df))
}
