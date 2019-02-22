#' @title Exponential smoothing
#'
#' @description Exponential smoothing of (possibly non-equidistant)
#'     time series.
#'
#' @name esmooth
#'
#' @param yvar Time series, a numeric vector.
#' @param tvar Time variable, a numeric vector. Defaults to 1, 2, ...
#' @param alpha Smoothing parameter; if NULL, alpha will be estimated
#'     by minimizing a prediction error.
#' @param init Intial value for the first smoothed value; defaults to
#'     the average of the first three observations.
#' @param fit Should smoothing parameter be fitted to data?
#' @param filter. Experimental feature, please do not use.
#'
#' @examples
#'
#' yvar <- aggregate(co2)
#' tvar <- seq_along(yvar)
#' f1 <- ses(yvar, tvar)
#' f2 <- des(yvar, tvar)
#'
#' at <- 1 + seq(0, 35, by=5)
#'
#' par(mfrow=c(2,1))
#' plot(f1)
#' forecast_lines(f1, at=at, ahead=0:5, col='red', lwd=3)
#' forecast_lines(f2, at=at, ahead=0:5, col='blue', lwd=3)
#'
#' ## Add more noise
#' yvar2 <- yvar + rnorm(length(yvar), sd=40)
#' f1 <- ses(yvar2, tvar)
#' f2 <- des(yvar2, tvar)
#'
#' plot(f1)
#' at <- 1 + seq(0, 35, by=5)
#' forecast_lines(f1, at=at, ahead=0:5, col='red', lwd=3)
#' forecast_lines(f2, at=at, ahead=0:5, col='blue', lwd=3)
#' 

#' @rdname esmooth
ses <- function(yvar, tvar=NULL, alpha=NULL, init=NULL, fit=TRUE, n.ahead=1, filter.=NULL){
    obj <- .SES(yvar, tvar, alpha, init, filter.)
    if (!fit) obj
    else update(obj, alpha=es_fit(obj, n.ahead=n.ahead))
}

#' @rdname esmooth
des <- function(yvar, tvar=NULL, alpha=NULL, init=NULL, fit=TRUE, n.ahead=1, filter.=NULL){
    obj <- .DES(yvar, tvar, alpha, init, filter.)
    if (!fit) obj
    else update(obj, alpha=es_fit(obj, n.ahead=n.ahead))    
}


.SES <- function(yvar, tvar=NULL, alpha=NULL, init=NULL, filter.=NULL){
    
    if (is.null(tvar)) tvar <- seq_along(yvar)
    if (is.null(alpha)) alpha <- .5 ##.getalpha(yvar, tvar, n.ahead=1, FUN.=.SES)
    
    na.y  <- is.na(yvar)
    tvar2 <- tvar[!na.y]
    yvar2 <- yvar[!na.y]
    
    alpha <- c(alpha, alpha)[1:2]
    wgt2  <- do.call(cbind, lapply(alpha, .seswgt, tvar2)) ## Now wgt2 is n x 2 matrix
    
    if (is.null(init))
        init <- mean(na.omit(yvar2)[1:3])

    S1    <- .ses.core(yvar2, wgt2, init=init)
    y.fit <- rep(NA, length(yvar))
    y.fit[!na.y] <- S1
    
    if (is.na(y.fit[1])) y.fit[1] <- init
        
    ## Forecast missing for ses; Kræver y.fit
    for (ii in 2:length(y.fit)){
        if (is.na(y.fit[ii])){
            y.fit[ii] <- y.fit[ii - 1]
        }
    }   
    
    if (!is.null(filter.)){
        filter. <- filter. / sum(filter.)
        y.fit   <- as.numeric(filter(y.fit, filter., sides=1))
    }
    
    ans <- list(x=tvar, y=y.fit, y.obs=yvar, init=init, alpha=alpha[1], cls=".SES")
    class(ans) <- c("SES", "ES", "list")
    ans
}


.DES <- function(yvar, tvar=NULL, alpha=NULL, init=NULL, filter.=NULL){

    if (is.null(tvar)) tvar <- seq_along(yvar)
    if (is.null(alpha)) alpha <- .5 ## .getalpha(yvar, tvar, n.ahead=1, FUN.=.DES)
    
    na.y  <- is.na(yvar)
    tvar2 <- tvar[!na.y]
    yvar2 <- yvar[!na.y]
    
    alpha <- c(alpha, alpha)[1:2]
    wgt2  <- do.call(cbind, lapply(alpha, .seswgt, tvar2)) ## Now wgt2 is n x 2 matrix
    
    if (is.null(init))
        init <- mean(na.omit(yvar2)[1:3])


    
    S1  <- .ses.core(yvar2, wgt2, init=init)
    S2  <- .ses.core(S1, wgt2, init=S1[1])    
    xx2 <- 2 * S1 - S2
    
    ##bb2 <- (S1 - S2)*wgt2/(1-wgt2)
    ## SHD: Tricky when wgt2 is vector valued; this is certainly a hack
    wgt2ave <- rowMeans(wgt2)
    bb2 <- (S1 - S2) * wgt2ave / (1 - wgt2ave)
    
    bb <- xx <- rep(NA, length(yvar))
    xx[!is.na(yvar)] <- xx2
    bb[!is.na(yvar)] <- bb2
    
    ## Forecast missing for des 
    y.forec <- rep(NA, length(yvar))
    last    <- 1
    if (is.na(xx[last])) {
        xx.last <- init
        bb.last <- 0
    } else {
        xx.last <- xx[last]
        bb.last <- bb[last]
    }
    for (ii in 1:length(y.forec)){	
        if (is.na(xx[ii])){
            y.forec[ii] <- xx.last + (tvar[ii] - tvar[last]) * bb.last
        } else {
            last <- ii
            xx.last <- xx[last]
            bb.last <- bb[last]
        }
    }
    
    y.fit <- xx
    y.fit[is.na(y.fit)] <- y.forec[!is.na(y.forec)]
    
    if (!is.null(filter.)){
        filter. <- filter. / sum(filter.)
        y.fit <- as.numeric(filter(y.fit, filter., sides=1))
    }
    
    ans <- list(x=tvar, y=y.fit, y.obs=yvar, xx=xx, bb=bb, init=init, alpha=alpha[1], cls=".DES")
    class(ans) <- c("DES", "ES", "list")
    ans
}

.ses.core <- function(yvar2, wgt, init=0){
    S1 <- yvar2
    S1[1] <- init	
    for (ii in 2:length(yvar2)){
        fe <- (yvar2[ii] - S1[ii-1]) ## fe: forecast error ??       
        ##S1[ii] <- S1[ii-1] + wgt[ii]*fe
        S1[ii] <- S1[ii-1] + ((fe > 0) * wgt[ii, 1] + (fe < 0) * wgt[ii, 2]) * fe ## what ??
    }
    S1
}

.seswgt <- function(alpha, tvar){
    dd   <- c(0, abs(diff(tvar)))
    ans  <- 1 - ((1 - alpha)^dd)
    ans
}

.getalpha <- function(yvar, tvar=seq_along(yvar), n.ahead=1, FUN.){

    ff <- function(alpha, yvar, tvar, n.ahead){
        es <- do.call(FUN., list(yvar, tvar, alpha=alpha))
        #print(es)
        .forecasterror(es, n.ahead)
    }
    aa <- optimize(ff, interval=c(0, 1), yvar=yvar, tvar=tvar, n.ahead=n.ahead)$minimum
    aa
}

.fit_es <- function(es, n.ahead=1){
    .getalpha(es$y.obs, es$tvar, n.ahead=n.ahead, FUN.=es$cls)
}

.vv <- Vectorize(.fit_es, vectorize.args="n.ahead")

#' @title Fit exponential smoothing object.
#'
#' @description Fit exponential smoothing object by minimizing the squared \code{n.ahead} steps prediction error.
#'
#' @name es_fit
#'
#' @param object Exponential smoothing object
#' @param n.ahead Number of steps ahead used in fitting.
#'
es_fit <- function(object, n.ahead=1){
    .vv(object, n.ahead)
}




##fit_alpha <- Vectorize(.getalpha, vectorize.args="n.ahead")

.forecasterror <- function(object, n.ahead=1){
    UseMethod(".forecasterror")
}

.forecasterror.DES <- function(object, n.ahead=1){
    yvar <- object$y.obs
    tvar <- object$x
    na.y  <- is.na(yvar)
    tvar2 <- tvar[!na.y]
    yvar2 <- yvar[!na.y]
    
    dtvar2 <- diff(tvar2)
    n2  <- length(dtvar2)
    xx2 <- object$xx[!na.y]
    bb2 <- object$bb[!na.y]

    f <- xx2[1:n2] + bb2[1:n2] * dtvar2
    ##fe <- yvar2 - c(NA, f)
    ## str(list(length(yvar2), length(f), n.ahead,
    ##          length(f[1:(1 + length(f) - n.ahead)])
    ##          ))
    fe <- yvar2 - c(rep(NA, n.ahead), f[1:(1 + length(f) - n.ahead)])

    sum(fe^2, na.rm=T) / sum(!is.na(fe))
}

.forecasterror.SES <- function(object, n.ahead=1){
    
    ##fe <- object$y.obs - c(NA, object$y[1:(length(object$x) - 1)])
    fe <- object$y.obs - c(rep(NA, n.ahead), object$y[1:(length(object$x) - n.ahead)])
    sum(fe^2, na.rm=T) / sum(!is.na(fe))      
}




update.ES <- function(object, alpha, ...){
    do.call(object$cls,
            list(yvar=object$y.obs, tvar=object$x,
                 init=object$init, alpha=alpha))
}




#' @title Forecast for exponential smoothing
#' @description Forecast for exponential smoothing.
#' @name forecast

#' @rdname forecast
#' @param object A 'SES' or 'DES' object.
#' @param at Timepoint at which prediction starts.
#' @param h Steps ahead for which predictions are made.
#' 
forecast <- function(object, at=NULL, h=NULL){
    UseMethod("forecast")
}

#' @rdname forecast
forecast.SES <- function(object, at=NULL, h=NULL){

    x <- object$x
    if (is.null(at)) at <- x[length(x)]
    if (is.null(h)) h <- c(0, 1)
        
    i <- max(which(x <= at))
    fr.t <- x[i]
    yp <- rep(object$y[i], length(h))
    list(x=at + h, y=yp)
}

#' @rdname forecast
forecast.DES <- function(object, at=NULL, h=NULL){

    x <- object$x
    if (is.null(at)) at <- x[length(x)]
    if (is.null(h)) h <- c(0, 1)
    
    i <- max(which(x <= at))
    fr.t <- x[i]
    
    yp <- object$xx[i] + object$bb[i] * h
    list(x=at + h, y=yp)
}







#' @rdname forecast
#' @param ... Graphics parameters passed on to lines()
forecast_lines <- function(object, at=NULL, h=NULL, ...){
    UseMethod("forecast_lines")
}

#' @rdname forecast
forecast_lines.ES <- function(object, at=NULL, h=NULL, ...){
    if (is.null(at)) at <- object$x
    sapply(at, function(a) lines(forecast(object, at=a, h=h), ...))
    invisible()
}



residuals.ES <- function(object,...){
    object$y.obs - object$y
}

fitted.ES <- function(object,...){
    object$y
}

plot.ES <- function(x, ...){
    plot.default(x$x, x$y.obs, ...)
    lines(x$x, x$y, col='red', ...)
}

print.ES <- function(x, ...){
    str(x[c("cls", "alpha")])
}





