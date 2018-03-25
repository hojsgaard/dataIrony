#' Cow body weight
#' 
#' Body weight of cows througout a lactation for 48 cows.
#'
#' @name data-cowbw
#' @docType data
#'
#' @aliases cowbw cowbw2
#' 
#' @format 
#' \describe{
#' \item{cowid:}{Identifier of cow}
#' \item{tfc:}{Time from calving in days}
#' \item{bw:}{Body weight}
#' }
#'
## #' @references Venables, W.N; Ripley, B.D.(1999) Modern Applied Statistics with
## #' S-Plus, Heidelberg, Springer, 3rd edition, chapter 7.2
## #'
#'
#' @details There are 48 cowids in \code{cowbw}; \code{cowbw2}
#'     contains data for the first six cowids.
#' 
#' @keywords datasets
#' @examples
#'
#' require(ggplot2)
#' data(cowbw2)
#' 
#' qplot(tfc, bw, data=cowbw2, colour=cowid) + geom_path()
#'
#' cowlst <- cowbw2 %>% split(.$cowid)
#' 
#' i <- 2
#' dd <- cowlst[[i]]
#' tvar <- dd$tfc
#' yvar <- dd$bw
#'
#' f1 <- ses(yvar, tvar)
#' f2 <- des(yvar, tvar)
#'
#' plot(f1)
#' at <- seq(10, 310, by=20)
#' forecast_lines(f1, at=at, ahead=0:20, col='red', lwd=3)
#' forecast_lines(f2, at=at, ahead=0:20, col='blue', lwd=3)
"cowbw"
"cowbw2"
