
#' Selection intensity
#'
#' \code{SelInt} evaluates selection intensity given proportion of selected individuals.
#'
#' @param x numeric, proportion of selected individuals.
#'
#' @return Selection intensity.
#'
#' @examples
#' SelInt(x=1.00)
#' SelInt(x=0.99)
#' SelInt(x=0.50)
#' SelInt(x=0.05)
#' SelInt(x=0.01)
#' @export
#' @importFrom stats qnorm dnorm
SelInt <- function(x) {
  if(any(x < 0) | any(x > 1)) stop("'x' must be in range [0, 1]")
  dnorm(qnorm(x, lower.tail=FALSE)) / x
  ## qnorm() maps [0, 1] to N(0, 1)
  ## dnorm() specifys the height i.e. z
}
