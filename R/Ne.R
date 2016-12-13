
#' Evaluate effective population size (Ne)
#'
#' \code{Ne} evaluates effective population size given some demographic parameters.
#'
#' @param nM numeric, number of breeding males.
#' @param nF numeric, number of breeding females.
#' @param AvgProgM numeric, average number of progeny (family size) per male
#'   (number of progeny that will contribute to the next generation!!!).
#' @param AvgProgF numeric, average number of progeny (family size) per female
#'   (number of progeny that will contribute to the next generation!!!).
#' @param VarProgM numeric, average number of progeny (family size) per male
#'   (number of progeny that will contribute to the next generation!!!).
#' @param VarProgF numeric, average number of progeny (family size) per female
#'   (number of progeny that will contribute to the next generation!!!).
#' @param c numeric, fraction of males among breeding individuals.
#' @param Fis numeric, inbreeding coefficient (within (sub)-population) due to
#'   the excess of matings between relatives over random mating expectation.
#'
#' @return Evaluated effective population size.
#'
#' @examples
#' Ne(nM=100, nF=100)
#' Ne(nM=10, nF=100)
#' Ne(nM=10, nF=100, VarProgM=0)
#' @export
Ne <- function(nM, nF, AvgProgM=(nM+nF)/nM, AvgProgF=(nM+nF)/nF, VarProgM=AvgProgM, VarProgF=AvgProgF, c=nM/(nM+nF), Fis=0) {
  # Derive
  # * dV_m = deviance of variance in the number of offspring in comparison to a Poisson process in   males
  # * dV_f = deviance of variance in the number of offspring in comparison to a Poisson process in females
  dVarM <- VarProgM - AvgProgM
  dVarF <- VarProgF - AvgProgF
  # Evaluate
  1 / ((1/nM + 1/nF + (dVarM*c^2)/nM + (dVarF*(1-c)^2)/nF) * (1+Fis)/4)
}

#' Find number of breeding males and frmales for targeted effective population size.
#'
#' \code{FindNe} searches which number of breeding males and frmales give targeted
#'   effective population size.
#'
#' @param NeTarget numeric, targeted effective population size.
#' @param nM numeric, vector of number of breeding males to evaluate over.
#' @param nF numeric, vector of number of breeding males to evaluate over.
#' @param ... passed to \code{\link{Ne}}.
#'
#' @return Data frame with evaluated combinations ranked by distance from the target.
#'
#' @examples
#' FindNe(NeTarget=100, nM=c(50, 100, 150), nF=c(50, 100, 150))
#' FindNe(NeTarget=100, nM=c(10, 20, 30, 50, 100), nF=c(50, 100, 150), VarProgM=0)
#' @export
FindNe <- function(NeTarget, nM, nF, ...) {
  nComb <- length(nM*nF)
  Ret <- data.frame(nM=rep(NA, times=nComb), nF=rep(NA, times=nComb), Ne=NA, Diff=NA)
  i <- 0
  for (nM in nM) {
    for (nF in nF) {
      i <- i + 1
      Ret[i, "nM"] <- nM
      Ret[i, "nF"] <- nF
      Ret[i, "Ne"] <- Ne(nM=nM, nF=nF, ...)
      Ret[i, "Diff"] <- Ret[i, "Ne"] - NeTarget
      Ret[i, "DiffSq"] <- Ret[i, "Diff"]^2
    }
  }
  Ret[order(Ret$DiffSq), ]
}

