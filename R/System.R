
#' Run system command with more control/reporting
#'
#' \code{System} runs system command via \code{system} and gives you more reporting.
#'
#' @param Command \code{command} in \code{\link{system}}.
#' @param Wait \code{wait} in \code{\link{system}}.
#' @param CheckValue numeric, check the command return value against this value.
#' @param NoteStart character, add some notes prior to starting the command.
#' @param NoteEnd character, add some notes after the command has been run.
#' @param NoteError character, add some notes only when the error happens.
#' @param NoteDetail character, add some detailed notes to all the notes.
#' @param ... arguments passed to \code{\link{system}}
#'
#' @return As in \code{\link{system}}.
#'
#' @export
#'
#' @examples
#' System(Command="ls -l")
#'
#' @seealso \code{\link{system}}
System <- function(Command, Wait=TRUE, CheckValue=0, NoteStart=NULL, NoteEnd=NULL, NoteError=NULL, NoteDetail=NULL,...) {
  if (!is.null(NoteStart)) {
    cat(paste0("START: ", Command,
               ifelse(!is.null(NoteDetail), paste0(" ", NoteDetail), ""), "\n"))
  }
  Check <- system(command=Command, wait=Wait, ...)
  if (Check > CheckValue) {
    stop(paste0("ERROR: ", Command,
                paste0(" return value: ", CheckValue),
                ifelse(!is.null(NoteDetail), paste0(" ", NoteDetail), ""),
                ifelse(!is.null(NoteError),  paste0(" ", NoteError), ""), "\n"))
  }
  if (!is.null(NoteEnd)) {
    cat(paste0("END: ", Command,
               ifelse(!is.null(NoteDetail), paste0(" ", NoteDetail), ""), "\n"))
  }
  invisible(Check)
}
