## function that allows multiple outputs from user defined functions. 
# Call it by using the := operator and wrapping multiple outputs in c().
# For example:
#
# functionReturningTwoValues <- function() { return(list(1, matrix(0, 2, 2))) }
# c(a, b) := functionReturningTwoValues()
#
# Code from here: http://code.google.com/p/miscell/source/browse/rvalues/rvalues.r


':=' = function(lhs, rhs) {
  frame = parent.frame()
  lhs = as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs = lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs = list(rhs)
  if (length(lhs) > length(rhs))
    rhs = c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) }
