print.hts <- function(x,...)
{
    cat("Hierarchical Time Series")
    cat("\n  ",length(x$m),"Levels")
    cat("\n   Number of series at each level:",x$m)
    cat("\n   Total number of series:",sum(x$m))
    cat("\n   Number of observations per series:",nrow(x$y),"\n")
    cat("\n   Top level series:\n")
    print(x$gma)
}
