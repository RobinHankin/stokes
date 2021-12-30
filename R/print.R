`print.ktensor` <- function(x,...){
    if(is.zero(x)){
        cat(paste("The zero linear map from V^",arity(x)," to R with V=R^n:\n",sep=""))
    } else {
        cat(paste("A linear map from V^",arity(x)," to R with V=R^",max(index(x)),":\n",sep=""))
    }
    if(isTRUE(getOption("stokes_symbolic_print"))){
        cat(as.symbolic(x,d="d",symbols=as.character(seq_len(max(index(x))))),"\n")
    } else {
        class(x) <- "spray"
        print(x,...)
    }
    return(invisible(x))
}

`print.kform` <- function(x,...){
    if(is.zero(x)){
        cat(paste("The zero alternating linear map from V^",arity(x)," to R with V=R^n:\n",sep=""))
    } else {
        cat(paste("An alternating linear map from V^",arity(x)," to R with V=R^",max(index(x)),":\n",sep=""))
    }
    if(isTRUE(getOption("stokes_symbolic_print"))){
        cat(as.symbolic(x,d="d",symbols=as.character(seq_len(max(index(x))))),"\n")
    } else {
        class(x) <- "spray"
        print(x,...)
    }
    return(invisible(x))
}
