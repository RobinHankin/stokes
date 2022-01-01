`print.ktensor` <- function(x,...){
    m <- max(0,index(x))
    if(is.zero(x)){
        cat(paste("The zero linear map from V^",arity(x)," to R with V=R^n:\n",sep=""))
    } else {
        cat(paste("A linear map from V^",arity(x)," to R with V=R^",m,":\n",sep=""))
    }
    if(isTRUE(getOption("stokes_symbolic_print"))){
        cat(as.symbolic(x,d="d",symbols=as.character(seq_len(m))),"\n")
    } else {
        class(x) <- "spray"
        print(x,...)
    }
    return(invisible(x))
}

`print.kform` <- function(x,...){
    m <- max(0,index(x))
    if(is.zero(x)){
        cat(paste("The zero alternating linear map from V^",arity(x)," to R with V=R^n:\n",sep=""))
    } else {
        cat(paste("An alternating linear map from V^",arity(x)," to R with V=R^",m,":\n",sep=""))
    }
    if(isTRUE(getOption("stokes_symbolic_print"))){
        cat(as.symbolic(x,d="d",symbols=as.character(seq_len(m))),"\n")
    } else {
        class(x) <- "spray"
        print(x,...)
    }
    return(invisible(x))
}
