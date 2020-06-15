`print.ktensor` <- function(x,...){
    if(is.zero(x)){
        cat(paste("The zero linear map with arity ",arity(x),";\n",sep=""))
    } else {
        cat(paste("A linear mapping from V^",arity(x)," to R with V=R^",max(index(x)),":\n",sep=""))
    }
    class(x) <- "spray"
    print(x,...)
    return(invisible(x))
}

`print.kform` <- function(x,...){
    if(is.zero(x)){
        cat(paste("The zero alternating linear map with arity ",arity(x),";\n",sep=""))
    } else {
        cat(paste("A linear map from V^",arity(x)," to R with V=R^",max(index(x)),":\n",sep=""))
    }
    class(x) <- "spray"
    print(x,...)
    return(invisible(x))
}
