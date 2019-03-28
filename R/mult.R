`as.ktensor` <- function(S){
  stopifnot(is.spray(S))
  stopifnot(all(index(S)>0))
  class(S) <- c("ktensor","spray")
  return(S)
}

`as.function.ktensor` <- function(x, ...){
    stopifnot(is.spray(x))
    v <- value(x)
    M <- index(x)
    k <- seq_len(ncol(M))
    p <- seq_len(nrow(M))
    function(E){
      sum(sapply(p,function(i){v[i]*prod(E[cbind(M[i,],k)])}))
    }
}

`lose_repeats` <- function(S){
    wanted <- apply(index(S),1,function(x){all(table(x)==1)})
    spray(index(S)[wanted,,drop=FALSE],value(S)[wanted])
}

`consolidate` <- function(S){  # S is a spray object corresponding to an alternating form
    newind <- 0*index(S)
    change_sign <- numeric(nrow(index(S)))
    for(i in seq_len(nrow(index(S)))){
        o <- index(S)[i,]
        newind[i,] <- sort(o)
        change_sign[i] <- sgn(word(rbind(order(o))))
    }
    spray(newind, value(S)*change_sign, addrepeats=TRUE)  # does a lot of work (potentially)
}

`include_perms` <- function(S){ # S is a spray object
    k <- ncol(index(S))
    p <- allperms(k)

    f <- function(v,x){ # v is a vector, one row of an index matrix; x is its coeff
        spray(matrix(v[p],nrow(p),k),sgn(p)*x)
    }

    out <- f(index(S)[1,,drop=TRUE],value(S)[1]) # do the first row first, then...
    for(i in seq_len(nrow(index(S)))[-1]){  # ...do the other rows
        out <- out + f(index(S)[i,,drop=TRUE],value(S)[i])
    }
    return(out)  # should be in alternating form
}
    
`Alt` <- function(S){ # Returns Alt(S), an alternating multilinear
                      # function (mathematically equivalent to a form,
                      # but including redundancy)

  ## works in three steps.  Firstly, lose the repeats (that is, rows
  ## with a repeated index, as in [1,3,4,1,2] ("1" appears twice).
  ## Then, sort the rows.  Then, sum over all orderings:

    S %<>% lose_repeats
    if(nrow(index(S))==0){  # the zero form
        return(S)
    }
    
    return(S %>% consolidate %>% include_perms %>% `/`(factorial(ncol(index(S)))))
}

`cross` <- function(S1,S2){  # returns S1\otimes S2
    M1 <- index(S1)
    M2 <- index(S2)
    jj <- as.matrix(expand.grid(seq_len(nrow(M1)),seq_len(nrow(M2))))
    f <- function(i){c(M1[jj[i,1],],M2[jj[i,2],])}
    out <- spray(t(sapply(seq_len(nrow(jj)),f)),c(outer(value(S1),value(S2))))
    return(out)
}

`wedge` <- function(x, ...) {
   if(nargs()<3){
     wedge2(x,...)
   } else {
     wedge2(x, Recall(...))
   }
}

`wedge2` <- function(F1,F2){

  stopifnot(inherits(F1,'kform'))
  stopifnot(inherits(F2,'kform'))
  
  

  ## we need to go through F1 and F2 line by line (wedge product is
  ## left- and right- distributive).
  
  ind1 <- index(F1)
  var1 <- value(F1)
  ind2 <- index(F2)
  var2 <- value(F2)
  
  n1 <- length(var1)
  n2 <- length(var2)

  f <- function(i){c(ind1[i[1],,drop=TRUE],ind2[i[2],,drop=TRUE])}
  M <- expand.grid(seq_len(n1),seq_len(n2)) %>% apply(1,f) %>% t
  kform(M, c(outer(var1,var2)))
}

`kform_basis` <- function(n,k,coeffs){  # just a matrix (a low-level helper function)
    f <- function(x){which(x>0)}
    t(apply(blockparts(rep(1,n),k),2,f))
}

`kform` <- function(M,coeffs){
  stopifnot(all(M>0))
  out <- spray(M,coeffs,addrepeats=TRUE) %>% lose_repeats %>% consolidate
  class(out) <- c("kform", "spray")   # This is the only class setter for kform objects
  return(out)
}

`kform_general`  <- function(W,k,coeffs){
    M <-  kform_basis(length(W),k)
    M[] <- W[M]
    kform(M,coeffs)
}

`as.function.kform` <- function(x,...){
    M <- index(x)
    coeffs <- value(x)
    return(function(E){
      out <- 0
      for(i in seq_len(nrow(M))){
        out <- out + coeffs[i]*det(E[M[i,,drop=TRUE],])
      }
      return(out)
    })
}
