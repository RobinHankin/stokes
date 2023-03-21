`ktensor` <- function(S){
  stopifnot(is.spray(S))
  stopifnot(all(index(S)>0))
  class(S) <- c("ktensor","spray")  # This is the only place class ktensor is set
  return(S)
}

`as.ktensor` <- function(M,coeffs){
    if(is.kform(M)){
        return(kform_to_ktensor(M))
    } else if(is.spray(M)){
        return(ktensor(M))
    } else {
        return(ktensor(spray(M,coeffs)))
    }
}

`is.ktensor` <- function(x){inherits(x,"ktensor")}
`is.kform` <- function(x){inherits(x,"kform")}

`as.function.ktensor` <- function(x, ...){
    stopifnot(is.ktensor(x))
    if(is.zero(x)){return(function(E){0})}
    v <- elements(coeffs(x))
    M <- index(x)
    k <- seq_len(ncol(M))
    p <- seq_len(nrow(M))
    function(E){
      sum(sapply(p,function(i){v[i]*prod(E[cbind(M[i,],k)])}))
    }
}

`kill_trivial_rows` <- function(S){
    I <- index(S)
    if(length(I)>0){  # cannot use nrow(I)==0, as 'I' might be NULL
      wanted <- apply(I,1,function(x){all(table(x)==1)})
      out <- spray(I[wanted,,drop=FALSE],elements(coeffs(S))[wanted])
    } else {
      out <- S
    }
    return(out)
}

`consolidate` <- function(S){  # S is a spray object corresponding to an alternating form
    I <- index(S)
    if(length(I)==0){return(S)}
    newind <- 0*I
    change_sign <- numeric(nrow(I))
    for(i in seq_len(nrow(I))){
        o <- I[i,]
        newind[i,] <- sort(o)
        jj <- word(rbind(order(o)))
        if(is.id(jj)){
            change_sign[i] <- 1
        } else {
            change_sign[i] <- sgn(jj) # which might be 1
        }
    }  # i loop closes

    out <- spray(newind, elements(coeffs(S))*change_sign, addrepeats=TRUE)  # does a lot of work (potentially)
    if(length(index(out))==0){ # zero form
      out <- spray(matrix(0,0,ncol(I)))
    }

    return(out)     
}

`include_perms` <- function(S){ # S is a spray object
    S <- kill_trivial_rows(S) # not strictly necessary (trivial rows get killed below); here for efficiency
    k <- ncol(index(S))
    p <- allperms(k)

    outmat <- matrix(0L,nrow(p)*nterms(S),k)
    outvec <- seq_len(nrow(outmat))
    for(i in seq_len(nrow(index(S)))){
      jj <- seq(from=1+(i-1)*factorial(k), to=i*factorial(k))
      outmat[jj,] <- matrix(index(S)[i,,drop=TRUE][p],nrow(p),k,byrow=FALSE)
      outvec[jj ] <- sgn(p)*elements(coeffs(S))[i]
    }

    return(spray(outmat,outvec,addrepeats=TRUE))  # should be  alternating
}
    
`Alt` <- function(S,give_kform=FALSE){ # Returns Alt(S), an alternating multilinear
                      # function (mathematically equivalent to a form,
                      # but including redundancy)

  ## works in three steps.  Firstly, lose the repeats (that is, rows
  ## with a repeated index, as in [1,3,4,1,2] ("1" appears twice).
  ## Then, sort the rows.  Then, sum over all orderings:

  if(is.kform(S)){return(S)}
  if(give_kform){return(kform(S)/factorial(arity(S)))}
  out <- kill_trivial_rows(S)
    if(nrow(index(out))==0){  # the zero form
        return(S*0)
    }

    ktensor(include_perms(consolidate(out))/factorial(ncol(index(out))))
}

`tensorprod` <- function(U, ...) {
   if(nargs()<3){
     tensorprod2(U, ...)
   } else {
     tensorprod2(U, Recall(...))
   }
}

`tensorprod2` <- function(U1,U2){  # returns U1\otimes U2
    if(is.empty(U1) | is.empty(U2)){
      return(as.ktensor(cbind(index(U1)[0,],index(U2)[0,])))
    }
    return(ktensor(spraycross(U1,U2)))
}

`%X%` <- function(x,y){tensorprod2(x,y)}

`wedge` <- function(x, ...) {
   if(nargs()<3){
     wedge2(x,...)
   } else {
     wedge2(x, Recall(...))
   }
}

`wedge2` <- function(K1,K2){
  if(missing(K2)){return(K1)}
  if(is.ktensor(K1) | is.ktensor(K2)){stop("wedge product only defined for kforms")}
  if(`|`(!is.kform(K1),!is.kform(K2))){return(K1*K2)}

  if(is.empty(K1) | is.empty(K2)){
    return(zeroform(arity(K1)+arity(K2)))
    }

  kform(spraycross(K1,K2)) # the meat
}

`%^%` <- function(x,y){wedge(x,y)}

`kform_basis` <- function(n,k){  # just a matrix (a low-level helper function)
    if(k==1){return(as.matrix(seq_len(n)))}
    f <- function(x){which(x>0)}
    t(apply(blockparts(rep(1,n),k),2,f))
}

`kform` <- function(S){
    stopifnot(is.spray(S))
    stopifnot(all(index(S)>0))
    S <- consolidate(kill_trivial_rows(S))
    class(S) <- c("kform", "spray")   # This is the only class setter for kform objects
    return(S)
}

`as.kform` <- function(M,coeffs,lose=TRUE){
    if(is.spray(M)){return(kform(M))}
    if(length(c(M))==0){M <- matrix(1,1,0)} # kludge
    out <- kform(spray(M,coeffs))
    if(lose){out <- lose(out)}
    return(out)
}

`d` <- function(i){as.kform(i)}

`e` <- function(i,n=i){
    out <- numeric(n)
    out[i] <- 1
    return(out)
}

`kform_general`  <- function(W,k,coeffs,lose=TRUE){
    if(length(W)==1){W <- seq_len(W)}
    M <-  kform_basis(length(W),k)
    M[] <- W[M]
    as.kform(M,coeffs,lose=lose)
}

`as.function.kform` <- function(x, ...){
    M <- index(x)
    coeffs <- elements(coeffs(x))
    return(function(E){
      out <- 0
      E <- cbind(E)
      for(i in seq_len(nrow(M))){
        out <- out + coeffs[i]*det(E[M[i,,drop=TRUE],,drop=FALSE])
      }
      return(out)
    })
}

`rform` <- function(terms=9, k=3, n=7, coeffs, ensure=TRUE){
  if(missing(coeffs)){coeffs <- seq_len(terms)}
  ind <- t(replicate(terms,sample(seq_len(n),k)))
  if(ensure & all(ind)<n){ind[sample(which(ind==max(ind)),1)] <- n}
  kform(spray(ind,coeffs,addrepeats=TRUE))
}

`rtensor` <- function(terms=9,k=3, n=7, coeffs){
    if(missing(coeffs)){coeffs <- seq_len(terms)}
    M <- matrix(sample(seq_len(n),terms*k,replace=TRUE),terms,k)
    ktensor(spray(M,seq_len(terms),addrepeats=TRUE))
}
    
`as.1form` <- function(v){
    kform(spray(cbind(seq_along(v)),v))
}
`grad` <- as.1form

`.putw` <- function(v,symbols,prodsymb,d){  # eg v=(1,3,4,7)
  out <- paste(paste(d,symbols[v],sep=""),prodsymb,sep="",collapse="")
  return(substr(out, 1, nchar(out)-1) )
}

`as.symbolic` <- function(M,symbols=letters,d=""){
  if(is.kform(M)){
    prodsymb <- "^"
  } else if(is.ktensor(M)){
    prodsymb <- "*"
  } else {
    stop("only takes ktensor or kform objects")
  }
  
  IM <- index(M)
  v <- elements(coeffs(M))

  out <- ""
  for(i in seq_len(nrow(IM))){
    if(v[i] == 1){
      jj <- "+"
    } else if(v[i] == -1){
      jj <- "-"
    } else if(v[i] > 0){
      jj <- paste("+",v[i],sep="")
    } else if(v[i] < 0){
      jj <- v[i]
    } else {
      stop("this cannot happen")
    }
    out <- paste(out, paste(jj,.putw(IM[i,],symbols,prodsymb=prodsymb,d=d),sep=" "))
  }
  return(noquote(out))
}

`hodge` <- function(K, n=dovs(K), g, lose=TRUE){
    if(missing(g)){g <- rep(1,n)}

    if(is.empty(K)){
        if(missing(n)){
            stop("'K' is zero but no value of 'n' is supplied")
        } else {
            return(kform(spray(matrix(1,0,n-arity(K)),1)))
        }
    } else if(is.volume(K,n)){
        return(scalar(coeffs(K),lose=lose))
    } else if(is.scalar(K)){
        if(missing(n)){
            stop("'K' is scalar but no value of 'n' is supplied")
        } else {
            return(volume(n)*coeffs(K))
        }
    } # weird edge-cases end
    
    
    stopifnot(n >= dovs(K))
    
    f1 <- function(o){seq_len(n)[!seq_len(n) %in% o]}
    f2 <- function(x){permutations::sgn(permutations::as.word(x))}
    f3 <- function(v){prod(g[v])}
    
    iK <- index(K)
    jj <- apply(iK,1,f1)
    if(is.matrix(jj)){
        newindex <- t(jj)
    } else {
        newindex <- as.matrix(jj)
    }
    
    x_coeffs <- elements(coeffs(K))
    x_metric <- apply(iK,1,f3)
    x_sign <- apply(cbind(iK,newindex),1,f2)  # get the sign 
    
    as.kform(newindex,x_metric*x_coeffs*x_sign)
}

`inner` <- function(M){
    ktensor(spray(expand.grid(seq_len(nrow(M)),seq_len(ncol(M))),c(M)))
}

`pullback` <- function(K,M)
{
    if(is.zero(K)){return(K)}
    Reduce(`+`,sapply(seq_along(coeffs(K)),
                      function(i){
                          do.call("wedge",
                                  apply(
                                      M[index(K)[i,,drop=FALSE],,drop=FALSE],1,
                                      as.1form))*elements(coeffs(K))[i]},simplify=FALSE))
}

`issmall` <- function(M,tol=1e-8){  # tests for a kform being either zero or "small"
    if(is.zero(M)){
        return(TRUE)
    } else {
        error <- coeffs(M)
        if(all(abs(error)<tol)){
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
}  # issmall() closes


`stretch` <- function(K,d){
  M <- index(K)
  d <- d[seq_len(max(M))]
  M[] <- d[M]  # the meat
  as.kform(index(K),elements(coeffs(K))*apply(M,1,prod))
}
  
`keep` <- function(K,yes){
    jj <- rep(0L,dovs(K))
    jj[yes] <- 1
    stretch(K,jj)
}

`discard` <- function(K,no){
    jj <- rep(1L,dovs(K))
    jj[no] <- 0L
    stretch(K,jj)
}

`zerotensor` <- function(n){as.ktensor(rep(1,n))*0}
`zeroform` <- function(n){0*as.kform(seq_len(n),lose=FALSE)}

`contract_elementary` <- function(o,v){
  out <- zeroform(length(o)-1)
  for(i in seq_along(o)){
    out <- out + (-1)^(i+1)*v[o[i]]*as.kform(rbind(o[-i]),lose=FALSE)
  }
  return(out)
}

`contract` <- function(K,v,lose=TRUE){
    if(is.vector(v)){
        out <- Reduce("+",Map("*", apply(index(K),1,contract_elementary,v),elements(coeffs(K))))
    } else {
        stopifnot(is.matrix(v))
        out <- K
        for(i in seq_len(ncol(v))){
            out <- contract(out,v[,i,drop=TRUE],lose=FALSE)
        }
    }
    if(lose){out <- lose(out)}
    return(disordR::drop(out))
}

`scalar` <- function(s,kform=TRUE,lose=FALSE){
    if(lose){
        stopifnot(length(s)==1)
        return(s)
    } else {
        if(kform){
            return(s*kform(spray(matrix(1,1,0))))
        } else {
            return(s*ktensor(spray(matrix(1,1,0))))
        }
    }
}
    
`0form` <- function(s=1,lose=FALSE){scalar(s,kform=TRUE,lose=lose)}
`0tensor` <- function(s=1,lose=FALSE){scalar(s,kform=FALSE,lose=lose)}

`is.scalar` <- function(M){
  return(
      is.zero(M)                                   || 
      ((length(M)==1) & is.numeric(M))             || 
      (is.kform(M)   & all(dim(index(M))==c(1,0))) || 
      (is.ktensor(M) & all(dim(index(M))==c(1,0)))
  )
}

`volume` <- function(n){as.kform(seq_len(n))}

`is.volume` <- function(K,n=dovs(K)){
  return(
  (nterms(K) == 1)      &&
  (nrow(index(K)) == 1) &&
  (arity(K) > 0)        &&
  n == dovs(K)    && 
  all(seq_len(arity(K)) == index(K))
  )
}

setGeneric("lose",function(x){standardGeneric("lose")})

`lose` <- function(M){UseMethod("lose",M)}
`lose.kform` <- function(M){
    if(arity(M)==0){
        return(coeffs(M))
    } else {
        return(M)
    }
}

`lose.ktensor` <- lose.kform 

`zap` <- function(X){UseMethod("zap",X)}
`zap.kform` <- function(X){kform(spray::zap(X))}
`zap.ktensor` <- function(X){ktensor(spray::zap(X))}

`kform_to_ktensor` <- function(S){
    stopifnot(is.kform(S))
    if(is.zero(S)){
        return(as.ktensor(index(S),0))
    } else {
        return(as.ktensor(include_perms(as.spray(unclass(S)))))
    }
}

`coeffs<-.kform` <- function(S,value){
    jj <- coeffs(S)
    if(is.disord(value)){
        stopifnot(consistent(coeffs(S),value))
        jj <- value
    } else {
        jj[] <- value  # the meat
    }
    return(kform(spray(index(S),elements(jj))))
}

`coeffs<-.ktensor` <- function(S,value){
    jj <- coeffs(S)
    if(is.disord(value)){
        stopifnot(consistent(coeffs(S),value))
        jj <- value
    } else {
        jj[] <- value  # the meat
    }
    return(ktensor(spray(index(S),elements(jj))))
}

`vector_cross_product` <- function(M){
    n <- nrow(M)
    stopifnot(n==ncol(M)+1)
    (-1)^n*sapply(seq_len(n),function(i){(-1)^i*det(M[-i,])})
}

`kinner` <- function(o1,o2,M){
    stopifnot(arity(o1) == arity(o2))
    k <- arity(o1)
    if(missing(M)){M <- diag(nrow=max(c(index(o1),index(o2))))}

    out <- 0
    
    k <- arity(o1)
    c1 <- elements(coeffs(o1))
    c2 <- elements(coeffs(o2))
    for(no1 in seq_len(nterms(o1))){
        for(no2 in seq_len(nterms(o2))){
            MM <- matrix(0,k,k)
            for(i in seq_len(k)){
                for(j in seq_len(k)){
                    MM[i,j] <- MM[i,j] + M[index(o1)[no1,i], index(o2)[no2,j]]
                }
            }
            out <- out + det(MM)*c1[no1]*c2[no2]
        } # o2 terms loop closes
    } # o1 terms loop closes
    return(out)
}

`dovs` <- function(K){
    if(is.zero(K) || is.scalar(K)){
        return(0)
    } else {
        return(max(index(K)))
    }
}


`summary.kform` <- function(object, ...){
  class(object) <- "spray"
  out <- spray::summary(object)
  out[[3]] <- nterms(object)
  class(out) <- "summary.kform"
  return(out)
}

`summary.ktensor` <- function(object, ...){
  class(object) <- "spray"
  out <- spray::summary(object)
  out[[3]] <- nterms(object)
  class(out) <- "summary.ktensor"
  return(out)
}

`print.summary.kform` <- function(x,...){
  cat(paste("A kform object with ", x[[3]], " terms.  Summary of coefficients: \n\n",sep=""))
  print(x[[1]])
  cat("\n\nRepresentative selection of index and coefficients:\n\n")
  print(kform(x[[2]]))
}

`print.summary.ktensor` <- function(x,...){
  cat(paste("A ktensor object with ", x[[3]], " terms.  Summary of coefficients: \n\n",sep=""))
  print(x[[1]])
  cat("\n\nRepresentative selection of index and coefficients:\n\n")
  print(ktensor(x[[2]]))
}

setGeneric("sort")
