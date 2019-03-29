"Ops.kform" <-
  function (e1, e2 = NULL) 
{
  unary <- nargs() == 1
  lclass <- nchar(.Method[1]) > 0
  rclass <- !unary && (nchar(.Method[2]) > 0)
  
  if(unary){
    if (.Generic == "+") {
      return(as.ktensor(as.spray(e1)))
    } else if (.Generic == "-") {
      return(as.ktensor(as.spray(-e1)))
    } else {
      stop("Unary operator '", .Generic, "' is not implemented for kforms")
    }
  }
  if (!is.element(.Generic, c("+", "-", "*", "/", "==", "!=")))
    stop("operator '", .Generic, "' is not implemented for kforms")
  
  if (lclass && rclass) {
    if (.Generic == "+"){
      return(askform(as.spray(e1)+as.spray(e2)))
    } else if (.Generic == "-") {
      return(kform(e1-e2))
    } else if (.Generic == "=="){
      return(e1==e2)
    } else if(.Generic == "!="){
      return(e1!=e2)
    } else {
      stop("Binary operator '", .Generic, "' is not implemented for kforms")
    }
  } else if(xor(lclass,rclass)){
    if(.Generic == "*"){
      return(as.ktensor(e1*e2))
    } else if(.Generic == "/"){
      return(as.ktensor(e1/e2))
    } else {
      stop("Binary operator '", .Generic, "' is not meaningful in this context")
    }
  } else if ((!lclass) & (!rclass)){
    stop("odd---neither argument has class kform?")
  }  else {
    stop("this cannot happen")
  }
}
 
