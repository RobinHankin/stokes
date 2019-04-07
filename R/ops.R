"Ops.kform" <-
    function (e1, e2 = NULL) 
{
    unary <- nargs() == 1
    lclass <- nchar(.Method[1]) > 0
    rclass <- !unary && (nchar(.Method[2]) > 0)
    
    if(unary){
        if (.Generic == "+") {
            return(as.kform(e1))
        } else if (.Generic == "-") {
            return(as.kform(spray(index(e1),-value(e1))))
        } else {
            stop("Unary operator '", .Generic, "' is not implemented for kforms")
        }
    }
    if (!is.element(.Generic, c("+", "-", "*", "/", "==", "!="))){
        stop("operator '", .Generic, "' is not implemented for kforms")
    }

    f <- function(o){spray(index(o),value(o))}
    if (lclass && rclass) {
        if (.Generic == "+"){
            return(as.kform(f(e1) + f(e2)))
        } else if (.Generic == "-") {
            return(as.kform(f(e1) - f(e2)))
        } else if (.Generic == "=="){
            return(f(e1) == f(e2))
        } else if(.Generic == "!="){
            return(f(e1) != f(e2))
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for kforms")
        }
    } else if(lclass & !rclass){
        if(.Generic == "*"){
            return(as.kform(f(e1)*e2))
        } else if(.Generic == "/"){
            return(as.kform(f(e1)/e2))
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for kform <op> other ")
        }
    } else if(!lclass & rclass){
        if(.Generic == "*"){
            return(as.kform(f(e2)*e1))
        } else if(.Generic == "/"){
            stop("kforms are not a field")
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for other <op> kform")
        }
    } else if ((!lclass) & (!rclass)){
        stop("odd---neither argument has class kform?")
    }  else {
        stop("this cannot happen")
    }
}
    
"Ops.ktensor" <- function (e1, e2 = NULL){
    unary <- nargs() == 1
    lclass <- nchar(.Method[1]) > 0
    rclass <- !unary && (nchar(.Method[2]) > 0)
    
    if(unary){
        if (.Generic == "+") {
            return(e1)
        } else if (.Generic == "-") {
            return(as.ktensor(spray(index(e1),-value(e1))))
        } else {
            stop("Unary operator '", .Generic, "' is not implemented for ktensors")
        }
    }
    if (!is.element(.Generic, c("+", "-", "*", "/", "==", "!="))){
        stop("operator '", .Generic, "' is not implemented for ktensors")
    }

    f <- function(o){spray(index(o),value(o))}
    if (lclass && rclass) {
        if (.Generic == "+"){
            return(as.ktensor(f(e1) + f(e2)))
        } else if (.Generic == "-") {
            return(as.ktensor(f(e1) - f(e2)))
        } else if (.Generic == "=="){
            return(f(e1) == f(e2))
        } else if(.Generic == "!="){
            return(f(e1) != f(e2))
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for ktensors")
        }
    } else if(lclass & !rclass){
        if(.Generic == "*"){
            return(as.ktensor(f(e1)*e2))
        } else if(.Generic == "/"){
            return(as.ktensor(f(e1)/e2))
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for ktensor <op> other ")
        }
    } else if(!lclass & rclass){
        if(.Generic == "*"){
            return(as.ktensor(f(e2)*e1))
        } else if(.Generic == "/"){
            stop("ktensors not a field")
        } else {
            stop("Binary operator '", .Generic, "' is not implemented for other <op> ktensor")
        }
    } else if ((!lclass) & (!rclass)){
        stop("odd---neither argument has class ktensor?")
    }  else {
        stop("this cannot happen")
    }
}
    
