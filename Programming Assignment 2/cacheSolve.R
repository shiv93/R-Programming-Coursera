source("makeCacheMatrix.R")
cacheSolve<-function(object){
  value<-object$getInverse()
  if(!(is.null(value))){
    message ("Displaying cached value...")
    return(value)
    
  }
  else {
    matrix1<-object$get()
    if(nrow(matrix1)==ncol(matrix1)){
      inv<-solve(matrix1)
      object$setInverse(inv)
      return (inv)
    }
    
    return(message("Inverse not possible."))
    
  }
  
  
  
  
  
}