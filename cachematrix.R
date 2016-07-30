## Cousera Assigment



## create a matrix tov for inverse


makeCacheMatrix <- function(x = matrix()) {
  
  matrixInver = NULL
  setVal <- function(y){
    x <<- y
    matrixInver <<- NULL
  }
  
  
  getVal <- function() x
  
  setInver <- function(inv) matrixInver <<- inv
  getInver <- function() matrixInver
  
  list(setVal = setVal, getVa = getVAL, setInver=setInver, getInver = getInver)
}




## Function to work on special matrix

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
    
    matrixInv <- x$getInver()
    ## Already calculate ? 
    if(!is.null(matrixInv)){
      message("Already calculated")
      return (matrixInv)
      
    }
    
    ## else, set dat
    mat.data = x$getVal()
    inv = solve(mat.data, ...)
    
    x$setInver(matrixInv)
    
    return(matrixInv)
      

}
 