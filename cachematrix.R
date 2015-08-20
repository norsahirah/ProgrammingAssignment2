## makeCacheMatrix creates a list containing a matrix and the functions

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL   ## The inverse starts out as NULL
    setmatrix <- function(y) {
      x <<- y  ## reset the matrix which is used to calculate the inverse
      inv <<- NULL
      
    }
    
    getmatrix <- function() x  ## retrive the matrix before
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv  ## retrive the cached inverse
    
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)  ## create a list of function
  
}


## The function cacheSolve is to returns the inverse of the matrix in the list

cacheSolve <- function(x, ...) {
  
    inv <- x$getinverse()   ##retrive the inverse
    
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)    # If the object contains an inversed matrix, get the inversed matrix and exit the function 
    }
    
    data <- x$getmatrix()  ##if not ##get the matrix
    inv <- solve(data)  ## Calculate the inverse
    x$setinverse(inv) 
    inv  ## Return a matrix that is the inverse of x
    
}
  
 
  
  
  

     