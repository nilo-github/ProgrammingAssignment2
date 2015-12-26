## The following functions return the inverse of a square matrix. 
## Where the inverse of the matrix has already been calculated and cached,
## this function will return the cached result. Otherwise, the
## inverse will be calculated, cached and returned

## This function creates a special "matrix" object
## that can cache its inverse. The argument to the function should be a
## matrix. However, the argument to the function will create a matrix from
## the input parameter.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    ## Create the function "set" that will set the value of the variable x  
    ## for the vector list object to the value of the input parameter y for the function
    set <- function(y) {
      x <<- y
      m <<- NULL
    }

    ## Create the function "get" that will return the value of variable x
    get <- function() x

    ## Create the function "setInverse" that will set the value of m to the
    ## value of the inverse of matrix x
    setInverse <- function(solve) m <<- solve
    
    ## Create the function "getInverse" that will retrun the value of m
    getInverse <- function() m
    
    ## construct a list containing the get, set, getInverse and setInverse
    ## functions defined above and return
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The following function returns the inverse of the matrix in the matrix list object x.
## The inverse will be returned from the cache in the case that it already exists. 
## Otherwise, the inverse will be calculated and cached.

cacheSolve <- function(x, ...) {
    
    ## Try to obtain the matrix inverse using the getInverse function
    ## from the matrix list object
    m <- x$getInverse()
    
    ## If the matrix inverse was found above, return it
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## Where the inverse matrix was not found in the cache, get the matrix data 
    ## from the matrix list object, calculate the inverse of the matrix, set
    ## the value of the inverse for the matrix list object and return the value of 
    ## the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  
}
