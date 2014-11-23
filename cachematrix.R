## Solving the inverse of a matrix is a computationally intensive process for large matrices.
## The following functions allow the results of matrix inversion to be cached, such that subsequent 
## invocation of the cacheSolve function returns the cached results. 


## The makeCacheMatrix function, creates a list of functions to:
##      set the value of the matrix
##      get the value of the matrix
##      set the inverse of the matrix
##      get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverseMatrix) inv <<- inverseMatrix
    
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## The cacheSolve function computes the inverse matrix created by the makeCacheMatrix function
## It first checks to see if the inverse has already been computed. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix and sets the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInv()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    
    inverse <- solve(data, ...)
    x$setInv(inverse)
    
    inverse
}
