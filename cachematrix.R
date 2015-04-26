## Put comments here that give an overall description of what your
## functions do
## Implements a cacheable matrix than can cache the invers matrix
## as long as the matrix is not changed

## Write a short comment describing this function
## This function creates a special "matrix" object which is really a list
## containing a function to: set value of the matrix, get the value of the matrix,
## set the inverse and get the invers
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function
## calculates the inverse of a matrix, checking first the inverse from
## the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
