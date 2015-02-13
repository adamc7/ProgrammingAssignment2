## R Programming Assignment 2
## cachematrix.R Caching the Inverse of a Matrix

## makeCacheMatrix creates a special matrix that can be cahched
## cacheSolve solve (inverse) a matrix with a cache


makeCacheMatrix <- function(x = matrix()) {
    ## The makeCacheMatrix creates a special matrix a special "matrix" object that can cache its inverse.
    ##
    ## x is the input matrix 
    ##
    ## The function return a list with the following fuctions:
    ##   set: set the value of the matrix
    ##   get: get the value of the matrix
    ##   setInv: get the value of the inverse matrix
    ##   getInv: set the value of the inverse matrix
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL #reset inv matrix to NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    ## Matrix inverston is a costly operation,
    ## so if the matrix was already invereted the result is cached
    ##
    ## x is an input matrix to inverse which was created with makeCacheMatrix
    ## Output inverse matrix of X
    inv <- x$getInv()
    if(!is.null(inv)) {
        # inv is not null - matrix in cache
        # DEBUG: message("getting cached data")
        return(inv)
    } else {
        # Here inv is nul.  Matrix was not in cache 
        data <- x$get() 
        inv <- solve(data) #explcity inverse matrix
        x$setInv(inv) #Store in cache
        return(inv) 
    }
    
}
