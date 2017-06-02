## Put comments here that give an overall description of what your
## functions do

## The following function 'makeCacheMatrix' will take a single matrix as a parameter and returns a list of following functions 
## as it return value  
## 1) set function to set a matrix 
## 2) get/ return function to return a matrix
## 3) function to set a internal matrix to its inverse
## 4) function to get/ return the internal inverse matrix 

makeCacheMatrix <- function (m = matrix()) {
    
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    
    get <- function() m
    setInv <- function(m) i <<- m
    getInv <- function() i
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The following function 'cacheSolve' will take a single makeCacheMatrix variable as a parameter and returns it inverse matrix
## if the cacheMatrix is not already cached with Inverse matrix, it will compute the Inverse of the matrix to the variable 
## and returns the inverse matrix to the caller.

cacheSolve <- function (m, ...) {
        ## Return a matrix that is the inverse of 'x'

		i <- m$getInv()
    if(!is.null(i)) {
        message("Getting cached inverse matrix")
        return (i)
    }
    
    data <- m$get()
    m$setInv(solve(data,...))
    i <- m$getInv()
    
    return(i)
}
