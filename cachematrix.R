## the first function makeCacheMatrix creates a special "matrix" object 
## and can cache its inverse. 
## the second function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.


## creates a special "matrix", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(inv) {
                inverse <<- inv
        }
        
        getInverse <- function() {
                inverse
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function cacheSolve calculates the inverse of the special "matrix" created with the
## function makeCacheMatrix. It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
## via the setmean function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        
        if(is.null(inv)) {
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                inv
        }
        else {
                message("getting cached data")
                inv
        }
        
}
