## This function creates a list of functions for getting the matrix, setting the matrix
## Getting the inverse of the matrix, and setting the inverse of the matrix
## Allows for the caching of the inverse of the matrix in the environment

makeCacheMatrix <- function(x = matrix()) {
	
	  inverse_x <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) inverse_x <<- inverse
        getinverse <- function() inverse_x

        list(set = set, 
		 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function returns an inverse of the matrix x.  
## If the inverse is cached, it returns the cached value
## Otherwise it solves the inverse and sets that value in the cache for x

cacheSolve <- function(x, ...) {
	cacheinverse <- function(x, ...) {
	  	# First try to get a cached version of the inverse	
        	inverse_x <- x$getinverse()

	  	#if the cached version exists, return it
        	if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        	}
		
	  	# If there is no cached version, call solve to get the inverse
	  	# then set the cache to the value and return it
        	data <- x$get()
        	inverse_x <- solve(data, ...)
        	x$setinverse(inverse_x)
        	inverse_x
	}

