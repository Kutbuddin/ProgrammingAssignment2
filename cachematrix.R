## These functions calculate the inverse of a matrix
## and cache the result. Calling cacheSolve will use
## the cached inverse, unless the matrix has changed.

## Create a matrix with a cache-able inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y = matrix()){
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setInv <- function(inverse) inv <<- inverse
		getInv <- function() inv
		list(
				set = set, 
				get = get,
				setInv = setInv,
				getInv = getInv )
}


## Solve for the inverse matrix using the cache
## if present and if the matrix is unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invv <- x$getInv()
        if(!is.null(invv)){
        	message("getting cached data")
        	return(invv)
        }
        data <- x$get()
        invv <- solve(data, ...) # inverse of square matrix, R-base package 
        x$setInv(invv)
        invv
}
