## These two functions together calculate the inverse of a square matrix
## The result is put in a cache, so that the next time that the inverse of the same matrix
## is needed it is retrieved from the cache, instead of doing the costly calculation again

## Make an object that stores the original matrix and its inverse in another environment (cache)

makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mi <<- inverse
	getinverse <- function() mi
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function tries to get the inverse of the the matrix stored in the cache of the object x
## and return it
## If the inverse is not found in the cache, then the inverse is calculated and the value is
## stored in the cache of the object x and then returned

cacheSolve <- function(x, ...) {
	mi <- x$getinverse()
	if(!is.null(mi)) {
		message("getting cached data")
		return(mi)
	}
	data <- x$get()
	mi <- solve(data, ...)
	x$setinverse(mi)
	## Return a matrix that is the inverse of 'x'
	mi
}
