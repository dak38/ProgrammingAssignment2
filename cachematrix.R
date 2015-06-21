## Put comments here that give an overall description of what your
## functions do

## Makes a special matrix object with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(y) {
		x <<- y
		x_inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) x_inv <<- solve
	getinverse <- function() x_inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	x_inv <- x$getinverse()
	if(!is.null(x_inv)) {
		message("getting cached data")
		return(x_inv)
	}
	my_matrix <- x$get()
	x_inv <- solve(my_matrix, ...)
	x$setinverse(x_inv)
	x_inv
}