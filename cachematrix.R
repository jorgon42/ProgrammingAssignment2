## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#takes three parameters: matrix(content, row number, column number), and stores them in a list

makeCacheMatrix <- function(x=matrix()) {
	m <- NULL
	set <- function(y=matrix()) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# takes function above with its parameters and returns either a cached value of m (if m is not empty) or calculates
# the inverse from scratch

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
# As argument, takes makeCacheMatrix() or an object created by it.
	m <- x$getinverse()
	if(!is.null(m)) {
		message("Inverse cached. Getting cached data.")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
