## These two methods are used to cache the inverse of a matrix.
## makeCacheMatrix creates the matrix "object" and cacheSolve calculates the inverse.
## For example,
## > solve(matrix(c(1,2,3,4),2,2))
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Instantiate a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## x is the output of makeCacheMatrix.  Returns the inverse of the matrix, cached if applicable.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}

