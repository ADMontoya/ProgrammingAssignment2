## Use the functions as the example:
## mx<-matrix(c(2,-3,1,1,1,-2,-1,2,2), nrow = 3, ncol = 3)
## cacheSolve(makeCacheMatrix(mx))

## This function create a special matrix with some methods: get, set, setsolve and getsolve
## set: set the matrix
## get: return the matrix
## setsolve: set the inverse matrix in cache
## getsolve: return the inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}


## This function return the inverse matrix. If the inverse 
## matrix exists in cache, the inverse matrix is getting from 
## it. Otherwise, the inverse matrix is calculated and stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
	if(!is.null(m)) {
		message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
