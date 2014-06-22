## The functions below allow for calculating the inverse of a matrix by comparing
##   the matrix to a solution cache. if we already have the solution cached, we
##   output the cached solution. If not, we solve the matrix and add the solution
##   to the cache

## This function creates a special matrix object containing functions to read/write
##   matrices, and read/write the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
	## define our custom set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
	## define our custom get function
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
	## caching solve to a list that will be checked by cacheSolve()
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function calculates the inverse of the matrix created by the above function,
## first checking to see if we've already calculated the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	## check to see if we've already solved this matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
	## if not solved, then solve it
    data <- x$get()
    m <- solve(data, ...)
	## cache our solution
    x$setsolve(m)
	## print the solution
    m
}