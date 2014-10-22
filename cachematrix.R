## Example of functions to cache data (inverse matrix in this case)

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## Get the matrix
    get <- function() m
    ## Set the inverse of the matrix
    setInv <- function(inverse) i <<- inverse
    ## Get the inverse of the matrix
    getInv <- function() i

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function check if the inverse matrix is in the cache
## (if has been already calculated)
## and return the cached data or solve the inverse if is not cached.
cacheSolve <- function(x, ...) {
	# Get the inverse from the list
    m <- x$getInv()

    ## Check if the inverse exist (if has already been calculated).
	## Return the inverse if has already been calculated
    if( !is.null(m) ) {
		## Message for "wait for a moment please"
        message("wait for a moment please... ")
		message("Our minions are getting the cached data")
        return(m)
    }

	## Now, the inverse don't exist, so must be calculated.
    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse with matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInv(m)

    ## Return the matrix m that is the inverse of 'x'
    m
}
