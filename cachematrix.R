## The following functions calculate the inverse of a matrix and saves
## it to the cache such that the next time the user attempts to calculate 
## the matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		
		# Initialize store for inverse matrix
		invers <- NULL

		# Function sets the matrix
		set <- function(y) {
				x <<- y ## assign the input matrix y to the object x 
				## in the parent environment
				invers <<- NULL ## reinitialize object invers to null
				## in the parent environment
		}

		# This function returns the raw matrix
		get <- function() {
				x
		}

				# Way to set the inverse of the matrix
		setinvers <- function(i) {
				invers <<- i
		}

		# This function gets the cached inverse
		getinvers <- function() {
				invers
		}

		# Back a list of the methods
		list(set = set,	get = get,
			 setinvers = setinvers,
			 getinvers = getinvers)    
}


## This function first checks to see if the inverse has already been calculated.
## If the inverse has already been calculated and the matrix has not been changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		# get the cached inverse
		invers <- x$getinvers()

		if(!is.null(invers)) {
        # if the inverse if actually cached, just return it
				message("getting cached inverse")
				return(invers)
		}

		# otherwise, calculate the inverse and cache it
		matr <- x$get()
		invers <- solve(matr, ...)
		x$setinvers(invers)

		return(invers)
}
