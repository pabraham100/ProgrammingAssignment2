## Put comments here that give an overall description of what your
## functions does

## The first function, makeVector creates a special "vector", which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix  <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
		
		## The list of functions that gets returned along with cache if any
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the 
## value of the inverse in the cache via the solve and then setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        inv <- x$getinv()
		## Check if cache is empty - if not return cache value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		## Else compute inverse and set cache value and return inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
