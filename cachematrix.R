## Put comments here that give an overall description of what your
## functions do
##      set the value of the special matrix
##      get the value of the special matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

## Write a short comment describing this function
## This function create an object that stores a matrix and
## calculates and chache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) mi <<- solve
        getsolve <- function() mi
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
        

## Write a short comment describing this function
## Calculates the inverse of a matrix created with the above function.
## Also check's if the inverse was calculated previously, in that case
## it return the cache value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getsolve()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setsolve(mi)
        mi
}
