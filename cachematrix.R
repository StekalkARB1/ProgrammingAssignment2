## These two functions combined are used to cache the inverse of a matrix
## in order to avoid a second computation if the input parameter is not changing

## makeCacheMatrix takes in input the matrix that will be inverted, then creates a list
## with four functions, get, set, getinv, setinv; this list is a "format" readable by the following 
## function cacheSolve. After the initial input x <- makeCacheMatrix(matrix), the matrix can be updated
## using x$set(newmatrix) (this will also reset the cached valeue to NULL), read using x$get(),
## and its inverse can be read using x$getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initializes inv with a NULL value
        set <- function(y) {
                x <<- y
                inv <<- NULL
				## sets takes a new input and flushes the cache
        }
        get <- function() x ## returns the value of the input matrix
        setinv <- function(inverse) inv <<- inverse ## sets the value of inv (called by the second function)
        getinv <- function() inv ## returns the inverse stored (or NULL)
        list(set = set, get = get, ## names every function
             setinv = setinv,
             getinv = getinv)
}


## This function takes in input an object created with makeCacheMatrix, then uses x$getinv() to check
## if the inverse is already stored in inv; if yes, it returns that value, if not it computes
## the inverse and uses x$setinv to store the result; the second run with the same input will 
## return the cached value.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		## the code above takes the stored mean and checks if it is null or not (in this case it returnes the stored result, avoiding computations)
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
		## if there is not cached inverse, the function computes and returns it, and stores the value using setinv
}
