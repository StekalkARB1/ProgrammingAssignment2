## These two functions combined are used to cache the inverse of a matrix
## in order to avoid a second computation if the input parameter is not changing

## makeCacheMatrix takes in input the matrix that will be inverted, then creates a list
## with four functions, get, set, getinv, setinv; this list is a "format" readable by the following 
## function cacheSolve. After the initial input x <- makeCacheMatrix(matrix), the matrix can be updated
## using x$set(newmatrix) (this will also reset the cached valeue to NULL), read using x$get(),
## and its inverse can be read using x$getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
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
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
