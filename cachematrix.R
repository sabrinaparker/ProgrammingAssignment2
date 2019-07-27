## The two functions together create a matrix, check to 
## see if the inverse has already been calculated, then act 
## accordingly. If the inverse has already been caculated,
## the second function retrives the inverse from the cache. 
## If it has not been calculated, the second function 
## completes the computation. 

## The first function creates a matrix, 
## which is essentially a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse, and
##      4. get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}   

## The following function calculates the inverse of the
## matrix created with the above function. 

## It first checks to see if the inverse of the matrix
## has already been calculated. If so, the function
## gets the inverse from the cache (and skips the computation.)
## If the calculation has not been completed, the function
## will calculate the inverse, and then store this value in the cache
## for future computations. This setting of the inverse value is 
## achieved through the "setinverse" function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}   




