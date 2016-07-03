## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it. 

## Following are a pair of functions which calculate the inverse of a matrix and
## also cache the result for future reference.

## The following is used to create a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix
## created with the above function. Plus if the inverse is already cached,
## then it just retrieves that

cacheSolve <- function(x, ...) {
    inv <- x$getinverse
    if(!is.null(inv)) {
        print("Getting cached data....")
        return(inv)
    }
    ourmatrix = x$get
    inv <- solve(ourmatrix, ...)
    x$setinverse(inv)
    inv                 ## Return a matrix that is the inverse of 'x'
}
