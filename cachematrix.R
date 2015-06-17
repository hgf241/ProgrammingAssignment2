## Functions to return an inverse matrix. The first call calculates 
## the inverse matrix. Each following call uses the cached matrix.
## inverseMatrix <- cacheSolve(makeCacheMatrix(my_matrix))

## This function caches the inverse matrix.
## Introduces methods to access the actual and the inverse matrix.

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


## This function returns a inverse matrix. The first call calculates
## and stores the inverse matrix for subsequent calls. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
