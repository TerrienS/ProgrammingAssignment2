
## Caching the Inverse of a Matrix:
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
#of a matrix rather than compute it repeatedly 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_of_matrix <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_of_matrix <<- inverse
        getInverse <- function() inv_of_matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function does the inverse of the special "matrix" created by makeCacheMatrix above. 
#If the inverse has already been calculated with the hope data didn't change in between
# then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_of_matrix <- x$getInverse()
        if (!is.null(inv_of_matrix)) 
        {
                message("getting cached data")
                return(inv)
        }
        matrix_data <- x$get()
        inv <- solve(matrix_data, ...)
        x$setInverse(inv)
        inv
}
