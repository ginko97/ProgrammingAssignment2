## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse property as NULL
    set <- function(y) {
        x <<- y      # Assign the new matrix to 'x'
        inv <<- NULL # Reset the inverse cache since the matrix has changed
    }
    get <- function() x  # Return the matrix
    setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getinverse <- function() inv  # Return the cached inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Check for a cached inverse
    if (!is.null(inv)) {   # If the inverse is already cached
        message("getting cached data")
        return(inv)        # Return the cached inverse
    }
    mat <- x$get()         # Get the original matrix
    inv <- solve(mat, ...) # Compute the inverse
    x$setinverse(inv)      # Cache the computed inverse
    inv                    # Return the inverse
}
