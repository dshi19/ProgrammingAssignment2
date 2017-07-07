## The first function 'makeCacheMatrix' returns a list of
## functions that can:

## 1. Set the value of the original matrix
## 2. Get the value of the original matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    x <- data.matrix(x)
    inv <- NULL
    set <- function(new_matrix) {
        x <<- data.matrix(new_matrix)
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function 'cacheSolve' calculates the inverse of the
## list created by the 'makeCacheMatrix' function. First,
## 'cacheSolve' checks if the inverse matrix has already
## been calculated. If it has been, it will skip the
## calculation and return the inverse. If it has not been
## calculated, it will find the inverse and set the inverse
## in the cache using the 'setInverse' function, assuming
## that the original matrix has a determinant not equal to 0.
## Otherwise, an error message will be returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    if (det(mat) != 0) {
        inv <- solve(mat)
        x$setInverse(inv)
        return(inv)
    }
    else {
        message("Error: Inverse cannot be found. Inserted matrix's determinant is 0.")
    }
}
