## makeCacheMatrix makes a special list, containing methods that set and get the matrix
## makeCacheMatrix also stores methods that set and get the inverse from the given matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {inverse}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix of the special list

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}