
## The function 'makeCacheMatrix' takes a square matrix as argument and returns a
## list containing the following functions:
## 'set': Sets the value of the matrix.
## 'get': Gets the value of the matrix.
## 'setInvMatr': Stores the value of the inverse matrix in the cache.
## 'getInvMatr': Gets the value of the inverse matrix from the cache.
## Examples:
## Set new matrix: 'm1 <- makeCacheMatrix(matrix(c(4,6,3,7),2,2))'
## Set new matrix for m1 (this will also clear the cache): 'm1$set(matrix(c(4,6,3,7),2,2))'
## Get matrix stored in m1: 'm1$get()'
## Get inverse matrix stored in m1 (returns NULL if empty): 'm1$getInvMatr()'

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInvMatr <- function(inverse) inver <<- inverse
    getInvMatr <- function() inver
    list(set = set,
         get = get,
         setInvMatr = setInvMatr,
         getInvMatr = getInvMatr)
}


## The function 'cacheSolve' takes the list previously created by 'makeCacheMatrix' as
## argument and returns the value of the inverse matrix. If the inverse matrix
## is not stored in the cache, the value is calculated by using the solve() function.
## If the inverse matrix is stored in the cache, the value is retrieved from there instead
## of calculating it again. This approach is much faster with larger matrixes.
## Examples:
## Calculate inverse matrix for m1: 'cacheSolve(m1)'. If the result has already been
## calculated, cacheSolve will get the inverse matrix from the cache and you will see the
## message "getting cached data" followed by the result.

cacheSolve <- function(x, ...) {
    inver <- x$getInvMatr()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    m <- x$get()
    inver <- solve(m, ...)
    x$setInvMatr(inver)
    inver
}

