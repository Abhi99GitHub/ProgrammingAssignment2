## The makeCacheMatrix takes in a matrix as a parameter and has the functions stored which will return the inverse of 
## a matrix. In the cacheSolve function, the getInverse() method from the makeCacheMatrix function is called. If the inverse
## of a matrix is already stored in cache, then the inverse of the matrix is not calculated and the inverse from the cache is
## displayed on the screen. If the inverse of the matrix is not found in the cache, then the inverse of the matrix is calculated and the inverse is
## then displayed on the screen.

## The makeCacheMatrix function stores functions which can calculate the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function checks if the inverse of a matrix is already stored in cache. If the inverse of a matrix is
## stored in cache, then the "getting cached data" message is displayed and the inverse is displayed on the screen.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        DataValues <- x$get()
        i <- solve(DataValues, ...)
        x$setInverse(i)
        i
}
