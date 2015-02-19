## This pair of functions uses <<- operator to cache a result of caculation
## hence to save time for recalcuating by retrieving the cached result

## makeCacheMatrix contains 4 sub functions to set/get 
## the special Matrix object, and set/get the inverse of the Matrix
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse of the Matrix
## 4. get the value of the inverse of the Matrix 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve function calculates the inverse of the special Matrix
## created with the above funciton (makeCacheMatrix). If the inverse
## already exists, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the Matrix
## and cache the result via the setInverse sub-function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    theMatrix <- x$get()
    inverse <- solve(theMatrix, ...)
    x$setInverse(inverse)
    inverse
}
