## The following functions cache the inverse of a arbitraty matrix in order
## to save computational effort and as required by the coursera course.

## Creates a special matrix object that caches its inverse when already computed.
# As the instructions put it, the program does the following:
#1.  sets the value of the Matrix
#2.  gets the value of the Matrix
#3.  sets the value of the Inverse of the Matrix
#4.  gets the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## sets the value of the Matrix
    Inv<- NULL
    Set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    
    ## gets the value of the Matrix
    Get <- function() x
    
    ## sets the inverse of the Matrix
    SetInverse <- function(solve) Inv <<- solve
    GetInverse <- function() Inv
    
    ## gets the inverse of the Matrix
    list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
    
}

## Returns the inverse of a matrix. If the inverse has already been computed, it gets the result
# and avoids computation. Else, it computes it, and sets the value in the cache using the created
# SetInverse function

cacheSolve <- function(x, ...) {
    
    ## get the inverse of the matrix
    Inv <- x$GetInverse()
    
    ## check if there is the matrix, and if there is, print message stating it and returning the function
    if(!is.null(Inv)) {
        message("Getting cached data, inverse already computed")
        return(Inv)
    }
    
    ## if not: gets the inverse of the matrix
    Data <- x$Get()
    Inv <- solve(Data)
    
    ## and then sets the inverse of the matrix
    x$SetInverse(Inv)
    Inv
}
