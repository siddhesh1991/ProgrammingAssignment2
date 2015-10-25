## File contains pair of functions that cache the inverse of a matrix

## makeCacheMAtrix() contains getters and setters for 
## the original matrix and inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        ##setting value of matrix
        x <<- y
        ##setting inverse = null whenever value of matrix changed
        inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseCalculated){
        #setting value of inverse
        inverse <<- inverseCalculated
    }
    getInverse <- function() inverse
    
    ## Returning list of getters and setters
    list(setMatrix = set, getMatrix = get, setMatrixInverse = setInverse,
         getMatrixInverse = getInverse)
}


## cahceSolve() returns inverse of special matrix
## cacheSolve() calculates inverse only when it has not been calculated previously
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getMatrixInverse()
    ## checking if inverse is already available
    if(!is.null(inverse)){
        message("Getting Cached Inverse Of Matrix")
        return (inverse)
    }
    message("Calculating New Inverse Of Matrix")
    dataInverse <- x$getMatrix()
    inverse <- solve(dataInverse,...)
    x$setMatrixInverse(inverse)
    # Returning inverse Matrix
    inverse
}
