## makeCacheMatrix
    # creates a matrix object to cache the inverse of a matrix
## cacheSolve 
    # computes the inverse of a matrix returned by makeCacheMatrix
    # if the inverse has already been calculated then it retrieves 
    # the inverse from the cache
## Example of use
    # > xMat[1,1]<-2
    # > x <- makeCacheMatrix(xMat)
    # > cacheSolve(x)
    # > x$getInverse()
###################################################################
# creates a matrix object to cache the inverse of x
###################################################################
makeCacheMatrix <- function(x = matrix()) {
    # create a matrix object to cache the inverse of x
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(mInverse) m <<- mInverse
    getInverse <- function() m
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse
         )
}

###############################################################
# computes the inverse of a matrix or returns its inverse if it 
# has been calculated then retrives the inverse from the cache
###############################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if( !is.null(m)){
        message("getting cached data")
        return(m)
    }
    aMatrix <- x$get()
    aInverse <- solve(aMatrix)
    x$setInverse(aInverse)
}
