## Matrix inversion is a computationally costly process. The functions created here caclulate the inverse of a matrix
## (if the ivnerse has not already been stored in memory) and stores the inverse of the matrix in the cache.

## This function creates a cache of the inverse (m) of a matrix (x).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }


## The function creates the inverse of a matrix (m) if the inverse (m) is not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

## To test the functions, use the following:

A=matrix(c(5,6,7,8), nrow=2,ncol=2)
A

B = matrix(c(1,2,3,4), nrow=2,ncol=2)
B

aMatrix <- makeCacheMatrix(B)
aMatrix$get() # should return the matrix
aMatrix$getmatrix() # should be NULL
aMatrix$set(A)
cacheSolve(aMatrix) # calculates the inverse of the matrix
aMatrix$getmatrix() # retrieves the inverted matrix in cache
