## Avoid recalculating inverse matrices repeatedly.
##
## USE:
## For each matrix, create a caching object once:
## 
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## 
## and then call cacheSolve() repeatedly:
##
## inverseMatrix<- cacheSolve(amatrix)
## ...
## inverseMatrix<- cacheSolve(amatrix)
##
## The actual calculation will be performed only once.
##

## makeCacheMatrix creates and returns an object that
## - contains the original matrix
## - can contain the inverted matrix,
##   initialized to NULL before its calculation

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve returns the inverse of the matrix contained in parameter x
## If the inverse for this matrix has already been calculated, 
## return the cached value contained in x and print a message,
## else calculate the value with solve().

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
