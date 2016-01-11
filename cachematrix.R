## Functions to construct matrices and cache the inverse so the inverse of a matrix
## can be stored and used instead of computing it everytime it is needed

## makeCacheMatrix creates a special 'matrix' which is
## a list of functions that set/get a Matrix
## and set/get the inverse of a Matrix. 
## When this function is invoked, it sets the inverse to null

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of the special matrix 'x'
## if the inverse has been calculaed previously it returns the cached value
## otherwise it calculates the inverse using solve() function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


## Example of execution
## > x
## [,1] [,2]
## [1,]    2    1
## [2,]    1    2
## > m<-makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]
## [1,]  0.6666667 -0.3333333
## [2,] -0.3333333  0.6666667
## > cacheSolve(m)
## getting cached data
## [,1]       [,2]
## [1,]  0.6666667 -0.3333333
## [2,] -0.3333333  0.6666667
