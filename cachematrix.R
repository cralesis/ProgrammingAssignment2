## The functions below create an object that stores a matrix and caches its inverse

## Creates a matrix, containing a function to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function calculates the inverse of the matrix returned by the above.
## The function should retrieve the inverse from the cache if the inverse has already been computed

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
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

## Testing above functions
test <- matrix(c(1, 2, 3, 4), 2, 2)
test1 <- makeCacheMatrix(test)
## Inverse returned after calculation
cacheSolve(test1)
## Getting cached data
cacheSolve(test1)
