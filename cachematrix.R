#Matrix inversion is a costly computation and there may be some benefit to
# cathing the inverse of a matrix rather than computing it repeatedly. 

# makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse. 

mmakeCacheMatrix <- function(x = matrix()) {
      i <- NULL
    set <- function(y) {
         x <<- y            # "<<-" is a scoping assignment in R
         i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by 'makeCacheMatrix' function created above.If the inverse has 
# already been calculated and the matrix has not changed, cacheSolve will retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  # This returns a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## TESTING MY SOLUTION
# Calling the function with the matrix will 
    # 1. compute the inverse
    # 2. retrieve the inverse from the cache list
    # 3. change the call matrix to the inverse
    # 4. compute the inverse on that and return the original function

A <- matrix(c(1:4),2,2) # create a matrix having two rows and two columns
A1 <- makeCacheMatrix(A) # crate a special matrix object(A) that can 
                          # catche its inverse
A1$get()
    [,1] [,2]
[1,]    1    3
[2,]    2    4

A1$getinverse()
NULL
cacheSolve(A1)  # this will return the inverse after computation
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(A1)
> cacheSolve(A1)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

A1$getinverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

A1$set(matrix(c(1, 1, 2, 4), 2, 2))
A1$get()
     [,1] [,2]
[1,]    1    2
[2,]    1    4

A1$getinverse()
NULL

cacheSolve(A1)
     [,1] [,2]
[1,]  2.0 -1.0
[2,] -0.5  0.5

cacheSolve(A1)
getting cached data
     [,1] [,2]
[1,]  2.0 -1.0
[2,] -0.5  0.5

A1$getinverse()
     [,1] [,2]
[1,]  2.0 -1.0
[2,] -0.5  0.5
