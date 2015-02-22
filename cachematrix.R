## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse_param) inverse <<- inverse_param
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

# Examples:
# > m <- makeCacheMatrix()
# > m$set(matrix(c(1,1,1,3,4,3,3,3,4), 3, 3))
# > m$get()
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
# > mi <- <- cacheSolve(m)
# > mi
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > mi <- cacheSolve(m)
# getting cached data
# > mi
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > m$get() %*% mi
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1