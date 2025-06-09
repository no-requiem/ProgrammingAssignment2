# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
      
      # set function
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(i) inverse <<- i
      
      getInverse <- function() inverse
      list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function returns the inverse of a matrix as efficiently as possible
# by using the cached inverse matrices

cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
