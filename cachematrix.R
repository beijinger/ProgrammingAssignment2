## makeCacheMatrix function creates a matrix object
## that can cache its inverse.

##It is a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of inverse matix
## get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve,
           getsolve = getsolve)
}


## calculates the inverse of the matrix created by
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

