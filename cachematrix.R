## The set of fucntions creates and/or caches the inverse of any given matrix 
##provided the inverse exists. 

## The first function creates a special "matrix" that is a list containing 
## fucntions to set the value of the matrix, get the value of the matrix, 
## set the ineverse and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
    {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function looks to see if the inverse of the given matrix is known.
## If the inverse is known, it returns the cached value. If it is not known
## then the inverse is calculated and is set the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
