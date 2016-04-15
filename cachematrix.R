## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Function to get the matrix
  get <- function() x
  
  #Function to set the matrix inverse
  setinverse <- function(matrixinverse)
    m <<- matrixinverse
  
  #Function to get the matrix
  getinverse <- function() m
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  
  #checking if the inverse already exists
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  
  #if not already cached compute new inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
