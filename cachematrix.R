## To return the inverse of a matrix obtained from the user.
## If the value is already computed and stored in cache then existing value is returned.
## Else new inverse is calculated and returned.

## This function is used to get/set the value of matrix and its inverse 

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


## This function is used to get user input 
## Compute inverse and return the value to user.

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
