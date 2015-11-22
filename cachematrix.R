  ## Below are two functions that are used to create a special object 
  ## that stores a matrix and cache's its inverse matrix.
  ## (c) Assignment2 description (Above and below comments)
  
  ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  ## It is really a list containing a function to
  ## -set the value of the matrix 
  ## -get the value of the matrix
  ## -set the value of the inverse matrix
  ## -get the value of the inverse matrix 
  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## -set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## -get the value of the matrix
  get <- function() x
  
  ## -set the value of the inverse matrix
  setInverse <- function(solve) m <<- solve
  
  ## -get the value of the inverse matrix 
  getInverse <- function() m
  
  ## return a list where each element is a function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  m <- x$getInverse()
  
  ##  Check if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##  If not calculate the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}