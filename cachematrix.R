## ###############################################################
## The following functions implement a means to reduce the cost of 
## computing the inverse of a large unchanging matrix.
## ###############################################################

## makeCacheMatrix defines a 'class' which stores an invertable
## matrix and it's inverted value. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) i <<- inverted
  getinverted <- function() i
  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


## cacheSolve uses the solve function to invert the matrix stored in an 
## cachedMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverted()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverted(i)
  i
}


## testProgrammingAssignment2 is a very simple test of the makeCacheMatrix
## and the cacheSolve functions 

testProgrammingAssignment2 <- function() {
  message("======TESTING======")
  A = matrix(c(1,2,3,1), nrow = 2, ncol = 2)
  message("------A-----")
  print(A)
  
  cA = makeCacheMatrix(A)
  message("------cA-----")
  print(cA)
  
  sA = cacheSolve(cA)
  message("------sA-------")
  print(sA)
  
  sA1 = cacheSolve(cA)
  message("------sA1------")
  print(sA1)
  
  cB = makeCacheMatrix(A)
  message("------cB------")
  print(cB)
  
  sB = cacheSolve(cB)
  message("------sB------")
  print(sB)
}

# testProgrammingAssignment2()


