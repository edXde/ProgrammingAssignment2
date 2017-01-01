## ###############################################################
## The following functions implement a means to reduce the cost of 
## computing the inverse of a large unchanging matrix. This is 
## based heavily on the example given in the programming 
## assignment instructions. I've also included a simple test 
## function to exercise the code.
## ###############################################################

## makeCacheMatrix defines a 'class' which stores an invertable
## matrix and it's inverted value. 

makeCacheMatrix <- function(original = matrix()) {
  inverted <- NULL
  set <- function(m) {
    original <<- m
    inverted <<- NULL
  }
  get <- function() original
  setinverted <- function(i) inverted <<- i
  getinverted <- function() inverted
  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


## cacheSolve uses the solve function to invert the matrix stored in an 
## cachedMatrix object

cacheSolve <- function(x, ...) {
  inverted <- x$getinverted()
  if(!is.null(inverted)) {
    message("Getting cached inverted matrix")
    return(inverted)
  }
  original <- x$get()
  inverted <- solve(original, ...)
  x$setinverted(inverted)
  inverted
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

## testProgrammingAssignment2()


