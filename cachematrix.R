## Put comments here that give an overall description of what your
## functions do


## Function creates a special "Matrix", 
## which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse of Matrix
## get the value of the Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {

  ## Set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set and set the inverse of the matrix
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This Function retrun inverse of the matrix. If inverse already calculated then retrun cached inverse of matrix.

cacheSolve <- function(x, ...) {
       
  ## get the inverse of the matrix    
  m <- x$getmatrix()
  
  ## check If inverse already calculated
  if(!is.null(m)) {
    message("Getting cached inverse of matrix.")
    return(m)
  }
  ## if not get the inverse of the matrix   
  data <- x$get()
  ## Checking for error while computing inverse of matrix.
  m <- tryCatch(
       {
           solve(data, ...) 
       }, 
       error=function(e)
      {
          return("Error while computing inverse of matrix using solve(). Matrix must be invertable using solve().")
      })
  x$setmatrix(m)
  m
}
