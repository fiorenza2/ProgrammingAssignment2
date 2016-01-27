## This function takes an input matrix 'x' and derives a list which has various functions
## that enable us to view and manipulate properties of said input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # declaring the variable for the inverse of the Matrix in the parent function
  
  set <- function(y) { # defining the "set" child function, which allows us to set a new matrix
    
    x <<- y       # set the parent variable for the matrix
    i <<- NULL    # because we now have a new matrix, the inverse has not been calculated
      
  }
  
  get <- function() x   # the 'get' function returns the matrix that is stored within this structure
  
  setinv <- function(inv) i <<- inv # the 'setinv' function sets the inverse variable 'i' to an input value 'inv'
  
  getinv <- function() i # the 'getinv' function returns the inverse variable 'i'
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)  # generate the list which encapsulates this functionality

}


## 'cacheSolve' reads data of the type 'makeCacheMatrix', which is the 'x' variable, and returns
## the inverse of that matrix. It will determine if the inverse has been calculated before, and
## if it has, will return that previous value. If not, it will use 'solve' to determine the
## inverse, print that and store it in the input matrix structure.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() # set a variable 'i' to the inverse value stored in the input matrix structure
  
  if(!is.null(i)) { # if there is a non-null value...
  
    message("getting cached data")  # inform the user that it is pre-calculated...
    return(i) # and return that inverse, exiting the function
    
  }
  data <- x$get() # otherwise, if the value is null, obtain the matrix from the structure
  
  i <- solve(data, ...) # determine the inverse of that matrix
  
  x$setinv(i) # set the inverse of that matrix within the input matrix structure for future use
  
  i  # return the inverse matrix
  
}
