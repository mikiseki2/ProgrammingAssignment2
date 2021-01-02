#This is the second Programming assignment
#Here a function to create and manniluate cache of a matrix is created
# The function is created of two parts:

#makeCacheMatrix and cacdheSolve.
# In the first one a special matrix obkect is created while
# in the second funciton returns an inverse of of the matrix object



makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL # inverse matrix
  
  #set function for the matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  #set the return matrix object 
  get <- function() {
    return(x) #show the matrix
  }
  
  # defining the inverse matrix 
  setInvMatrix <- function(invM){
    invMatrix <<- invM #assign the inverse matrix to the environment var
  } 
  
  # function for returning the inverse matrix
  getInvMatrix <- function(){
    invMatrix #the environment var inverse matrix is return
  }
  
  #list of function for futere call within the environment
  list(set = set,
       get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


#Here starts the second part of the function
#The function will check if the inverse object already exist,
# later will return the inverse object in cache if it satisfy the rule
# or will compute the inverse matrix

cacheSolve <- function(x, ...) {
    invM <- x$getInvMatrix() #obtain the original matrix
  
  # checking is the inverse matrix already created and is it identical
  if (!is.null(invM)){
       if ( identical( x$get() %*% invM, invM %*% x$get() ) ){
         print("getting cached data")
      return(invM)
    }
  }
  
# Inverse matrix null, calculate the inverse matrix
  data <- x$get()
  invM <- solve(data, ...)
  
  x$setInvMatrix(invM)
  
# Return a matrix that is the inverse of 'x'
  print("getting new computated data")
  return(invM)
}