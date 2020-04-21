## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  #Create a null object that will hold the value of the inverted matrix
  solveMatrix <- NULL
  
  #This function will assing a new value for the matrix in the parent envieroment
  #thats why the (<<) is used 
  setMatrix <- function(y) {
    x <<- y
    solveMatrix <<- NULL
  }
  
  #Calling the value of the matrix argument
  getMatrix <- function() {x 
  }
  # Setting the inverse of the matrix
  setInverse <- function(inverse){
    solveMatrix <<- inverse
  }
  # Getting the inverse of the matrix
  getInverse <- function(){ 
    # Return the inverse 
    solveMatrix  
  }
  # Return a list of the methods; that will be used to refer to the fuctions using $
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


#Estimates the inverse of the matrix produced with "makeCacheMatrix" function. 
cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  solveMatrix <- x$getInverse()
  
  #If the inverse has been calculated (and the matrix has not changed),
  #then the "cachesolve" will retrieve the inverse from the cache.
  if(!is.null(solveMatrix)) { 
    message("fetching the invertible data")
    #return the inverse matrix
    return(solveMatrix) 
  }
  
  #When value of the invertible matrix is NULL then
  MatrixData <- x$getMatrix() 
  
  #Solving the matrix
  solveMatrix <- solve(MatrixData, ...) 
  
  ## Set the inverse to the object
  x$setInverse(solveMatrix)
  
  ## Return the matrix
  return(solveMatrix) 
}

#Testing the thing
test <- c(5,9,2,1,4,3,2,8,0)
dim(test) <- c(3, 3)
CacheMatrix <- makeCacheMatrix(test)
c3 <- cacheSolve(CacheMatrix)
c3    
