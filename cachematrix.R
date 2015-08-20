
## Store the inverse of a matrix
## in a variable used as cache
## Returns a list with the value of the matrix and its inverse.
## This list is also used to set the value of a new matrix and the initial calculation of its inverse

makeCacheMatrix <- function(x = matrix()) {
	#Variable to store the inverse of the matrix (cache value)
  inv <- NULL
  
  #Set a new matrix to calculate its inverse
  set <- function(new_mat) {
    mat <<- new_mat
    inv <<- NULL
  }
  #Get the matrix
  get <- function() mat
  
  #Set the value in the cache variable (makeCacheMatrix function container)
  setinv <- function(inverse) inv <<- inverse
  
  #Get the inverse of the cache variable
  getinv <- function() inv
  
  #List to set and get all variables
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of a matrix using the list restuned by makeCacheMatrix
## It gets the value of the matrix and checks whether the inverse has been already calculated

cacheSolve <- function(x, ...) {
	#Get the cache value and check whether it's null
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #No cache value. Calculate the inverse and store that value in the cache variable
  data <- mat$get()
  
  #Calculate inverse
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}
