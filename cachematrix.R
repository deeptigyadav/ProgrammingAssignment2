#This function creates a special "matrix" object that can cache its inverse
# 1. set the matrix: setmatrix
# 2. get the matrix: getmatrix
# 3. set the inverse: setinverse
# 4. get the inverse of the matrix: getinverse
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL #this is the cache
  setmatrix <- function(y) {
    x <<- y # input matrix y to x in the environment
    matrix <<- NULL # reset matrix to Null in the environment
  }
  getmatrix <- function() x #returns the matrix x
  setinverse <- function(inverse) matrix <<- inverse # sets the inverse of matrix
  getinverse <- function() matrix # gets the inverse of the matrix matrix
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix, 
       setinverse = setinverse, 
       getinverse = getinverse)  
}


## this function calculates the inverse of the matrix created above. 
## it first checks to see if the inverse has already been calculated and cached
## If it has been calculated it returns from the cache
## else it calculates the inverse and returns the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix <- x$getinverse() #gets the inverse of the matrix
  if(!is.null(matrix)) { #checks for cached inverse and returns if not null
    message("getting cached data")
    return(matrix)
  }
  data <- x$getmatrix() #get the matrix
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  matrix
  
}
