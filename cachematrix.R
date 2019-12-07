## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL                      #initialising inverse_matrix as null
  set <- function(y) {                        # assigning new matrix via set function
    x <<- y                                   
    inverse_matrix <<- NULL                   # for new matrix, inverse_matrix is set to null  
  }
  get <- function() x                         # gets the value of matrix
  set_inverse <- function(inverse) inverse_matrix <<- inverse   # sets the value of matrix inverse 
  get_inverse <- function() inverse_matrix                      # gets the value of matrix inverse
  list(set = set, get = get,                  # to refer to functions
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## makeCacheMatrix calculates the inverse of matrix but first checks to see if the inverse 
## has already been calculated.If so, it gets the inverse from the makeCacheMatrix 

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)) {               #if inverse_matrix is not null
    message("getting cached matrix inverse")   # diplay message and return inverse_matrix
    return(inverse_matrix)    
  }
  data <- x$get()                             # get matrix data
  inverse_matrix <- solve(data, ...)          # using solve to calculate inverse
  x$set_inverse(inverse_matrix)               # set inverse_matrix
  inverse_matrix                              #return inverse_matrix 
}

## Testing 
## 2 x 2 matrix
test <- matrix(1:4,2,2)
mat <- makeCacheMatrix(test)
mat$get()
mat$get_inverse()
cacheSolve(mat)
## Test for caching
cacheSolve(mat)


## 2 x 2 matrix
test <- matrix(c(2,1,2,2),2,2)
mat <- makeCacheMatrix(test)
mat$get()
mat$get_inverse()
cacheSolve(mat)
## Test for caching
cacheSolve(mat)

## 4 x 4 matrix
test <- matrix(c(3,3,6,1,6,9,0,0,1,9,0,0,3,1,1,5),4,4)
mat <- makeCacheMatrix(test)
mat$get()## 4 x 4 matrix
mat$get_inverse()
cacheSolve(mat)
## Test for caching
cacheSolve(mat)
