## The following functions compute the inverse of a matrix and save them in a cache.
 
## makeCacheMatrix function requires an argument of type matrix (or array)
## and it returns a special "matrix" in the form of a list with functions
## to set and get the matrix, and to set and get its inverse.

## cacheSolve function either retrives inverted matrix from the cache
## or computes inverted matrix if it is not available in the cache
## and then returns this inverted matrix
## Note: Only numeric (or complex) and square matrices which are
## non-singular (determinant of matrix is not 0) are invertible.

## Example 1:
## m <- matrix(c(1,2,2,1), nrow = 2)
## mat1 <- makeCacheMatrix(m)
## cacheSolve(mat1)

## Example 2:
## y <- array(c(1,2,3,14,25,6,-7,8,9), dim = c(3,3))
## mat2 <- makeCacheMatrix(y)
## cacheSolve(mat2)

## Example 3:
## Empty original matrix
## mat3 <- makeCacheMatrix()
## cacheSolve(mat3)
## This will return 1x1 matrix containing "NA".

##creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
    #local variable to set function to call for inverting the matrix
    a <- NULL
    
    #function to set the non-changing matrix
    #in order to retrive it in cacheSolve function
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    
    #function to get the non-changing matrix in cacheSolve function
    get <- function() x
    
    #function to set the function to call for inverting the matrix
    #in order to retrive it in cacheSolve function
    setinverse <- function(solve) a <<- solve
    
    #function to get the inverted matrix in cacheSolve function
    getinverse <- function() a
    
    #return the list containing the above four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##computes/retrieves the inverse of the special "matrix" object
##returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
    #retrieve the inverted matrix
    a <- x$getinverse()
    
    #if the inverted matrix is not null, do not compute it again
    #but display appropriate message and return it from the cache
    #(which will exit cacheSolve function)
    if(!is.null(a)) {
        message("getting cached inverted matrix")
        return(a)
    }
    
    #if the inverted matrix is null, create, cache and return the inverted matrix
    
    #first, get the matrix using get function defined in makeCacheMatrix function
    mat <- x$get()
    
    #second, find the inverse of the retrieved matrix assuming it is invertible,
    #that is, numeric/complex and square matrix which is non-singular
    #(insert code here to check for invertibility of matrices - not required for this assignment)
    a <- solve(mat)
    
    #third, call setsolve function defined in makeCacheMatrix function
    #in order to cache the inverted matrix
    x$setinverse(a)
    
    #finally return the inverted matrix
    a
}
