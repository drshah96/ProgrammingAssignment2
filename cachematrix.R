#The code consist of two functions: makeCacheMatrix and cacheSolve that catches the inverse of Matrix
#makeCacheMatrix creates the special matrix that caches itself while solveCache function computes the inverse of the matrix

#Creating Special Matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #Setting Matrix
    setMatrix <- function(y){
        x <<- y
        m <<- NULL
    }
    
    #Getting Matrix
    getMatrix <- function() x
    
    #Setting the Inverse for the matrix
    setInverse <- function(resultMatrix) m <<- resultMatrix 
    #Getting the Inverse for the matrix
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse <- setInverse,
         getInverse <- getInverse)
}


#Computation of Inverse Matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    #Checking if the matix is not null
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    #Assigning values of x to Data
    data <- x$getMatrix()
    
    #creating inverse of Matrix
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
