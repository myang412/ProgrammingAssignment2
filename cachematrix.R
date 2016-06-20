## Coursera, Programming Assignment- Caching the Inverse of a Matrix
## The assignment is to write a pair of functions that cache the
## inverse of a matrix.  


makeCacheMatrix <- function(x = matrix()) {
        #inverse matrix default value- NULL
        inverseMatrix <- NULL
        
        set <- function(y){
                x <<- y
                inverseMatrix <<- NULL
        }
        # get the value of matrix
        get <- function() x
       
        # set inverse matrix
        setInverse <- function(inverse) inverseMatrix <<- inverse
       
        # get inverse matrix
        getInverse <- function() inverseMatrix
        
        #create the list
        list(set = set, get = get, 
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the inverse matrix
        inverseMatrix <- x$getInverse()
        
        #Check the matrix if it is not null
        if(!is.null(inverseMatrix)){
                message("getting cached data")
                return(inverseMatrix)
        }
        
        # calculate and return the inverse
        data <- x$get()
        inverseMatrix <- solve(data,...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
        
