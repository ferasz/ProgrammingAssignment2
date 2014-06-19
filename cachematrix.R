## The following code contains two functions - makeCacheMatrix and cacheSolve which are used to determine and display the inverse of a square matrix
## The calculated inverse is stored in cache
## If the inverse has to be calculated again while the matrix has not been changed(still the same) the inverse will be returned from cache and no inverse computation shall take place
## If the matrix has been changed, the inverse shall be calculated and returned to the user


#The makeCacheMatrix develops four functions - set, get, setinv and getinv
#This functions takes as an input a matrix "x" which is to be defined by the user

makeCacheMatrix <- function(x = matrix()) {

# "s" is defined as the inverse of the matrix "x" which is initialized to NULL

        s <- NULL

# The set function defines the matrix "x" and assigns values to its various rows/columns outside the current environment using "<<-"
#The inverse is returned as NULL outside the function's environment in case the matrix was changed

        set <- function(y) {
                x <<- y
                s <<- NULL
        }

# The get function returns matrix "x"

        get <- function() x 

# The setinv defines the inverse of matrix "x" outside its current environment

        setinv <- function(inv) s <<- inv

#The getinv function returns the inverse of matrix "x"

        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv ,
             getinv = getinv )
}


# The cacheSolve function takes matrix "x" as an input and returns its inverse to the user
# If the inverse of the matrix "x" was computed earlier and the matrix has not been changed then the inverse will obtained from cache and skips doing the computation again

cacheSolve <- function(x, ...) {

# The inverse of matrix "x" is set as the ouput of the getinv function from the makeCachematrix function

        s <- x$getinv()

# If the inverse is not NULL indicating that the matrix has not been modified/changed the inverse will be returned from cache with no further computations
# Moreover a message will be displayed showing that the inverse was gotten from cache

        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }

#If the inverse is NULL indicating that the matrix has been changed, the new matrix will obtained from the get function
# The inverse of the matrix will be computed using the solve function
#The inverse will be returned to the user

        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setinv(s)
        s
}
