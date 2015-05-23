##These two functions allow you to 
#(1) solve a matrix (get an inverse) for an input matrix
#(2) store the inverse matrix in a new environment
#(3) cache the inverse matrix when the same input matrix is loaded again and give you a note
#saying "getting cached data"
#(4) If new matrix is loaded then it will calculate the inverse of that matrix and add the inverse to
#m environment


## Description about makeCacheMatrix function
#(1) takes an argument (matrix)
#(2) calculate it's inverse
#(3) store the results in m environment
makeCacheMatrix <- function(x=numerix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Description about cacheMatrix function
#This matrix goes into the m environment (above function) and check if the
#inverse of that matrix exist.  If it exists, then it will skip calculating the inverse of that
#matrix and pull the stored results and print it with a message "getting cached data".
#If the m environment don't have the stored inverse then it will go ahead and calculate the
#inverse and print it and also store the results in the m environmet
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

##example
#create a 3x3 matrix called "z"
z <-matrix(c(1,1,2,3,1,3,1,2,4),3,3)
z

#use matrix "z" as an argument in the makeCacheMatrix function
CheckCacheMatrix_z <- makeCacheMatrix(z)

#now use function "cacheSolve to print the inverse of that z matrix.
cacheSolve(CheckCacheMatrix_z)

#If you run this command again, you will get results with a note "getting cached data"
#indicating these results were not calculated but pulled up from the "m" environment
cacheSolve(CheckCacheMatrix_z)


