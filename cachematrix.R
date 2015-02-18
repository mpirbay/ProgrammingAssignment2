## These functions provide setter and getter for a matrix and its inverse.
# another function calculates the inverse.
# The aim is to avoid recalculation of the inverse if this has already been previously

## The following function returns a list of 4 functions which are the getter/setter of
# matrix and its inverse


makeCacheMatrix <- function(mx = matrix())
{
	# initialize inverse of matrix at NULL
    inverse <- NULL
    set <- function(my) # set a new matrix
    {
	#sets the matrix in the parent environnement to the value passed
        mx <<- my

	# assigns NULL to the inverse of a newly set matrix since it has not been calculated yet. The inverse is stored in the parent environment
        inverse <<- NULL
    }

	# returns the currently stored matrix
    get <- function() mx

	# assigns the value inv to the  variable "inverse" in the parent environment
	# this function is called by cacheSolve to set the inverse
    setinverse <- function(inv) inverse <<- inv

	#returns the currently stored inverse matrix.
                                        getinverse <- function() inverse

	#returns a list of 4 functions setter and getter for matrix and its inverse
                                        list(set = set, get = get,
                                                setinverse = setinverse,
                                                getinverse = getinverse)
}


# This function calculates the inverse of the makecachematrix object if it has not been
#calculated previously. ALternatively it returns the cached value
cacheSolve <- function(mx), ...)
{

	# call to getinverse to retrieve the inverse of the matrix
    inverse <- mx$getinverse()

	# if the inverse is not NULL, it means it has already been calculated for the current stored matrix
    if(!is.null(inverse))
    {
	# then it just retrieves the stored inverse
        message("getting cached data")
        return(inverse)
    }

	#else retrieve the matrix
    data <- mx$get()

	#calculate the inverse
    inv <- solve(data, ...)

	#store it in the parent environment
    inverse<<-mx$setinverse(inv)

	#returns the calculated inverse
           inv
}


