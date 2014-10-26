## This R code consists of two functions.
## 1st function creates a list of functions to a) create a square Matrix
## b) get (print) the matrix c) calculate the inverse of this sq. Matrix and
## d) get (print) the Inverse.

## 1st function - makeCacheMatrix.
## Create_Matrix, Get_Matrix, Cal_Inverse & Get_Inverse are the 4 functions in
## makeCacheMatrix function.

makeCacheMatrix <- function(m = matrix()) {
        ## below commands set NULL value to Matrix & Inverse objects in
        ## MakeCacheMatrix function envrionment.
        Inverse <- NULL
        Matrix <- NULL
        ## Create_Matrix takes a numeric value as input and creates a sq.Matrix
        ## of rows & columns equal to the 'input' numeric value.
        ## The Matrix is formed with values <= integer 9.
        ## When a new Matrix is constructed, the Inverse is set to NULL.
        Create_Matrix <- function(r_c) {
                Matrix <<- matrix(sample.int(9, r_c^2, TRUE),r_c,r_c)
                Inverse <<- NULL
        }
        ## Get_Matrix results the constructed Matrix
        Get_Matrix <- function() Matrix

        ## Cal_Inverse calculates the Inverse of the Matrix.
        Cal_Inverse <- function() Inverse <<- solve(Matrix)
        
        ## Get_Inverse results the Inverse Matrix.
        Get_Inverse <- function() Inverse
        
        ## Below list names the functions in makeCacheMatrix function.
        list(Cr = Create_Matrix, Ma = Get_Matrix,
             Ca = Cal_Inverse,
             In = Get_Inverse)
}


## 2nd function calcualtes the Inverse of the given sq. Matrix, if it is not
## calculated already. If Inverse is already calculated, this function reads it
## from the Cache (from the get Inverse function of 1st function)

## 2nd function - cacheSolve

cacheSolve <- function(m, ...) {
        ## Reads the Inverse value from makeCacheMatrix function.
        ## If value exists, it returns the value.
        Inverse <- m$In()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        
        ## If Inverse is NULL (no value exists previously), then it gets
        ## calculated and assigned to m$Ma() of the makeCacheMatrix function.
        Matrix <- m$Ma()
        Inverse <- solve(Matrix)
        m$Ca()
        Inverse
}