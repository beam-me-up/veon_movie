# veon_movie
Implement a ticket reservation system for movies. The interactions with the system should be http/json based. Interview with Veon. 

# Build

make

# Run 

make run 

or

./_rel/veon_movie_release/bin/veon_movie_release console

# Test 

make tests 

# Attributes
* `imdbId` is IMDB movie identifier
* `screenId` is an externally managed identifier of information when and where the movie is screened.
* `movieTitle` is the title of the movie
* `availableSeats` the total seats available for this movie
* `reservedSeats` the total number of reserved seats for a movie and screen.


# Implementation 

Note: Single API Server (not developed for distributed server)

### Register a movie

POST /admin/movie
 
    {
        "imdbId": "tt0111161",
        "availableSeats": 100,
        "screenId": "screen_123456"
    }    

201 Created (Registration Success)
409 Conflict (Already Registered)
400 Bad Request

Checks:
{imdbId and screenId} combination is unique
multiple screenId with single imdbId is allowed
movieTitle has been fetched using http://themoviedb.org/documentation/api  


### Reserve a seat at the movie

POST /user/movie

    {
        "imdbId": "tt0111161",
        "screenId": "screen_123456"
    }    

200 OK (Reserved a seat)
403 Forbidden (Seats are full)
404 Not Found (No Movie available on the ScreenID)
400 Bad Request

### Retrieve information about the movie

GET /user/movie/:imdbId/screen/:screenId

Response
200 OK

    {
        "imdbId": "tt0111161",
        "screenId": "screen_123456",
        "movieTitle": "The Shawshank Redemption",
        "availableSeats": 100,
        "reservedSeats": 50
    }   
    
404 Not Found (No Movie available on the ScreenID)
400 Bad Request

### Any other API
400 Bad Request


# Curl Commands

Add Movie: 

$ curl -X POST -d '{"imdbId": "tt0111161", "availableSeats": 100, "screenId": "screen_123456"}' -H "Content-Type: application/json" http://localhost:8080/admin/movie

Book Movie:

$ curl -X POST -d '{"imdbId": "tt0111161", "screenId": "screen_123456"}'  -H "Content-Type: application/json" http://localhost:8080/user/movie

Get Movie:

$ curl -X GET  -H "Content-Type: application/json" http://localhost:8080/user/movie/tt0111161/screen/screen_123456


