/*
    Name of program contained in the file: Assignment6.js
    Brief description of the program: This program creates an HTTP server that allows remote access to a file system
    Inputs: None
    Output: Information to the user regarding file status
    All collaborators: N/A
    Other Sources for the Code: Stack Overflow, W3Schools, "JS Node File Server (con't)" Canvas Slides
    Author's full name: Nabeel Ahmad
    Creation date: 03/27/2024
    Replit: https://replit.com/join/qwibfiarro-nfahmad
    Testing:
            Command: curl -X GET http://localhost:8000/file.txt
            Result: File not found / Contents of file
            Reasoning: This test is good because if the file is non-existent, it will indicate that. 
                       And if the file is existent, it will output the contents of the file

            Command: curl -X DELETE http://localhost:8000/file.txt
            Result: Deletion of file / No action
            Reasoning: This test is good because if the file is non-existent, it will do nothing.
                       And if the file is existent, it will delete the file
            
            Command: curl -X PUT -d Hello http://localhost:8000/file.txt
            Result: File creation with text saying "Hello"
            Reasoning: This test is good because it tests the PUT method, and the writing aspect of the program

            Command: curl -X MKCOL http://localhost:8000/directory
            Result: Directory creation / No action
            Reasoning: This test is good because if the directory is non-existent, it will make one.
                       And if the directory is existent, it will do nothing.
            
            Command: curl -X GET http://localhost:8000/directory/file.txt
            Result: File not found / Contents of file
            Reasoning: This test is good because if the file is non-existent, it will indicate that. 
                       And if the file is existent, it will output the contents of the file.
                       This is useful for seeing if the directory handling is working properly
            
            Command: curl -X DELETE http://localhost:8000/directory/file.txt
            Result: Deletion of file / No action
            Reasoning: This test is good because if the file is non-existent, it will do nothing.
                       And if the file is existent, it will delete the file.
                       This is useful for seeing if the directory handling is working properly
            
            Command: curl -X PUT -d Hello http://localhost:8000/directory/file.txt
            Result: File creation with text saying "Hello"
            Reasoning: This test is good because it tests the PUT method, and the writing aspect of the program.
                       This is useful for seeing if the directory handling is working properly

            Command: curl -X PATCH http://localhost:8000/file.txt
            Result: "The method PATCH is not supported."
            Reasoning: This test is good because it tests an unsupported method
*/

//Import necessary modules from Node.js standard library
import {createServer} from "http";
//Import necessary module from Node.js standard library
import {parse} from "url";
//Import necessary module from Node.js standard library
import mime from "mime";
//Import necessary modules from Node.js standard library
import {resolve, sep} from "path";
//Import necessary modules from Node.js standard library
import {createReadStream, createWriteStream} from "fs";
//Import necessary modules from Node.js standard library
import {stat, readdir, mkdir, rmdir, unlink} from "fs/promises";
//Assigns methods to a null object
const methods = Object.create(null);
//Assigns baseDirectory as the current working directory
const baseDirectory = process.cwd();
/*This starts a server that just returns 405 error responses, 
which is the code used to indicate that the server refuses to handle a given method*/
createServer((request, response) => {
    //Determine the proper handler for the request method
    let handler = methods[request.method] || notAllowed;
    //Execute the handler
    handler(request)
    //Catch any potential errors
    .catch(error => {
        //If the thrown error does not have a status code, this will run
        if (error.status != null) return error;
        //The handler adds a status code to the error (500)
        return {body: String(error), status: 500};
    })
    //This will run when the request handler’s promise is resolved
    .then(({body, status = 200, type = "text/plain"}) => {
        response.writeHead(status, {"Content-Type": type});
        /*When the value of body is a readable stream it will have a 
        pipe method that is used to forward all 
        content from a readable stream to a writable stream*/
        if (body && body.pipe) body.pipe(response);
        /*If value of body is NOT a readable stream, it is assumed to be either null (no body), 
        a string, or a buffer, and it is passed directly to the response’s end method*/
        else response.end(body);
    });
//Start the server listening on port 8000
}).listen(8000);
//Creates a function to to handle requests that are not allowed
async function notAllowed(request) {
    //This will return the status of the error and a message
    return {
        //Error status 405
        status: 405,
        //Message indicating which method is not supported
        body: `The method ${request.method} is not supported.`
    };
}
//Creates a method to read a file
methods.GET = async function(request) {
    /*To figure out which file path corresponds to a request URL, 
    the urlPath function uses Node’s built-in url module to parse the URL*/
    let path = urlPath(request.url);
    //Creates the variable stats
    let stats;
    //Try-Catch block
    try {
        //Determine if it is a regular file or a directory
        stats = await stat(path);
    //When the file does not exist, stat will throw an error object
    } catch (error) {
        //The error object will have the code property of "ENOENT"
        if (error.code != "ENOENT") throw error;
        //The error status and message
        else return {status: 404, body: "File not found"};
    }
    //Determine if it is a directory
    if (stats.isDirectory()) {
        //If it is a directory, we use readdir to read the array of files in a directory and return it to the client
        return {body: (await readdir(path)).join("\n")};
    //Otherwise, if it is not a directory
    } else {
        //Create a readable stream with createReadStream and return that as the body
        return {body: createReadStream(path),
                //The mime package knows the correct type for a large number of file extensions
                type: mime.getType(path)};
    }
};
//Creates a function that extracts the file path from the request URL and validates it
function urlPath(url) {
    //Parse URL
    let {pathname} = parse(url);
    //Resolve function from the path module resolves relative paths
    let path = resolve(decodeURIComponent(pathname).slice(1));
    /*When the path doesn't start with the base directory, 
    the function throws an error response object, 
    using the HTTP status code indicating that access to the resource is forbidden*/
    if (path != baseDirectory &&
    //The sep binding from the path package is the system's path separator
    !path.startsWith(baseDirectory + sep)) {
        //The error response object being thrown
        throw {status: 403, body: "Forbidden"};
    }
    //Return the path
    return path;
}
//Creates a method to handle delete requests
methods.DELETE = async function(request) {
    //Translate the url into a file name
    let path = urlPath(request.url);
    //Invoke stat object called stats
    let stats;
    //Try-Catch block
    try {
        //Wait for stat to find the file
        stats = await stat(path);
    //Handle a non-existent file name
    } catch (error) {
        //The error object will have the code property of "ENOENT"
        if (error.code != "ENOENT") throw error;
        //Otherwise return status 204
        else return {status: 204};
    }
    //If the file name is a directory, remove it
    if (stats.isDirectory()) await rmdir(path);
    //If the file name is not a directory, remove it
    else await unlink(path);
    //Report that the file deletion was successful
    return {status: 204};
};
//This is the handler for writing a file
methods.PUT = async function(request) {
    //Translate the url into a file name
    let path = urlPath(request.url);
    //Write a wrapper, pipeStream, that creates a promise around the outcome of calling pipe
    await pipeStream(request, createWriteStream(path));
    //Return status 204
    return {status: 204};
};
//Creates a promise around the outcome of calling pipe
function pipeStream(from, to) {
    //return Promise (nothing)
    return new Promise((resolve, reject) => {
        //For when something goes wrong when opening the file
        from.on("error", reject);
        //For when the stream from the request may also fail
        to.on("error", reject);
        //When pipe is done, it will close the output stream
        to.on("finish", resolve);
        //Pipe data from stream to stream
        from.pipe(to);
    });
}
//This creates a directory by calling mkdir from the fs module
methods.MKCOL = async function (request) {
    //Translate the url into a file name
    let path = urlPath(request.url);
    //Try-Catch block
    try {
        //Create the directory at the specified path
        await mkdir(path, {recursive: true});
        //Return status 204
        return {status: 204};
    } catch (error) {
        //The error status and message
        return {status: 500, body: String(error)};
    }
};