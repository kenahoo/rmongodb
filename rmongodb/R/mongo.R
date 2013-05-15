#   Copyright (C) 2008-2011 10gen Inc.
# 
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.



##' Create an object of class "mongo"
##' 
##' Connect to a MongoDB server or replset and return an object of class
##' "mongo" used for further communication over the connection.
##' 
##' All parameters are stored as attributes of the returned mongo object. Note
##' that these attributes only reflect the initial parameters. Only the
##' external data pointed to by the "mongo" attribute actually changes if, for
##' example, mongo.timeout is called after the initial call to
##' \code{mongo.create}.
##' 
##' 
##' @param host (string vector) A list of hosts/ports to which to connect.  If
##' a port is not given, 27017 is used. Seperate ports from the IP address by
##' colon, like "120.0.0.1:12345".
##' @param name (string) The name of the replset to which to connect. If name
##' == "" (the default), the hosts are tried one by one until a connection is
##' made.  Otherwise, name must be the name of the replset and the given hosts
##' are assumed to be seeds of the replset.  Each of these is connected to and
##' queried in turn until one reports that it is a master.  This master is then
##' queried for a list of hosts and these are in turn connected to and verified
##' as belonging to the given replset name.  When one of these reports that it
##' is a master, that connection is used to form the actual connection as
##' returned.
##' @param username (string) The username to be used for authentication
##' purposes.  The default username of "" indicates that no user authentication
##' is to be performed by the initial connect.
##' @param password (string) The password corresponding to the given username.
##' @param db (string) The name of the database upon which to authenticate the
##' given username and password.  If authentication fails, the connection is
##' disconnected, but mongo.get.err() will indicate not indicate an error.
##' @param timeout (as.integer) The number of milliseconds to wait before
##' timing out of a network operation.  The default (0L) indicates no timeout.
##' @return If successful, a mongo object for use in subsequent database
##' operations; otherwise, mongo.get.err() may be called on the returned mongo
##' object to see why it failed.
##' @seealso \link{mongo},\cr \code{\link{mongo.is.connected}},\cr
##' \code{\link{mongo.disconnect}},\cr \code{\link{mongo.reconnect}},\cr
##' \code{\link{mongo.get.err}},\cr \code{\link{mongo.get.primary}},\cr
##' \code{\link{mongo.get.hosts}},\cr \code{\link{mongo.get.socket}},\cr
##' \code{\link{mongo.set.timeout}},\cr \code{\link{mongo.get.timeout}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' \dontrun{
##'     mongo <- mongo.create("192.168.0.3")}
##' 
mongo.create <- function(host="127.0.0.1", name="", username="", password="", db="admin", timeout=0L) {
    mongo <- .Call(".mongo.create")
    attr(mongo, "host") <- host
    attr(mongo, "name") <- name
    attr(mongo, "username") <- username
    attr(mongo, "password") <- password
    attr(mongo, "db") <- db
    attr(mongo, "timeout") <- timeout
    .Call(".mongo.connect", mongo)
}


##' Retrieve an connection error code from a mongo object
##' 
##' Retrieve an connection error code from a mongo object indicating the
##' failure code if mongo.create() failed.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return (integer) error code as follows:
##' 
##' mongo.create() errors:
##' 
##' Other errors:
##' @returnItem 0 No Error
##' @returnItem 1 No socket - Could not create socket.
##' @returnItem 2 Fail - An error occurred attempting to connect to socket
##' @returnItem 3 Address fail - An error occured calling getaddrinfo().
##' @returnItem 4 Not Master - Warning: connected to a non-master node
##' (read-only).
##' @returnItem 5 Bad set name - given name doesn't match the replica set.
##' @returnItem 6 No Primary - Cannot find primary in replica set - connection
##' closed.
##' @returnItem 7 I/O error - An error occured reading or writing on the
##' socket.
##' @returnItem 8 Read size error - The response is not the expected length.
##' @returnItem 9 Command failed - The command returned with 'ok' value of 0.
##' @returnItem 10 BSON invalid - Not valid for the specified operation.
##' @returnItem 11 BSON not finished - should not occur with R driver.
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (!mongo.is.connected(mongo)) {
##'     print("Unable to connect.  Error code:")
##'     print(mongo.get.err(mongo))
##' }
##' 
mongo.get.err <- function(mongo)
    .Call(".mongo.get.err", mongo)



##' Disconnect from a MongoDB server
##' 
##' Disconnect from a MongoDB server.  No further communication is possible on
##' the connection. However, \code{\link{mongo.reconnect}()} may be called on
##' the mongo object to restablish the connection.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return The mongo object is returned.
##' @seealso \link{mongo},\cr \code{\link{mongo.create}},\cr
##' \code{\link{mongo.reconnect}},\cr \code{\link{mongo.is.connected}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     n_people <- mongo.count(mongo, "test.people")
##'     mongo.disconnect(mongo)
##' }
##' 
mongo.disconnect <- function(mongo)
    .Call(".mongo.disconnect", mongo)



##' Reconnect to a MongoDB server
##' 
##' Reconnect to a MongoDB server. Calls mongo.disconnect and then attempts to
##' re-establish the connection.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @seealso \code{\link{mongo.create}},\cr \code{\link{mongo.disconnect}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo))
##'     mongo.reconnect(mongo)
##' 
mongo.reconnect <- function(mongo)
    .Call(".mongo.reconnect", mongo)



##' Destroy a MongoDB connection
##' 
##' Destroy a \link{mongo} connection.  The connection is disconnected first if
##' it is still connected. No further communication is possible on the
##' connection.  Releases resources attached to the connection on both client
##' and server.
##' 
##' Although the 'destroy' functions in this package are called automatically
##' by garbage collection, this one in particular should be called as soon as
##' feasible when finished with the connection so that server resources are
##' freed.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return NULL
##' @seealso \link{mongo},\cr \code{\link{mongo.disconnect}},\cr
##' \code{\link{mongo.is.connected}}\cr \code{\link{mongo.reconnect}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     n_people <- mongo.count(mongo, "test.people")
##'     mongo.destroy(mongo)
##'     print(n_people)
##' }
##' 
mongo.destroy <- function(mongo)
    .Call(".mongo.destroy", mongo)



##' Determine if a mongo object is connected to a MongoDB server
##' 
##' Returns TRUE if the parameter mongo object is connected to a MongoDB
##' server; otherwise, FALSE.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return Logical TRUE if the mongo connection object is currently connected
##' to a server; otherwise, FALSE.
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.count(mongo, "test.people"))
##' }
##' 
mongo.is.connected <- function(mongo)
    .Call(".mongo.is.connected", mongo)



##' Get the socket assigned to a mongo object by mongo.create().
##' 
##' Get the the low-level socket number assigned to the given mongo object by
##' mongo.create().
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return Integer socket number
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo))
##'     print(mongo.get.socket(mongo))
##' 
mongo.get.socket <- function(mongo)
    .Call(".mongo.get.socket", mongo)



##' Get the host & port of the server to which a mongo object is connected.
##' 
##' Get the host & port of the server to which a mongo object is connected.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return String host & port in the format "\%s:\%d".
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' \dontrun{
##' mongo <- mongo.create(c("127.0.0.1", "192.168.0.3"))
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.get.primary(mongo))
##' }
##' }
##' 
mongo.get.primary <- function(mongo)
    .Call(".mongo.get.primary", mongo)



##' Get a lists of hosts & ports as reported by a replica set master upon
##' connection creation.
##' 
##' Get a lists of hosts & ports as reported by a replica set master upon
##' connection creation.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return NULL if a replica set was not connected to; otherwise, a list of
##' host & port strings in the format "%s:%d".
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}
##' @export
##' @examples
##' 
##' \dontrun{
##' mongo <- mongo.create(c("127.0.0.1", "192.168.0.3"), name="Inventory")
##' if (mongo.is.connected(mongo))
##'     print(mongo.get.hosts(mongo))
##' }
##' 
mongo.get.hosts <- function(mongo)
    .Call(".mongo.get.hosts", mongo)



##' Set the timeout value on a mongo connection
##' 
##' Set the timeout value for network operations on a mongo connection.
##' Subsequent network operations will timeout if they take longer than the
##' given number of milliseconds.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param timeout (as.integer) number of milliseconds to which to set the
##' timeout value.
##' @seealso \code{\link{mongo.get.timeout}},\cr \code{\link{mongo.create}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     mongo.set.timeout(mongo, 2000L)
##'     timeout <- mongo.get.timeout(mongo)
##'     if (timeout != 2000L)
##'         error("expected timeout of 2000");
##' }
##' 
mongo.set.timeout <- function(mongo, timeout)
    .Call(".mongo.set.timeout", mongo, timeout)



##' Get the timeout value of a mongo connection
##' 
##' Get the timeout value for network operations on a mongo connection.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return (integer) timeout value in milliseconds.
##' @seealso \code{\link{mongo.set.timeout}},\cr \code{\link{mongo.create}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     mongo.set.timeout(mongo, 2000L)
##'     timeout <- mongo.get.timeout(mongo)
##'     if (timeout != 2000L)
##'         error("expected timeout of 2000");
##' }
##' 
mongo.get.timeout <- function(mongo)
    .Call(".mongo.get.timeout", mongo)



##' Determine if a mongo connection object is connected to a master
##' 
##' Determine if a mongo connection object is connected to a master.  Normally,
##' this is only used with replsets to see if we are currently connected to the
##' master of the replset. However, when connected to a singleton, this
##' function reports TRUE also.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return (logical) TRUE if the server reports that it is a master;
##' otherwise, FALSE.
##' @seealso \code{\link{mongo.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' \dontrun{
##' mongo <- mongo.create(c("127.0.0.1", "192.168.0.3"), name="Accounts")
##' if (mongo.is.connected(mongo)) {
##'     print("isMaster")
##'     print(if (mongo.is.master(mongo)) "Yes" else "No")
##' }
##' }
##' 
mongo.is.master <- function(mongo)
    .Call(".mongo.is.master", mongo)



##' Autherticate a user and password
##' 
##' Autherticate a user and password against a given database on a MongoDB
##' server.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Security+and+Authentication}.
##' 
##' Note that \code{\link{mongo.create}()} can authenticate a username and
##' password before returning a connected mongo object.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param username (string) username to authenticate.
##' @param password (string) password corresponding to username.
##' @param db (string) The database on the server against which to validate the
##' username and password.
##' @seealso \code{\link{mongo.add.user}},\cr \link{mongo},\cr
##' \code{\link{mongo.create}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo))
##'     mongo.authenticate(mongo, "Joe", "ZxYaBc217")
##' 
mongo.authenticate <- function(mongo, username, password, db="admin")
    .Call(".mongo.authenticate", mongo, username, password, db)



##' Add a user and password
##' 
##' Add a user and password to the given database on a MongoDB server for
##' authentication purposes.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Security+and+Authentication}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param username (string) username to add.
##' @param password (string) password corresponding to username.
##' @param db (string) The database on the server to which to add the username
##' and password.
##' @seealso \code{\link{mongo.authenticate}},\cr \link{mongo},\cr
##' \code{\link{mongo.create}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo))
##'     mongo.add.user(mongo, "Jeff", "H87b5dog")
##' 
mongo.add.user <- function(mongo, username, password, db="admin")
    .Call(".mongo.add.user", mongo, username, password, db)



##' Retrieve an server error code from a mongo connection object
##' 
##' Retrieve an server error record from a the MongoDB server.  This describes
##' the last error that occurs while accessing the give database. While this
##' function retrieves an error record in the form of a mongo.bson record, it
##' also sets the values returned by \code{\link{mongo.get.server.err}()} and
##' \code{\link{mongo.get.server.err.string}()}. You may find it more
##' convenient using those after calling \code{mongo.get.last.err()} rather
##' than unpacking the returned mongo.bson object.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param db (string) The name of the database for which to get the error
##' status.
##' @return NULL if no error was reported; otherwise,
##' 
##' (\link{mongo.bson}) This BSON object has the form { err : "\emph{error
##' message string}", code : \emph{error code integer} }
##' @seealso \code{\link{mongo.get.server.err}},\cr
##' \code{\link{mongo.get.server.err.string}},\cr
##' \code{\link{mongo.get.prev.err}}\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##' 
##'     # try adding a duplicate record when index doesn't allow this
##' 
##'     db <- "test"
##'     ns <- "test.people"
##'     mongo.index.create(mongo, ns, "name", mongo.index.unique)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 22L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 27L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     err <- mongo.get.last.err(mongo, db)
##'     print(mongo.get.server.err(mongo))
##'     print(mongo.get.server.err.string(mongo))
##' }
##' 
mongo.get.last.err <- function(mongo, db)
    .Call(".mongo.get.last.err", mongo, db)



##' Retrieve an server error code from a mongo connection object
##' 
##' Retrieve the previous server error record from a the MongoDB server.  While
##' this function retrieves an error record in the form of a mongo.bson record,
##' it also sets the values returned by \code{\link{mongo.get.server.err}()}
##' and \code{\link{mongo.get.server.err.string}()}. You may find it more
##' convenient using those after calling \code{mongo.get.prev.err()} rather
##' than unpacking the returned mongo.bson object.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param db (string) The name of the database for which to get the error
##' status.
##' @return NULL if no error was reported; otherwise,
##' 
##' (\link{mongo.bson}) This BSON object has the form { err : "\emph{error
##' message string}", code : \emph{error code integer} }
##' @seealso \code{\link{mongo.get.server.err}},\cr
##' \code{\link{mongo.get.server.err.string}},\cr
##' \code{\link{mongo.get.last.err}}\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##' 
##'     # try adding a duplicate record when index doesn't allow this
##' 
##'     db <- "test"
##'     ns <- "test.people"
##'     mongo.index.create(mongo, ns, "name", mongo.index.unique)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 22L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 27L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     # try insert again
##'     mongo.insert(mongo, ns, b);
##' 
##'     err <- mongo.get.prev.err(mongo, db)
##'     print(mongo.get.server.err(mongo))
##'     print(mongo.get.server.err.string(mongo))
##' }
##' 
mongo.get.prev.err <- function(mongo, db)
    .Call(".mongo.get.prev.err", mongo, db)



##' Retrieve an server error code from a mongo connection object
##' 
##' Send a "reset error" command to the server, it also resets the values
##' returned by\cr \code{\link{mongo.get.server.err}()} and
##' \code{\link{mongo.get.server.err.string}()}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param db (string) The name of the database on which to reset the error
##' status.
##' @return NULL
##' @seealso \code{\link{mongo.get.server.err}},\cr
##' \code{\link{mongo.get.server.err.string}},\cr
##' \code{\link{mongo.get.last.err}},\cr \code{\link{mongo.get.prev.err}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##' 
##'     # try adding a duplicate record when index doesn't allow this
##' 
##'     db <- "test"
##'     ns <- "test.people"
##'     mongo.index.create(mongo, ns, "name", mongo.index.unique)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 22L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "John")
##'     mongo.bson.buffer.append(buf, "age", 27L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b);
##' 
##'     err <- mongo.get.last.err(mongo, db)
##'     print(mongo.get.server.err(mongo))
##'     print(mongo.get.server.err.string(mongo))
##'     mongo.reset.err(mongo, db)
##' }
##' 
mongo.reset.err <- function(mongo, db)
    .Call(".mongo.reset.err", mongo, db)



##' Retrieve an server error code from a mongo connection object
##' 
##' Retrieve an server error code from a mongo connection object.
##' 
##' \code{\link{mongo.find}()}, \code{\link{mongo.find.one}()},
##' \code{\link{mongo.index.create}()} set or clear this error code depending
##' on whether they are successful or not.
##' 
##' \code{\link{mongo.get.last.err}()} and \code{\link{mongo.get.prev.err}()}
##' both set or clear this error code according to what the server reports.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return (integer) Server error code
##' @seealso \code{\link{mongo.get.server.err.string}},\cr
##' \code{\link{mongo.get.last.err}},\cr \code{\link{mongo.get.prev.err}},\cr
##' \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.index.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     # construct a query containing invalid operator
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.start.object(buf, "age")
##'     mongo.bson.buffer.append(buf, "$bad", 1L)
##'     mongo.bson.buffer.finish.object(buf)
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     result <- mongo.find.one(mongo, "test.people", query)
##'     if (is.null(result)) {
##'         print(mongo.get.server.err.string(mongo))
##'         print(mongo.get.server.err(mongo))
##'     }
##' }
##' 
mongo.get.server.err <- function(mongo)
    .Call(".mongo.get.server.err", mongo)



##' Retrieve an server error code from a mongo connection object
##' 
##' Retrieve an server error string from a mongo connection object.
##' 
##' \code{\link{mongo.find}()}, \code{\link{mongo.find.one}()},
##' \code{\link{mongo.index.create}()} set or clear this error string depending
##' on whether they are successful or not.
##' 
##' \code{\link{mongo.get.last.err}()} and \code{\link{mongo.get.prev.err}()}
##' both set or clear this error string according to what the server reports.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @return (string) Server error string
##' @seealso \code{\link{mongo.get.server.err}},\cr
##' \code{\link{mongo.get.last.err}},\cr \code{\link{mongo.get.prev.err}},\cr
##' \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.index.create}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     # construct a query containing invalid operator
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.start.object(buf, "age")
##'     mongo.bson.buffer.append(buf, "$bad", 1L)
##'     mongo.bson.buffer.finish.object(buf)
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     result <- mongo.find.one(mongo, "test.people", query)
##'     if (is.null(result)) {
##'         print(mongo.get.server.err(mongo))
##'         print(mongo.get.server.err.string(mongo))
##'     }
##' }
##' 
mongo.get.server.err.string <- function(mongo)
    .Call(".mongo.get.server.err.string", mongo)



##' Add record to a collection
##' 
##' Add record to a collection.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Inserting}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param ns (string) namespace of the collection to which to add the record.
##' @param b (\link{mongo.bson}) The record to add.
##' 
##' In addition, \code{b} may be a list which will be converted to a mongo.bson
##' object by \code{\link{mongo.bson.from.list}()}.
##' @return TRUE if the command was successfully sent to the server; otherwise,
##' FALSE.
##' 
##' \code{\link{mongo.get.last.err}()} may be examined to verify that the
##' insert was successful on the server if necessary.
##' @seealso \code{\link{mongo.insert.batch}},\cr
##' \code{\link{mongo.update}},\cr \code{\link{mongo.find}},\cr
##' \code{\link{mongo.find.one}},\cr \code{\link{mongo.remove}},\cr
##' \link{mongo.bson},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     ns <- "test.people"
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Joe")
##'     mongo.bson.buffer.append(buf, "age", 22L)
##'     b <- mongo.bson.from.buffer(buf)
##'     mongo.insert(mongo, ns, b)
##' 
##'     # do the same thing in shorthand:
##'     mongo.insert(mongo, ns, list(name="Joe", age=22L))
##' }
##' 
mongo.insert <- function(mongo, ns, b) {
    if (typeof(b) == "list")
        b <- mongo.bson.from.list(b)
    .Call(".mongo.insert", mongo, ns, b)
}



##' Add multiple records to a collection
##' 
##' Add multiple records to a collection.  This function eliminates some
##' network traffic and server overhead by sending all the records in a single
##' message.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Inserting}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param ns (string) namespace of the collection to which to add the record.
##' @param lst A list of (\link{mongo.bson}) records to add.
##' @return TRUE if the command was successfully sent to the server; otherwise,
##' FALSE.
##' 
##' \code{\link{mongo.get.last.err}()} may be examined to verify that the
##' insert was successful on the server if necessary.
##' @seealso \code{\link{mongo.insert}},\cr \code{\link{mongo.update}},\cr
##' \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.remove}},\cr \link{mongo.bson},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     ns <- "test.people"
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Dave")
##'     mongo.bson.buffer.append(buf, "age", 27L)
##'     x <- mongo.bson.from.buffer(buf)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Fred")
##'     mongo.bson.buffer.append(buf, "age", 31L)
##'     y <- mongo.bson.from.buffer(buf)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Silvia")
##'     mongo.bson.buffer.append(buf, "city", 24L)
##'     z <- mongo.bson.from.buffer(buf)
##'     mongo.insert.batch(mongo, ns, list(x, y, z))
##' }
##' 
mongo.insert.batch <- function(mongo, ns, lst)
    .Call(".mongo.insert.batch", mongo, ns, lst)




##' mongo.update() flag constant for an upsert
##' 
##' Flag to \code{\link{mongo.update}()} (1L): insert ObjNew into the database
##' if no record matching criteria is found.
##' 
##' 
##' @return 1L
##' @seealso \code{\link{mongo.update}},\cr
##' \code{\link{mongo.update.multi}},\cr \code{\link{mongo.update.basic}}.
mongo.update.upsert <- 1L


##' mongo.update() flag constant for updating multiple records
##' 
##' Flag to \code{\link{mongo.update}()} (2L): Update multiple records rather
##' than just the first one matched by criteria.
##' 
##' 
##' @return 2L
##' @seealso \code{\link{mongo.update}},\cr
##' \code{\link{mongo.update.upsert}},\cr \code{\link{mongo.update.basic}}.
mongo.update.multi  <- 2L


##' mongo.update() flag constant for performing a basic update
##' 
##' Flag to \code{\link{mongo.update}()} (4L): Perform a basic update.
##' 
##' 
##' @return 4L
##' @seealso \code{\link{mongo.update}},\cr \code{\link{mongo.update.multi}}\cr
##' \code{\link{mongo.update.upsert}}
mongo.update.basic  <- 4L



##' Perform an update on a collection
##' 
##' Perform an update on a collection.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Updating}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param ns (string) namespace of the collection to which to update.
##' @param criteria (\link{mongo.bson}) The criteria with which to match
##' records that are to be updated.
##' 
##' Alternately, \code{criteria} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param objNew (\link{mongo.bson}) The replacement object.
##' 
##' Alternately, \code{objNew} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param flags (integer vector) A list of optional flags governing the
##' operation: \itemize{ \item\code{\link{mongo.update.upsert}}: insert ObjNew
##' into the database if no record matching criteria is found.
##' \item\code{\link{mongo.update.multi}}: update multiple records rather than
##' just the first one matched by criteria.
##' \item\code{\link{mongo.update.basic}}: Perform a basic update.  }
##' @seealso \link{mongo},\cr \link{mongo.bson},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.find}},\cr
##' \code{\link{mongo.find.one}},\cr \code{\link{mongo.remove}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     ns <- "test.people"
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Joe")
##'     criteria <- mongo.bson.from.buffer(buf)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.start.object(buf, "$inc")
##'     mongo.bson.buffer.append(buf, "age", 1L)
##'     mongo.bson.buffer.finish.object(buf)
##'     objNew <- mongo.bson.from.buffer(buf)
##' 
##'     # increment the age field of the first record matching name "Joe"
##'     mongo.update(mongo, ns, criteria, objNew)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Jeff")
##'     criteria <- mongo.bson.from.buffer(buf)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Jeff")
##'     mongo.bson.buffer.append(buf, "age", 27L)
##'     objNew <- mongo.bson.from.buffer(buf)
##' 
##'     # update the entire record to { name: "Jeff", age: 27 }
##'     # where name equals "Jeff"
##'     # if such a record exists; otherwise, insert this as a new reord
##'     mongo.update(mongo, ns, criteria, objNew,
##'         mongo.update.upsert)
##' 
##'     # do a shorthand update:
##'     mongo.update(mongo, ns, list(name="John"), list(name="John", age=25))
##' }
##' 
mongo.update <- function(mongo, ns, criteria, objNew, flags=0L) {
    if (typeof(criteria) == "list")
        criteria <- mongo.bson.from.list(criteria)
    if (typeof(objNew) == "list")
        objNew <- mongo.bson.from.list(objNew)
    .Call(".mongo.update", mongo, ns, criteria, objNew, flags)
}



##' Remove records from a collection
##' 
##' Remove all records from a collection that match a given criteria.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Removing}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param ns (string) namespace of the collection from which to remove
##' records.
##' @param criteria (\link{mongo.bson}) The criteria with which to match
##' records that are to be removed. The default of mongo.bson.empty() will
##' cause \emph{all} records in the given collection to be removed.
##' 
##' Alternately, \code{criteria} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @seealso \link{mongo},\cr \link{mongo.bson},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.update}},\cr
##' \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Jeff")
##'     criteria <- mongo.bson.from.buffer(buf)
##' 
##'     # remove all records where name is "Jeff"
##'     # from collection people in database test
##'     mongo.remove(mongo, "test.people", criteria)
##' 
##'     # remove all records from collection cars in database test
##'     mongo.remove(mongo, "test.cars")
##' 
##'     # shorthand: remove all records where name is "Fred"
##'     mongo.remove(mongo, "test.people", list(name="Fred"))
##' }
##' 
mongo.remove <- function(mongo, ns, criteria=mongo.bson.empty()) {
    if (typeof(criteria) == "list")
        criteria <- mongo.bson.from.list(criteria)
    .Call(".mongo.remove", mongo, ns, criteria)
}



##' Find one record in a collection
##' 
##' Find the first record in a collection that matches a given query.
##' 
##' This is a simplified version of mongo.find() which eliminates the need to
##' step through returned records with a cursor.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Querying}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param ns (string) The namespace of the collection from in which to find a
##' record.
##' @param query (\link{mongo.bson}) The criteria with which to match the
##' record that is to be found. The default of mongo.bson.empty() will cause
##' the the very first record in the collection to be returned.
##' 
##' Alternately, \code{query} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param fields (\link{mongo.bson}) The desired fields which are to be
##' returned frtom the matching record.  The default of mongo.bson.empty() will
##' cause all fields of the matching record to be returned; however, specific
##' fields may be specified to cut down on network traffic and memory overhead.
##' 
##' Alternately, \code{fields} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @return NULL if no record matching the criteria is found; otherwise,
##' 
##' (\link{mongo.bson}) The matching record/fields.
##' 
##' Note that NULL may also be returned if a database error occurred (when a
##' badly formed query is used, for example).
##' \code{\link{mongo.get.server.err}} and
##' \code{\link{mongo.get.server.err.string}} may be examined in that case.
##' @seealso \code{\link{mongo.find}},\cr \code{\link{mongo.index.create}},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.update}},\cr
##' \code{\link{mongo.remove}},\cr \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "name", "Jeff")
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # find the first record where name is "Jeff"\
##'     #    in collection people of database test
##'     b <- mongo.find.one(mongo, "test.people", query)
##'     if (!is.null(b))
##'         print(b)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "_id", 1L)
##'     mongo.bson.buffer.append(buf, "age", 1L)
##'     fields <- mongo.bson.from.buffer(buf)
##' 
##'     # find the first record where name is "Jeff"
##'     #    in collection people of database test
##'     # return only the _id and age fields of the matched record
##'     b <- mongo.find.one(mongo, "test.people", query, fields)
##'     if (!is.null(b))
##'         print(b)
##' 
##'     # find the first record in collection cars of database test
##'     have.car <- !is.null(mongo.find.one(mongo, "test.cars"))
##' 
##'     # shorthand using a list:
##'     b <- mongo.find.one(mongo, "test.people", list(name="Jose"))
##' }
##' 
mongo.find.one <- function(mongo, ns, query=mongo.bson.empty(), fields=mongo.bson.empty()) {
    if (typeof(query) == "list")
        query <- mongo.bson.from.list(query)
    if (typeof(fields) == "list")
        fields <- mongo.bson.from.list(fields)
    .Call(".mongo.find.one", mongo, ns, query, fields)
}



##' mongo.find flag constant - cursor tailable
##' 
##' \code{\link{mongo.find}()} flag constant - cursor tailable.
##' 
##' 
##' @return 2L
mongo.find.cursor.tailable   <- 2L


##' mongo.find flag constant - slave ok
##' 
##' \code{\link{mongo.find}()} flag constant - slave ok.
##' 
##' 
##' @return 4L
mongo.find.slave.ok          <- 4L


##' mongo.find flag constant - oplog replay
##' 
##' \code{\link{mongo.find}()} flag constant - oplog replay.
##' 
##' 
##' @return 8L
mongo.find.oplog.replay      <- 8L


##' mongo.find flag constant - no cursor timeout
##' 
##' \code{\link{mongo.find}()} flag constant - no cursor timeout.
##' 
##' 
##' @return 16L
mongo.find.no.cursor.timeout <- 16L


##' mongo.find flag constant - await data
##' 
##' \code{\link{mongo.find}()} flag constant - await data.
##' 
##' 
##' @return 32L
mongo.find.await.data        <- 32L


##' mongo.find flag constant - exhaust
##' 
##' \code{\link{mongo.find}()} flag constant - exhaust.
##' 
##' 
##' @return 64L
mongo.find.exhaust           <- 64L


##' mongo.find flag constant - partial results
##' 
##' \code{\link{mongo.find}()} flag constant - partial results.
##' 
##' 
##' @return 128L
mongo.find.partial.results   <- 128L



##' Find records in a collection
##' 
##' Find records in a collection that match a given query.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Querying}.
##' 
##' 
##' @param mongo (\link{mongo}) a mongo connection object.
##' @param ns (string) namespace of the collection from which to find records.
##' @param query (\link{mongo.bson}) The criteria with which to match the
##' records to be found.  The default of mongo.bson.empty() will cause the the
##' very first record in the collection to be returned.
##' 
##' Alternately, \code{query} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param sort (\link{mongo.bson}) The desired fields by which to sort the
##' returned records. The default of mongo.bson.empty() indicates that no
##' special sorting is to be done; the records will come back in the order that
##' indexes locate them.
##' 
##' Alternately, \code{sort} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param fields (\link{mongo.bson}) The desired fields which are to be
##' returned from the matching record.  The default of mongo.bson.empty() will
##' cause all fields of the matching record to be returned; however, specific
##' fields may be specified to cut down on network traffic and memory overhead.
##' 
##' Alternately, \code{fields} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param limit (as.integer) The maximum number of records to be returned. A
##' limit of 0L will return all matching records not skipped.
##' @param skip (as.integer) The number of matching records to skip before
##' returning subsequent matching records.
##' @param options (integer vector) Flags governing the requested operation as
##' follows: \itemize{ \item\link{mongo.find.cursor.tailable}
##' \item\link{mongo.find.slave.ok} \item\link{mongo.find.oplog.replay}
##' \item\link{mongo.find.no.cursor.timeout} \item\link{mongo.find.await.data}
##' \item\link{mongo.find.exhaust} \item\link{mongo.find.partial.results} }
##' @return (\link{mongo.cursor}) An object of class "mongo.cursor" which is
##' used to step through the matching records.
##' 
##' Note that an empty cursor will be returned if a database error occurred.\cr
##' \code{\link{mongo.get.server.err}()} and
##' \code{\link{mongo.get.server.err.string}()} may be examined in that case.
##' @seealso \code{\link{mongo.cursor}},\cr \code{\link{mongo.cursor.next}},\cr
##' \code{\link{mongo.cursor.value}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.index.create}},\cr
##' \code{\link{mongo.update}},\cr \code{\link{mongo.remove}},\cr
##' \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "age", 18L)
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Find the first 100 records
##'     #    in collection people of database test where age == 18
##'     cursor <- mongo.find(mongo, "test.people", query, limit=100L)
##'     # Step though the matching records and display them
##'     while (mongo.cursor.next(cursor))
##'         print(mongo.cursor.value(cursor))
##'     mongo.cursor.destroy(cursor)
##' 
##' 
##'     # shorthand: find all records where age=32, sorted by name, 
##'     # and only return the name & address fields:
##'     cursor <- mongo.find(mongo, "test.people", list(age=32),
##'                          list(name=1L), list(name=1L, address=1L))
##' }
##' 
mongo.find <- function(mongo, ns, query=mongo.bson.empty(), sort=mongo.bson.empty(), fields=mongo.bson.empty(), limit=0L, skip=0L, options=0L) {
    if (typeof(query) == "list")
        query <- mongo.bson.from.list(query)
    if (typeof(sort) == "list")
        sort <- mongo.bson.from.list(sort)
    if (typeof(fields) == "list")
        fields <- mongo.bson.from.list(fields)
    .Call(".mongo.find", mongo, ns, query, sort, fields, limit, skip, options)
}



##' Advance a cursor to the next record
##' 
##' \code{\link{mongo.cursor.next}(cursor)} is used to step to the first or
##' next record.
##' 
##' \code{\link{mongo.cursor.value}(cursor)} may then be used to examine it.
##' 
##' 
##' @param cursor (\link{mongo.cursor}) A mongo.cursor object returned from
##' \code{\link{mongo.find}()}.
##' @return TRUE if there is a next record; otherwise, FALSE.
##' @seealso \code{\link{mongo.find}},\cr \link{mongo.cursor},\cr
##' \code{\link{mongo.cursor.value}},\cr \code{\link{mongo.cursor.destroy}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "city", "St. Louis")
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Find the first 1000 records in collection people
##'     # of database test where city == "St. Louis"
##'     cursor <- mongo.find(mongo, "test.people", query, limit=1000L)
##'     # Step though the matching records and display them
##'     while (mongo.cursor.next(cursor))
##'         print(mongo.cursor.value(cursor))
##'     mongo.cursor.destroy(cursor)
##' }
##' 
mongo.cursor.next <- function(cursor)
    .Call(".mongo.cursor.next", cursor)



##' Fetch the current value of a cursor
##' 
##' \code{\link{mongo.cursor.value}(cursor)} is used to fetch the current
##' record belonging to a\cr \code{\link{mongo.find}()} query.
##' 
##' 
##' @param cursor (\link{mongo.cursor}) A mongo.cursor object returned from
##' \code{\link{mongo.find}()}.
##' @return (\link{mongo.bson}) The current record of the result set.
##' @seealso \code{\link{mongo.find}},\cr \code{\link{mongo.cursor}},\cr
##' \code{\link{mongo.cursor.next}},\cr \code{\link{mongo.cursor.value}},\cr
##' \code{\link{mongo.cursor.destroy}},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "city", "St. Louis")
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Find the first 1000 records in collection people
##'     # of database test where city == "St. Louis"
##'     cursor <- mongo.find(mongo, "test.people", query, limit=1000L)
##'     # Step though the matching records and display them
##'     while (mongo.cursor.next(cursor))
##'         print(mongo.cursor.value(cursor))
##'     mongo.cursor.destroy(cursor)
##' }
##' 
mongo.cursor.value <- function(cursor)
    .Call(".mongo.cursor.value", cursor)



##' Release resources attached to a cursor
##' 
##' \code{mongo.cursor.destroy(cursor)} is used to release resources attached
##' to a cursor on both the client and server.
##' 
##' Note that \code{mongo.cursor.destroy(cursor)} may be called before all
##' records of a result set are iterated through (for example, if a desired
##' record is located in the result set).
##' 
##' Although the 'destroy' functions in this package are called automatically
##' by garbage collection, this one in particular should be called as soon as
##' feasible when finished with the cursor so that server resources are freed.
##' 
##' 
##' @param cursor (\link{mongo.cursor}) A mongo.cursor object returned from
##' \code{\link{mongo.find}()}.
##' @return TRUE if successful; otherwise, FALSE (when an error occurs during
##' sending the Kill Cursor operation to the server). in either case, the
##' cursor should not be used for further operations.
##' @seealso \code{\link{mongo.find}},\cr \link{mongo.cursor},\cr
##' \code{\link{mongo.cursor.next}},\cr \code{\link{mongo.cursor.value}}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "city", "St. Louis")
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Find the first 1000 records in collection people
##'     # of database test where city == "St. Louis"
##'     cursor <- mongo.find(mongo, "test.people", query, limit=1000L)
##'     # Step though the matching records and display them
##'     while (mongo.cursor.next(cursor))
##'         print(mongo.cursor.destroy(cursor))
##'     mongo.cursor.destroy(cursor)
##' }
##' 
mongo.cursor.destroy <- function(cursor)
    .Call(".mongo.cursor.destroy", cursor)




##' mongo.index.create flag constant - unique keys
##' 
##' \code{\link{mongo.index.create}()} flag constant - unique keys (no
##' duplicates).
##' 
##' 
##' @return 1L
mongo.index.unique     <- 1L


##' mongo.index.create flag constant - drop duplicate keys
##' 
##' \code{\link{mongo.index.create}()} flag constant - drop duplicate keys.
##' 
##' 
##' @return 4L
mongo.index.drop.dups  <- 4L


##' mongo.index.create flag constant - background
##' 
##' \code{\link{mongo.index.create}()} flag constant - background.
##' 
##' 
##' @return 8L
mongo.index.background <- 8L


##' mongo.index.create flag constant - sparse
##' 
##' \code{\link{mongo.index.create}()} flag constant - sparse.
##' 
##' 
##' @return 16L
mongo.index.sparse     <- 16L



##' Add an index to a collection
##' 
##' Add an index to a collection.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/Indexes}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param ns (string) The namespace of the collection to which to add an
##' index.
##' @param key An object enumerating the fields in order which are to
##' participate in the index. This object may be a vector of strings listing
##' the key fields or a \link{mongo.bson} object containing the key fields in
##' the desired order.
##' 
##' Alternately, \code{key} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @param options (integer vector) Optional flags governing the operation:
##' \itemize{ \item\code{\link{mongo.index.unique}}
##' \item\code{\link{mongo.index.drop.dups}}
##' \item\code{\link{mongo.index.background}}
##' \item\code{\link{mongo.index.sparse}} }
##' @return NULL if successful; otherwise, a \link{mongo.bson} object
##' describing the error.\cr \code{\link{mongo.get.server.err}()} or
##' \code{\link{mongo.get.server.err.string}()} may alternately be called in
##' this case instead of examining the returned object.
##' @seealso \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.update}},\cr
##' \code{\link{mongo.remove}},\cr \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     # Add a city index to collection people in database test
##'     b <- mongo.index.create(mongo, "test.people", "city")
##'     if (!is.null(b)) {
##'         print(b)
##'         stop("Server error")
##'     }
##' 
##'     # Add an index to collection people in database test
##'     # which will speed up queries of age followed by name
##'     b <- mongo.index.create(mongo, "test.people", c("age", "name"))
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "age", 1L)
##'     mongo.bson.buffer.append(buf, "name", 1L)
##'     key <- mongo.bson.from.buffer(buf)
##' 
##'     # add an index using an alternate method of specifying the key fields
##'     b <- mongo.index.create(mongo, "test.people", key)
##' 
##'     # create an index using list of that enumerates the key fields
##'     b <- mongo.index.create(mongo, "test.cars", list(make=1L, model=1L))
##' }
##' 
mongo.index.create <- function(mongo, ns, key, options=0L) {
    if (typeof(key) == "list")
        key <- mongo.bson.from.list(key)
    .Call(".mongo.index.create", mongo, ns, key, options)
}



##' Count records in a collection
##' 
##' Count the number of records in a collection that match a query See
##' \url{http://www.mongodb.org/display/DOCS/Indexes}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param ns (string) The namespace of the collection in which to add count
##' records.
##' @param query \link{mongo.bson} The criteria with which to match records
##' that are to be counted.  The default of mongo.bson.empty() matches all
##' records in the collection.
##' 
##' Alternately, \code{query} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @return (double) The number of matching records.
##' @seealso \code{\link{mongo.find}},\cr \code{\link{mongo.find.one}},\cr
##' \code{\link{mongo.insert}},\cr \code{\link{mongo.update}},\cr
##' \code{\link{mongo.remove}},\cr \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     # Count the number of records in collection people of database test
##'     people.count <- mongo.count(mongo, "test.people")
##'     print("total people")
##'     print(people.count)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "age", 21L)
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Count the number of records in collection people of database test
##'     # where age == 21
##'     just.legal.count <- mongo.count(mongo, "test.people", query)
##'     print("people of age 21")
##'     print(just.legal.count)
##' 
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.start.object(buf, "age")
##'     mongo.bson.buffer.append(buf, "$gte", 21L)
##'     mongo.bson.buffer.finish.object(buf)
##'     query <- mongo.bson.from.buffer(buf)
##' 
##'     # Count the number of records in collection people of database test
##'     # where age >= 21
##'     total.legal.count <- mongo.count(mongo, "test.people", query)
##'     print("people of age 21 or greater")
##'     print(total.legal.count)
##' 
##'     # shorthand using a list:
##'     ford.count <- mongo.count(mongo, "test.cars", list(make="Ford"))
##' }
##' 
mongo.count <- function(mongo, ns, query=mongo.bson.empty()) {
    if (typeof(query) == "list")
        query <- mongo.bson.from.list(query)
    .Call(".mongo.count", mongo, ns, query)
}



##' Issue a command to a database on MongoDB server
##' 
##' Issue a command to a MongoDB server and return the response from the
##' server.
##' 
##' This function supports any of the MongoDB database commands by allowing you
##' to specify the command object completely yourself.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/List+of+Database+Commands}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param db (string) The name of the database upon which to perform the
##' command.
##' @param command (\link{mongo.bson}) An object describing the command.
##' 
##' Alternately, \code{command} may be a list which will be converted to a
##' mongo.bson object by \code{\link{mongo.bson.from.list}()}.
##' @return NULL if the command failed.  \code{\link{mongo.get.err}()} may be
##' MONGO_COMMAND_FAILED.
##' 
##' (\link{mongo.bson}) The server's response if successful.
##' @seealso \code{\link{mongo.get.err}},\cr
##' \code{\link{mongo.simple.command}},\cr \code{\link{mongo.rename}},\cr
##' \code{\link{mongo.count}},\cr \code{\link{mongo.drop.database}},\cr
##' \code{\link{mongo.drop}},\cr \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##' 
##'     # alternate method of renaming a collection
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "renameCollection", "test.people")
##'     mongo.bson.buffer.append(buf, "to", "test.humans")
##'     command <- mongo.bson.from.buffer(buf)
##'     mongo.command(mongo, "admin", command)
##' 
##'     # use list notation to rename the collection back
##'     mongo.command(mongo, "admin", 
##'         list(renameCollection="test.humans", to="test.people"))
##' 
##'     # Alternate method of counting people
##'     buf <- mongo.bson.buffer.create()
##'     mongo.bson.buffer.append(buf, "count", "people")
##'     mongo.bson.buffer.append(buf, "query", mongo.bson.empty())
##'     command <- mongo.bson.from.buffer(buf)
##'     result = mongo.command(mongo, "test", command)
##'     if (!is.null(result)) {
##'         iter = mongo.bson.find(result, "n")
##'         print(mongo.bson.iterator.value(iter))
##'     }
##' 
##' }
##' 
mongo.command <- function(mongo, db, command) {
   if (typeof(command) == "list")
        command <- mongo.bson.from.list(command)
    .Call(".mongo.command", mongo, db, command)
}



##' Issue a simple.command to a database on MongoDB server
##' 
##' Issue a simple command to a MongoDB server and return the response from the
##' server.
##' 
##' This function supports many of the MongoDB database commands by allowing
##' you to specify a simple command object which is entirely specified by the
##' command name and an integer or string argument.
##' 
##' See \url{http://www.mongodb.org/display/DOCS/List+of+Database+Commands}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param db (string) The name of the database upon which to perform the
##' command.
##' @param cmdstr (string) The name of the command.
##' @param arg An argument to the command, may be a string or numeric
##' (as.integer).
##' @return NULL if the command failed.  Use \code{\link{mongo.get.last.err}()}
##' to determine the cause.
##' 
##' (\link{mongo.bson}) The server's response if successful.
##' @seealso \code{\link{mongo.command}},\cr \code{\link{mongo.rename}},\cr
##' \code{\link{mongo.count}},\cr \code{\link{mongo.drop.database}},\cr
##' \code{\link{mongo.drop}},\cr \link{mongo},\cr \link{mongo.bson}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.simple.command(mongo, "admin", "buildInfo", 1))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.simple.command <- function(mongo, db, cmdstr, arg)
    .Call(".mongo.simple.command", mongo, db, cmdstr, arg)



##' Drop a database from a MongoDB server
##' 
##' Drop a database from MongoDB server.  Removes the entire database and all
##' collections in it.
##' 
##' Obviously, care should be taken when using this command.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param db (string) The name of the database to drop.
##' @return (Logical) TRUE if successful; otherwise, FALSE
##' @seealso \code{\link{mongo.drop}},\cr \code{\link{mongo.command}},\cr
##' \code{\link{mongo.rename}},\cr \code{\link{mongo.count}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.drop.database(mongo, "test"))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.drop.database <- function(mongo, db)
    .Call(".mongo.drop.database", mongo, db)



##' Drop a collection from a MongoDB server
##' 
##' Drop a collection from a database on MongoDB server.  This removes the
##' entire collection.
##' 
##' Obviously, care should be taken when using this command.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param ns (string) The namespace of the collection to drop.
##' @return (Logical) TRUE if successful; otherwise, FALSE
##' @seealso \code{\link{mongo.drop.database}},\cr
##' \code{\link{mongo.command}},\cr \code{\link{mongo.rename}},\cr
##' \code{\link{mongo.count}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.drop(mongo, "test.people"))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.drop <- function(mongo, ns)
    .Call(".mongo.drop", mongo, ns)



##' Rename a collection on a MongoDB server
##' 
##' Rename a collection on a MongoDB server.
##' 
##' Note that this may also be used to move a collection from one database to
##' another.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param from.ns (string) The namespace of the collection to rename.
##' @param to.ns (string) The new namespace of the collection.
##' @return TRUE if successful; otherwise, FALSE.
##' @seealso \code{\link{mongo.drop.database}},\cr \code{\link{mongo.drop}},\cr
##' \code{\link{mongo.command}},\cr \code{\link{mongo.count}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.rename(mongo, "test.people", "test.humans"))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.rename <- function(mongo, from.ns, to.ns)
    .Call(".mongo.rename", mongo, from.ns, to.ns)



##' Get a list of databases from a MongoDB server
##' 
##' Get a list of databases from a MongoDB server.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @return (string vector) List of databases.  Note this will not include the
##' system databases "admin" and "local".
##' @seealso \code{\link{mongo.get.database.collections}},\cr
##' \code{\link{mongo.drop.database}},\cr \code{\link{mongo.command}},\cr
##' \code{\link{mongo.rename}},\cr \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.get.databases(mongo))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.get.databases <- function(mongo)
    .Call(".mongo.get.databases", mongo)



##' Get a list of collections in a database
##' 
##' Get a list of collections in a database on a MongoDB server.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param db (string) Name of the database for which to get the list of
##' collections.
##' @return (string vector) List of collection namespaces in the given
##' database.
##' 
##' Note this will not include the system collection \code{db}.system.indexes
##' nor the indexes attached to the database. Use \code{mongo.find(mongo,
##' "db.system.indexes", limit=0L)} for information on any indexes.
##' @seealso \code{\link{mongo.get.databases}},\cr
##' \code{\link{mongo.drop.database}},\cr \code{\link{mongo.drop}},\cr
##' \code{\link{mongo.command}},\cr \code{\link{mongo.rename}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     print(mongo.get.database.collections(mongo, "test"))
##' 
##'     mongo.destroy(mongo)
##' }
##' 
mongo.get.database.collections <- function(mongo, db)
    .Call(".mongo.get.database.collections", mongo, db)



##' Get a vector of distinct keys in a collection
##' 
##' Get a vector of distinct keys in a collection.
##' 
##' See
##' \url{http://www.mongodb.org/display/DOCS/Aggregation#Aggregation-Distinct}.
##' 
##' 
##' @param mongo (\link{mongo}) A mongo connection object.
##' @param ns (string) The namespace of the collection in which to find
##' distinct keys.
##' @param key (string) The name of the key field for which to get distinct
##' values.
##' @param query \link{mongo.bson} An optional query to restrict the returned
##' values.
##' @return NULL if the command failed.  \code{\link{mongo.get.err}()} may be
##' MONGO_COMMAND_FAILED.
##' 
##' (vector) The result set of distinct keys.
##' @seealso \code{\link{mongo.command}},\cr
##' \code{\link{mongo.simple.command}},\cr \code{\link{mongo.find}},\cr
##' \link{mongo}.
##' @export
##' @examples
##' 
##' mongo <- mongo.create()
##' if (mongo.is.connected(mongo)) {
##'     keys <- mongo.distinct(mongo, "test.people", "name")
##'     print(keys)
##' }
##' 
mongo.distinct <- function(mongo, ns, key, query=mongo.bson.empty()) {
    pos <- regexpr('\\.', ns)
    if (pos == 0) {
        print("mongo.distinct: No '.' in namespace")
        return(NULL)
    }
    db <- substr(ns, 1, pos-1)
    collection <- substr(ns, pos+1, nchar(ns))
    b <- mongo.command(mongo, db, list(distinct=collection, key=key, query=query))
    if (!is.null(b))
        b <- mongo.bson.value(b, "values")
    b
}
