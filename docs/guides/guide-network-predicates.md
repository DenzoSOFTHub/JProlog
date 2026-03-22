# JProlog Network Predicates — User Guide

This guide covers all network communication built-in predicates: TCP sockets (client/server), HTTP requests, UDP datagrams, and DNS resolution.

## 1. TCP Client Communication

### Connecting to a Server

```prolog
% tcp_connect/3 - create a TCP client connection
?- tcp_connect('localhost', 8080, Socket).
% Socket = '$socket_1'

% Connect to a remote host
?- tcp_connect('api.example.com', 443, Socket).
```

### Sending Data

```prolog
% tcp_send/2 - send a string over a socket
?- tcp_send(Socket, 'Hello, server!').

% Send HTTP-like request manually
?- tcp_send(Socket, 'GET / HTTP/1.1\r\nHost: example.com\r\n\r\n').
```

### Receiving Data

```prolog
% tcp_receive/3 - receive up to MaxBytes bytes
?- tcp_receive(Socket, Data, 4096).
% Data = 'HTTP/1.1 200 OK...'

% End-of-stream detection
?- tcp_receive(Socket, Data, 1024).
% Data = 'end_of_stream'   (when the remote closes the connection)
```

### Closing a Socket

```prolog
?- tcp_close(Socket).
```

### Complete TCP Client Example

```prolog
% Simple TCP echo client
echo_client(Host, Port, Message, Response) :-
    tcp_connect(Host, Port, Socket),
    tcp_send(Socket, Message),
    tcp_receive(Socket, Response, 4096),
    tcp_close(Socket).

% ?- echo_client('localhost', 7777, 'ping', Reply).
% Reply = 'pong'
```

## 2. TCP Server

### Creating a Server Socket

```prolog
% tcp_server_socket/2 - bind to a port and listen
?- tcp_server_socket(9090, ServerSocket).
% ServerSocket = '$server_socket_1'
```

### Accepting Client Connections

```prolog
% tcp_accept/2 - wait for and accept an incoming connection (blocking)
?- tcp_accept(ServerSocket, ClientSocket).
% ClientSocket = '$socket_2'
```

### Complete TCP Server Example

```prolog
% Simple echo server (handles one client)
start_echo_server(Port) :-
    tcp_server_socket(Port, SS),
    write('Server listening on port '), write(Port), nl,
    tcp_accept(SS, Client),
    write('Client connected'), nl,
    handle_client(Client),
    tcp_close(SS).

handle_client(Socket) :-
    tcp_receive(Socket, Data, 4096),
    (Data = 'end_of_stream' ->
        write('Client disconnected'), nl,
        tcp_close(Socket)
    ; Data = 'quit' ->
        tcp_send(Socket, 'Goodbye!'),
        tcp_close(Socket)
    ;
        write('Received: '), write(Data), nl,
        tcp_send(Socket, Data),    % Echo back
        handle_client(Socket)      % Continue
    ).
```

### Multi-Client Server (Sequential)

```prolog
% Accepts clients in a loop
server_loop(ServerSocket) :-
    tcp_accept(ServerSocket, Client),
    handle_client(Client),
    server_loop(ServerSocket).

run_server(Port) :-
    tcp_server_socket(Port, SS),
    server_loop(SS),
    tcp_close(SS).
```

## 3. HTTP Requests

### GET Requests

```prolog
% http_request/4 - generic HTTP request
?- http_request(get, 'http://httpbin.org/get', Status, Body).
% Status = 200
% Body = '{"args":{},"headers":{},...}'

% HEAD request
?- http_request(head, 'http://example.com', Status, _).
% Status = 200

% DELETE request
?- http_request(delete, 'http://api.example.com/items/42', Status, Body).
```

### POST Requests

```prolog
% http_post/4 - POST with a request body
?- http_post('http://httpbin.org/post', 'name=Alice&age=30', Status, Body).
% Status = 200
% Body = '{"form":{"name":"Alice","age":"30"},...}'

% POST JSON (content-type is application/x-www-form-urlencoded by default)
?- http_post('http://api.example.com/data', '{"key":"value"}', Status, Body).
```

### REST API Client Example

```prolog
% Get a resource
api_get(BaseURL, Resource, Data) :-
    atom_concat(BaseURL, Resource, URL),
    http_request(get, URL, Status, Data),
    Status =:= 200.

% Create a resource
api_create(BaseURL, Resource, Payload, Response) :-
    atom_concat(BaseURL, Resource, URL),
    http_post(URL, Payload, Status, Response),
    Status =:= 201.

% Usage:
% ?- api_get('http://api.example.com', '/users/1', UserData).
% ?- api_create('http://api.example.com', '/users', 'name=Bob', Resp).
```

### Error Handling with HTTP

```prolog
safe_http_get(URL, Result) :-
    catch(
        (http_request(get, URL, Status, Body),
         (Status =:= 200 ->
             Result = ok(Body)
         ;
             Result = error(Status, Body)
         )),
        Error,
        Result = exception(Error)
    ).
```

## 4. UDP Datagrams

### Creating a UDP Socket

```prolog
% udp_socket/2 - bind to a port
?- udp_socket(5000, UdpSock).
% UdpSock = '$udp_1'
```

### Sending a Datagram

```prolog
% udp_send/4 - send to a specific host:port
?- udp_send(UdpSock, 'localhost', 5001, 'Hello UDP!').
```

### Receiving a Datagram

```prolog
% udp_receive/4 - receive data with sender info (blocking)
?- udp_receive(UdpSock, Data, From, 1024).
% Data = 'Hello UDP!'
% From = from('127.0.0.1', 5000)
```

### Closing a UDP Socket

```prolog
?- udp_close(UdpSock).
```

### UDP Ping-Pong Example

```prolog
% UDP server (responder)
udp_responder(Port) :-
    udp_socket(Port, Sock),
    udp_receive(Sock, Data, from(IP, SenderPort), 1024),
    write('Received: '), write(Data), write(' from '), write(IP), write(':'), write(SenderPort), nl,
    udp_send(Sock, IP, SenderPort, 'pong'),
    udp_close(Sock).

% UDP client (sender)
udp_ping(ServerHost, ServerPort, Response) :-
    udp_socket(0, Sock),          % Bind to any available port
    udp_send(Sock, ServerHost, ServerPort, 'ping'),
    udp_receive(Sock, Response, _, 1024),
    udp_close(Sock).
```

## 5. DNS Resolution

```prolog
% hostname_address/2 - resolve hostname to IP address
?- hostname_address('localhost', IP).
% IP = '127.0.0.1'

?- hostname_address('www.google.com', IP).
% IP = '142.250.180.4'

% Use in combination with TCP
connect_by_name(Hostname, Port, Socket) :-
    hostname_address(Hostname, IP),
    write('Resolved '), write(Hostname), write(' to '), write(IP), nl,
    tcp_connect(IP, Port, Socket).
```

## 6. Protocol Implementation Examples

### Simple Chat Protocol

```prolog
% Server
chat_server(Port) :-
    tcp_server_socket(Port, SS),
    write('Chat server started on port '), write(Port), nl,
    tcp_accept(SS, Client),
    tcp_send(Client, 'Welcome to JProlog Chat!\n'),
    chat_loop(Client),
    tcp_close(SS).

chat_loop(Socket) :-
    tcp_receive(Socket, Msg, 4096),
    (Msg = 'end_of_stream' -> true
    ; Msg = '/quit' ->
        tcp_send(Socket, 'Bye!\n'),
        tcp_close(Socket)
    ;
        atom_concat('You said: ', Msg, Echo),
        tcp_send(Socket, Echo),
        chat_loop(Socket)
    ).
```

### HTTP Health Checker

```prolog
% Check if multiple URLs are reachable
check_health(URLs, Results) :-
    findall(
        result(URL, Status),
        (member(URL, URLs),
         catch(
             (http_request(get, URL, Status, _)),
             _,
             Status = error
         )),
        Results
    ).

% Usage:
% ?- check_health(['http://google.com', 'http://github.com'], R).
% R = [result('http://google.com', 200), result('http://github.com', 200)]
```

### Simple Key-Value Store over TCP

```prolog
% Protocol: GET key\n  -> value\n
%           SET key value\n  -> OK\n

:- dynamic kv_store/2.

kv_server(Port) :-
    tcp_server_socket(Port, SS),
    kv_accept_loop(SS).

kv_accept_loop(SS) :-
    tcp_accept(SS, Client),
    kv_handle(Client),
    kv_accept_loop(SS).

kv_handle(Socket) :-
    tcp_receive(Socket, Cmd, 4096),
    (Cmd = 'end_of_stream' ->
        tcp_close(Socket)
    ;
        process_kv_command(Cmd, Response),
        tcp_send(Socket, Response),
        kv_handle(Socket)
    ).

process_kv_command(Cmd, Response) :-
    (atom_concat('GET ', Key, Cmd) ->
        (kv_store(Key, Value) ->
            Response = Value
        ;
            Response = 'NOT_FOUND'
        )
    ; atom_concat('SET ', Rest, Cmd) ->
        % Simple parse: first token is key, rest is value
        assertz(kv_store(key, Rest)),
        Response = 'OK'
    ;
        Response = 'UNKNOWN_COMMAND'
    ).
```

## 7. Predicate Reference Summary

| Predicate | Description |
|-----------|-------------|
| **TCP Client** | |
| `tcp_connect/3` | Connect to host:port, return socket handle |
| `tcp_send/2` | Send string data over socket |
| `tcp_receive/3` | Receive up to N bytes (`end_of_stream` on close) |
| `tcp_close/1` | Close client or server socket |
| **TCP Server** | |
| `tcp_server_socket/2` | Create server socket on port |
| `tcp_accept/2` | Accept incoming connection (blocking) |
| **HTTP** | |
| `http_request/4` | HTTP GET/POST/PUT/DELETE (Method, URL, Status, Body) |
| `http_post/4` | HTTP POST with request body (URL, ReqBody, Status, RespBody) |
| **UDP** | |
| `udp_socket/2` | Create UDP socket bound to port |
| `udp_send/4` | Send datagram (Socket, Host, Port, Data) |
| `udp_receive/4` | Receive datagram (Socket, Data, from(IP,Port), MaxBytes) |
| `udp_close/1` | Close UDP socket |
| **DNS** | |
| `hostname_address/2` | Resolve hostname to IP address |

## 8. Notes and Limitations

- **Blocking I/O**: `tcp_accept`, `tcp_receive`, and `udp_receive` are blocking calls. They will wait until data arrives or the connection closes.
- **Timeouts**: `http_request` and `http_post` have a 30-second connect and read timeout.
- **Data format**: All data is transmitted as UTF-8 encoded strings. For binary protocols, use the byte-level I/O approach with code lists.
- **Concurrency**: Prolog execution is single-threaded. For concurrent server applications, consider handling one client at a time or using separate Prolog instances.
- **Security**: HTTPS connections depend on the JVM's default SSL configuration. No custom certificate handling is provided.
