# Frequency Server with Supervisor
At the start of week 3, we added a supervisor process to act as middleware betwen the client and the frequency server.  The purpose of this supervisor is the following:

1. To act as a load balancer for however many servers it administers.  In this case a simple round robin algorithm is used.
2. To restart a server in the event that it crashes
3. To allow new srvers to be added
4. To allow existing servers to be dropped

The frequency client sends its request to the supervisor which in turn, sends that request to the next server.  The server then responds directly to the client, bypassing the supervisor.

## Frequency Supervisor API
`start/0`  
Starts the frequency supervisor.  By default, the supervisor will spawn two freqeuncy servers. If a frequency server crashes, by default, the supervisor will restart it no more than twice.

`stop/0`  
Stops the frequency supervisor and all child freqeuncy servers.

`add_server/1`  
Causes a new frequency server to be created that administers the frequencies in the argument list.

`drop_server/1`  
Drops the frequency server identified the integer (server id) passed as an argument.

`list_servers/0`  
Prints a list of the currently active freqeuncy servers, similar to the following:

```erlang
---------------------------------+
Server Id      : 1
Server Pid     : <0.81.0>
Frequency List : [10,11,12,13,14,15]
Restart count  : 0
+----------------------------------+
Server Id      : 2
Server Pid     : <0.82.0>
Frequency List : [20,21,22,23,24,25]
Restart count  : 0
```

## Frequency Client API
`request_frequency/0`  
Requests that a frequency be allocate to this client.  The server that answers this request is determined by the round robin algorithm in the supervisor.

## Frequency Server API
The frequency server's API is managed entirely by the supervisor.

## Usage
```erlang
1> c(freq_supervisor, {d, debug}).
{ok,freq_supervisor}
2> c(freq_client, {d, debug}).
{ok,freq_client}
3> c(freq_server, {d, debug}).
{ok,freq_server}
4> freq_supervisor:start().
freq_supervisor:start/0  Starting frequency server supervisor
freq_supervisor:init/1  Initializing 2 frequency servers
true
freq_supervisor:start_server/3  Frequency server 1 starting (Restarts = 0)
freq_server:start/2  Starting freq_server1
freq_supervisor:start_server/3  Frequency server 2 starting (Restarts = 0)
freq_server:loop/1  Server 1 entering receive loop
freq_server:start/2  Starting freq_server2
freq_supervisor:child_minder/1  Monitoring 2 frequency servers
freq_server:loop/1  Server 2 entering receive loop
5> freq_client:request_frequency().
freq_client:request_frequency/0  Requesting a frequency
freq_client:send_to_supervisor/1  Sending allocate to supervisor
freq_supervisor:send_to_server/3  Sending {request,<0.56.0>,{1492,979934,819022},allocate} to server 1
freq_supervisor:child_minder/1  Monitoring 2 frequency servers
freq_server:loop/1  Sleeping for 200ms before answering request...
freq_server:allocate/3  Allocating next frequency to client <0.56.0>
freq_server:loop/1  Server 1 entering receive loop
Frequency 10 allocated by server 1
undefined
6> freq_client:drop_frequency().   
freq_client:drop_frequency/0  Deallocating frequency 10
freq_client:send_to_supervisor/1  Sending {deallocate,10,1} to supervisor
freq_supervisor:send_to_server/3  Sending {deallocate,10} to server 1
freq_supervisor:child_minder/1  Monitoring 2 frequency servers
freq_server:loop/1  Sleeping for 200ms before answering request...
freq_server:deallocate/2  Deallocating frequency 10 from <0.56.0>
freq_server:loop/1  Server 1 entering receive loop
Frequency 10 deallocated
ok

```
