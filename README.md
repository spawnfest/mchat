MCHAT
=====

Simple chat system that can do file transfers over websockets :)
- However file transfers work only in chrome browsers :(
- Made possible by Erlang, Cowboy and Web Workers with Binary WebSockets.

Build
-----
Assuming you have rebar install just run:
<pre>make</pre>

Run
---
<pre>
make run
</pre>
Default http://localhost:8080

Config
------
In config dir make appropriate changes.
Note: ip and port settings should be kept in sync for
      sys.config <-> client-config.json

Bugs
----
Testing over local network i.e. not "localhost"
- Get precondition failed errors on loading files from server the first time.
- Hence had to reload or retry certain features twice before resources were loaded.
