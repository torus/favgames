Favorite Games
=======================

IGDB https://api-docs.igdb.com/

Run
---

```
$ sudo docker-compose up
```

Server runs on http://localhost:2225

Setup
-----

Create `lib/config.scm` like so:

```
(define-module config
  (export twitch-clinet-id twitch-secret twitch-access-token
		  twitch-token-type)
  )

(select-module config)

(define twitch-clinet-id "CLIENT ID")
(define twitch-secret "SECRET")
(define twitch-access-token "ACCESS TOKEN")
(define twitch-token-type "bearer")
```

Create Tables
-------------

http://localhost:2225/admin/setup


See Also
---------

[torus/gauche-violet: Libuv + Gauche](https://github.com/torus/gauche-violet)
