DSLR-App WWW 
============================

Companion API for the [PB&J Camera Dolly Controller](http://www.github.com/jhstanton/DSLR-App) to
extend app functionality. The goal is to allow users to save keyframes independently of the user's
device.

This is deployed to Heroku and built using Halcyon following the [Haskell on Heroku
tutorial](https://haskellonheroku.com/tutorial/).

To run this locally do the following - 

```
% cabal sandbox init
% cabal install 
```

If needed, export a DATABASE_URL environment variable for local development. This assumes a local
Postgresql database already exists. The app performs all migrations automatically before serving
content by using the [`Persistent`](http://hackage.haskell.org/package/persistent) library. 

```
% export DATABASE_URL=postgres://$PG_USER:$PG_PW@$PG_HOST:$PG_PORT/$PG_DATABASE
```

where `$PG_USER`, `$PG_PW`, `$PG_HOST`, `$PG_PORT`, and `$PG_DATABASE` are the Postgres user to
connect with, the user's password, the server the database is hosted on, the port the service
listens to, and, finally, the name of the Postgres database in the cluster. 

The current URL for the dev shard on Heroku is (https://ancient-falls-78055.herokuapp.com)[https://ancient-falls-78055.herokuapp.com].

API documentation can be found in the (API documentation generate by Servant)[API.md].
