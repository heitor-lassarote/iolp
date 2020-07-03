# LowCode Platform Backend (Temporary Name)
This serves as the backend for the [LowCode UI](https://github.com/BrunoCaputo/low-code-ui), and contains functions and types that can be used to convert the editor's language into other languages (currently only JavaScript, CSS and HTML).

## Building
You can use `stack build` to build this project. To use Stack, you can get the [Haskell Platform](https://www.haskell.org/platform/).

You can run the project with `stack run -- --host HOST --port PORT --user DBUSER --dbname postgres --password PASSWORD --connections CONNECTION_POOL_COUNT`. A PostgreSQL database running in the background is needed. The database schema will automatically be created once the project is run for the first time (as defined in [Database.hs](https://github.com/heitor-lassarote/low-code/blob/master/app/Database.hs#L64#L74)).

Afterwards, navigate to the given host to use it.

## Tech
The main technologies being used in this project are:
* [Haskell2010](https://www.haskell.org/)
* [Cabal](https://github.com/haskell/cabal)
* [Stack](https://github.com/commercialhaskell/stack)
* [Yesod](https://www.yesodweb.com/)
* [PostgreSQL](https://www.postgresql.org/) via [persistent](https://github.com/yesodweb/persistent)

For a complete list of dependencies, see [low-code.cabal](https://github.com/heitor-lassarote/low-code/blob/master/low-code.cabal).

# Contributing

Anyone is free to contribute! Just make sure to follow the [Code of Conduct](https://github.com/heitor-lassarote/low-code/blob/master/CODE_OF_CONDUCT.md).

As for the coding style, the most important is to stay consistent with the existing code, but [this should be a good starting point](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

## License
This project is licensed under the [MIT License](https://github.com/heitor-lassarote/low-code/blob/master/LICENSE).
