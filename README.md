# Inatel Open Low-Code Platform

## Code Generator
This serves as the code generator for the platform and contains functions and types that can be used to convert the editor's language into other languages (currently only JavaScript, CSS and HTML).

### Building
You can use `stack build` to build this project. For that, you can get [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

You can run the project with `stack run -- --dbhost ADDRESS --dbname postgres --dbpassword PASSWORD --dbport DB_PORT --dbuser USERNAME --connections CONNECTION_POOL_COUNT --port PORT --certificate PATH_TO_CERTIFICATE --key PATH_TO_KEY`. A PostgreSQL database running in the background is needed. The database schema will automatically be created once the project is run for the first time (as defined in [Database.hs](https://github.com/heitor-lassarote/iolp/blob/master/code-generator/app/Database.hs#L25#L33)).

Additionally, you can specify the additional flags `--accept-insecure` to accept HTTP connections, `--detailed-request-logging` to have a more verbose logging of requests and `--ip-from-header` to use IP from reader when logging (useful when using a reverse proxy).

In case you need to generate a key and certificate, try reading the `warp-tls` [README](https://github.com/yesodweb/wai/blob/50d7a20ca31b9ff36b208ac2cb343c0821a93b25/warp-tls/README.md).

Afterwards, navigate to the given host to use it.

### Tech
The main technologies being used in this project are:
* [Haskell2010](https://www.haskell.org/)
* [Cabal](https://github.com/haskell/cabal)
* [Stack](https://github.com/commercialhaskell/stack)
* [Yesod](https://www.yesodweb.com/)
* [PostgreSQL](https://www.postgresql.org/) via [persistent](https://github.com/yesodweb/persistent)

For a complete list of dependencies, see [low-code.cabal](https://github.com/heitor-lassarote/iolp/blob/master/code-generator/low-code.cabal).

## IDE

This project was developed to undergraduate thesis of [INATEL](https://inatel.br/home/).

This project was generated with [Angular CLI](https://github.com/angular/angular-cli) version 11.0.1.

### Made by

[Bruno Pereira Garcia Caputo](https://github.com/BrunoCaputo) & [Heitor Toledo Lassarote de Paula](https://github.com/heitor-lassarote).

### Contributing

Anyone is free to contribute! Just make sure to follow the [Code of Conduct](https://github.com/heitor-lassarote/iolp/blob/master/CODE_OF_CONDUCT.md).

As for the coding style, the most important is to stay consistent with the existing code, but [this should be a good starting point](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

### License
This project is licensed under the [MIT License](https://github.com/heitor-lassarote/iolp/blob/master/LICENSE).
