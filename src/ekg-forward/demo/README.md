# Demo Programs

There are two demo programs, `demo-forwarder` and `demo-acceptor`. You can run these programs in different terminals and see an interaction between them.

Please see `forwarder.hs` module as an example of how to use `ekg-forward` library in your Forwarder application, and `acceptor.hs` module as an example of how to use it in your Acceptor application.

## How To Build It

As a result of `cabal build all` command, two demo programs will be built: `demo-forwarder` and `demo-acceptor`.

## How To Run It

Use the following commands:

```
cabal exec -- demo-forwarder
```

```
cabal exec -- demo-acceptor
```
