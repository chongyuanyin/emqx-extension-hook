# emqx_extension_hook

The `emqx_extension_hook` extremly enhance the extensibility for EMQ X. It allow using an others programming language to mount the hooks intead of erlang.

## Feature

- [ ] Support variaty of programming language or web services.
- [x] Support all hooks of emqx.
- [x] Allows you to use the return value to extend emqx behavior.

Notes: The current version only support `python` and `java`.

We temporarily no plans to support other languages. Plaease open a issue if you have to use other programming languages.

## Architecture

```
                            EMQ X
                            +============================+
                            |        Extension           |
 +----------+    CONNECT    | Hooks +----------------+   |
 |  Device  | <===========> - - - ->|    Drivers     |   |
 +----------+    PUB/SUB    |       +----------------+   |
                            |               |            |
                            +===============|============+
                                            |
                                            | Callbacks
             Third-party Runtimes           |
             +=======================+      |
             |  Python Script/ Java  |<-----+
             |  Classes/ Others      |
             +=======================+
```

## Drivers

### Python

***Requirements:***

- It requires the emqx hosted machine has Python2/Python3 Runtimes
- An executable commands in your shell, i,g: `python2` or `python3`

***Examples:***

See `test/scripts/main.py`

### Java

***Requirements:***

- It requires the emqx hosted machine has Java 8+ Runtimes
- An executable commands in your shell, i,g: `java`

***Examples:***

See `test/scripts/Main.java`

## Integration Guides

TODO.

## Beachmark

TODO.

## Known Issues or TODOs

- Provide a High level APIs
    * High level SDK for all funcs
    * Packaging Python/Java sources code for developing in IDE

- Configurable Log System.
    * The Java driver can not redirect the `stderr` stream to erlang vm on Windows platform

## Reference

- [erlport](https://github.com/hdima/erlport)
- [Eexternal Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- [The Ports Tutorial of Erlang](http://erlang.org/doc/tutorial/c_port.html)
