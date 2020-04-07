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

## Examples

- Python2/Python3: see `test/main.py`
- ...

## Programming Guides

See `docs/programming_guides.md`

## Beachmark

TODO.

## Reference

- [erlport](https://github.com/hdima/erlport)
- [Eexternal Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- [The Ports Tutorial of Erlang](http://erlang.org/doc/tutorial/c_port.html)
