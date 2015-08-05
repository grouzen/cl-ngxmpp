# cl-ngxmpp


[![Build Status](https://travis-ci.org/grouzen/cl-ngxmpp.svg?branch=development)](https://travis-ci.org/grouzen/cl-ngxmpp)

Native Common Lisp library for XMPP RFCs.

WARNING: This library is under heavy development.

# TODO:

- [X] Migrate from cl-async to blackbird library
- [X] Fix Travis-CI build
- [X] Code DSL to have more concise way to define stanzas
- [X] Re-use the brand new `define-stanza` in a definition of `define-xep` macro
- [X] Use short package nicknames instead of long names
- [X] Reorganize the structure of files and directories
- [X] Be able to represent stanzas as XML-encoded strings
- [ ] Get rid of `send-*` methods/functions, substitute them with macro `send-stanza`
- [ ] Re-think and (it would be better) rewrite the code in `client/xeps/xeps.lisp`
- [ ] Rewrite tests using mocks
- [ ] Think about adding hooks for basic actions like: connecting, disconnecting, authenticating, etc.

      It could be represented as a set of well-defined wrappers over `xmpp%:handle-stanza` method.
      There are some number of approaches to managable, user-defined, flexible hooks system:
      global hooks, like: `(add-hook 'some-hook #'(lambda () ...))`; per-session hooks.

- [ ] Try to split out xeps/async/etc functionality into different packages
- [ ] Move `handle-stanza` generic method from `xmpp%` package into `xmpp`
      (since, it's not a part of stanza's protocol anymore)
- [ ] Develop a high-level interface
- [ ] Asynchronous high-level interface
- [ ] Debug levels, and generic `print-debug` function
- [ ] Write more descriptive README
- [ ] Develop a simple bot for conferences based on 'Markov chains' as another example


# Architecture Overview

The XMPP constists of core and XEP parts, so the library tries to follow
the idea of extendable design incorporating several layers.

## Layers

### Low-level

A low-level code that is responsible for such things like:
connecting to a server through TCP socket, making the socket secure using TLS,
SASL authentication, XML parsing, and the core set of stanzas; is in the `xmpp%` package.
So, in fact, it implements the basic blocks for the Core part of the protocol, consequently,
theoretically, it can be used to implement not only the client side but also the server side.

### High-level

High-level part is in the package `cl-ngxmpp-client` (there is `xmpp` alias for it) — the most
interesting part for users of the library. It hides all low-level stuff under the hood,
instead of that, it gives you well-bounded entities such as: roster, message chat, groupchat,
file transfer, etc. The basic use-case suggests reacting on incoming events defining hooks
available for particular entity. There is a simple EDSL which allows you to define your own
XEPs easily.

## Diagram

```

              +----------------------------------------------------------------------------------------+
              |                                                                                        |
              |  +---------------------------------------------------------------------------------------+
              |  |                                                                                       |
              |  |  +--------------------------------------------------------------------------------------+
              |  |  |                                                                                      |
              |  |  |    +-------------,                 Core (Low-level)                                  |
              |  |  |    | +------------,                                                                  |
              |  |  |    | | +-----------,     +--------------+    +--------------+    +----------------+  |
              |  |  |    | | |           |     | XML-STREAM   |    | CONNECTION   |    | ADAPTER        |  |
              |  |  |    `-| |   XEPs    |     |              |    |              |    |                |  |
              |  |  |      `-|           |     | connection @----->| adapter @-------->| socket-stream  |  |
              |  |  |        +-----------+     +--------------+    +--------------+    +----------------+  |
              |  |  |                 ^                      ^                                             |
              |  |  |                 |               Client | (High-level)                                |
              |  |  |                 |                      |                                             |
              |  |  |   +-----------------+    +------------------+                                        |
              |  |  |   | SESSION     |   |    | CLIENT      |    |                                        |
              |  |  |   |             |   |    |             |    |                                        |
              |  |  |   | client @----|------->| xml-stream -@    |                                        |
              |  |  |   |             |   |    +------------------+                                        |
              |  |  |   | xeps-list @-*   |                                                                |
              |  |  |   |                 |    +-------------------,                                       |
              |  |  |   | domains @----------->| +-------------------,                                     |
              |  |  |   |                 |    | | +------------------+                                    |
              |  |  |   +-----------------+    | | |                  |                                    |
              |  |  |                          | | |     DOMAINs      |                                    |
              |  |  |                          `-| |                  |                                    |
              +--|  |                            `-|                  |                                    |
                 +--|                              +------------------+                                    |
                    +--------------------------------------------------------------------------------------+

```

# Examples:

Notice! Current examples are deprecated!

You can find examples in `examples/` directory.
First you need to load examples system:

```commonlisp
(ql:quickload :cl-ngxmpp-client.examples)
```

There is `echo-bot.lisp` example, to run it type:

```commonlisp
(cl-ngxmpp-client.examples.echo-bot:run
    :server-hostname "<your jabber server>"
    :username        "<your jabber login>"
    :password        "<your jabber password>"
    :to              "<opponent's jid>"
    :body            "<body of your message>")
```

After that bot will send the message, and will wait for messages in infinite loop.
