# cl-ngxmpp


[![Build Status](https://travis-ci.org/grouzen/cl-ngxmpp.svg?branch=development)](https://travis-ci.org/grouzen/cl-ngxmpp)

The native Common Lisp library for XMPP RFCs.

WARNING: This library is under heavy development.
    
# Architecture Overview

The XMPP basically constists of the core and XEP parts, the library tries to follow
an idea of an extendable design incorporating several layers.

## The Layers

### Low-level

The low-level code that is responsible for such things like:
connecting to server through TCP socket, making the socket secure using TLS,
SASL authentication, XML parsing, and the core set of stanzas; is in the `xmpp%` package.
So, in fact, it implements the basic blocks for the Core part of the protocol, consequently,
theoretically, it can be used to implement not only the client side but also the server side.

### High-level

The high-level part is in the package `cl-ngxmpp-client` (there is `xmpp` alias for it) — the most
interesting part for users of the library. It hides all low-level stuff under the hood,
instead of that, it gives you well-bounded entities (futher I'll call them "domains") such as:
roster, message chat, groupchat, file transfer, etc. The basic use-case suggests reacting on
incoming events defining hooks available for particular entity. There is a simple EDSL which
allows you to define your own XEPs easily.

## Diagram

*This section is not finished yet...*

To understand better what I mean I draw this diagram:


```

                                      Global View

 +--------------------------------------------------------------------------------------+
 |                                                                                      |
 |     +-------------,                 Core (Low-level)                                 |
 |     | +-----------,                                                                  |
 |     | | +-----------,     +--------------+    +--------------+    +----------------+ |
 |     | | |           |     | XML-STREAM   |    | CONNECTION   |    | ADAPTER        | |
 |     `-| |   XEPs    |     |              |    |              |    |                | |
 |       `-|           |     | connection @----->| adapter @-------->| socket-stream  | |
 |         +-----------+     +--------------+    +--------------+    +----------------+ |
 |                 ^                      ^                                             |
 |                 |               Client | (High-level)                                |
 |                 |                      |                                             |
 |   +-------------|---+    +-------------|----+                                        |
 |   | SESSION     |   |    | CLIENT      |    |                                        |
 |   |             |   |    |             |    |                                        |
 |   | client @----|------->| xml-stream @`    |                                        |
 |   |             |   |    +------------------+                                        |
 |   | xeps-list @-*   |                                                                |
 |   |                 |    +-------------------,                                       |
 |   | domains @----------->| +-------------------,                                     |
 |   |                 |    | | +------------------+                                    |
 |   +-----------------+    | | |                  |                                    |
 |                          | | |     DOMAINs ---. |                                    |
 |                          `-| |                | |                                    |
 |                            `-|                | |                                    |
 |                              +----------------|-+                                    |
 +-----------------------------------------------|--------------------------------------+
                                                 |
                                                 V

                                  Domain View


                     SESSION                 DOMAIN

     Network ------> route_stanza()          user-defined-routes 
                                             (dispathers over stanza's type)
                     domains


```

# How To Use

## Using low-level API

You shouldn't use it, it's an internal API.

## Using intermediate-level API

Use this API for simple apps or as a foundation for your own extensions above the cl-ngxmpp library.

The very basic example how to create a client, connect the client to a xmpp server,
log in, send a message, and wait for a response:

```commonlisp
(ql:quickload :cl-ngxmpp-client)

(let ((xmpp-client (make-instance 'xmpp:client :debuggable t)))
  (xmpp:connect-client xmpp-client :server-hostname "hostname")
  (when (xmpp:connectedp xmpp-client)
    (xmpp:login-client xmpp-client
                       :username "username"
                       :password "password"
                       ;; Only PLAIN and DIGEST-MD5 mechanisms are available due to the lack
                       ;; of support for others in the cl+ssl library
                       :mechanism "PLAIN or DIGEST-MD5")
    (when (xmpp:loggedinp xmpp-client)
      (xmpp:send-message xmpp-client :to "to_jid" :body "message")
      (let ((response (xmpp:receive-stanza xmpp-client)))
        ;; Here you get the instance of one of the stanza classes (see src/core/stanzas.lisp file).
        ;; Do whatever you want with it.
        (do something with the response)))))
```

Another way to handle incoming stanzas, but I wouldn't recommend using it:

```commonlisp
(ql:quickload :cl-ngxmpp-client)

;; Define a method for handling stanzas of a type of 'message'
(defmethod xmpp%:handle-stanza ((stanza xmpp%:message-stanza))
  (print stanza))

(let ((xmpp-client (make-instance 'xmpp:client :debuggable t)))
  (xmpp:connect-client xmpp-client :server-hostname "hostname")
  (when (xmpp:connectedp xmpp-client)
    (xmpp:login-client xmpp-client
                       :username "username"
                       :password "password"
                       :mechanism "PLAIN or DIGEST-MD5")
    (when (xmpp:loggedinp xmpp-client)
      (xmpp:send-message xmpp-client :to "to_jid" :body "message")
      ;; It waits for an incoming stanza, then calls an appropriate handle-stanza method.
      ;; It's something like an asynchronous interface. See also xmpp:proceed-stanza-loop.
      (xmpp:proceed-stanza xmpp-client))))

```

In case if you miss some functionality in the core XMPP protocol and need to use certain XEPs,
you can easily turn on needed XEPs (see a list of available XEPs in src/xeps/ directory):

```commonlisp
(ql:quickload :cl-ngxmpp-client)

(let ((xmpp-client (make-instance 'xmpp:client :debuggable t)))
  (xmpp:register-xeps xmpp-client '("multi-user-chat"
                                    "delayed-delivery"))
  ...)

;; Each xep provides its own list of stanzas, these stanzas are the same as usual stanzas
;; from the core (xmpp%) package. That means that you can use them the same
;; way as you did with core stanzas.
;;
;; Attention! The way to work with XEPs can be changed in the future!

```

# Examples:

Notice! Current examples are deprecated!

You can find the examples inside a `src/client/examples/` directory.
First you need to load an 'examples' system:

```commonlisp
(ql:quickload :cl-ngxmpp-client.examples)
```

There is an `echo-bot.lisp` example, to run it type in REPL:

```commonlisp
(cl-ngxmpp-client.examples.echo-bot:run
    :server-hostname "<your jabber server>"
    :username        "<your jabber login>"
    :password        "<your jabber password>"
    :to              "<opponent's jid>"
    :body            "<body of your message>")
```

after that the bot will send a message to your opponent, and then will be waiting
for messages from him/her in an infinite loop.

# ToDo:

- [X] Migrate from cl-async to blackbird library
- [X] Fix the Travis-CI build
- [X] Develop a DSL to have a more concise way to define stanzas
- [X] Re-use the brand new `define-stanza` in a definition of the `define-xep` macro
- [X] Use short package nicknames instead of the long names
- [X] Reorganize the structure of files and directories
- [X] Be able to represent stanzas as XML-encoded strings
- [X] Implement a generic `print-debug` function
- [X] Make the README file more descriptive
- [X] *CANCELLED* Move the `handle-stanza` generic method from the `xmpp%` package
into the `xmpp` (since, it's not a part of the stanza protocol anymore)
- [ ] *BLOCKED* Get rid of the `send-*` methods/functions, substitute them with a `send-stanza` macro
- [X] Re-think and (it would be better) rewrite/remove some code in the `client/xeps/xeps.lisp`
- [ ] Prepare the core version of the library for getting it into quicklisp repo
    - [X] Show usage examples
    - [ ] Merge the development and master branches to make a release
- [X] Revisit the `core/xeps.lisp`. The `xmpp-xeps:register-xeps` function should work
in scope of `client` objects. Currently, it affects the global scope, so that if multiple
`clients` are running in the same lisp image, they are writing/reading to/from a dynamic
variable *stanzas-dispatchers*, that's a race condition.
- [ ] Write more XEPs (see next item)
    - [ ] *NOT FINISHED* 0045 Multi User Chat (MUC)
    - [X] 0203 Delayed Delivery
    - [X] 0004 Data Forms
    - [ ] *NOT FINISHED* 0077 In-Band Registration
- [ ] Figure out how to validate stanzas (xml schema is a good option I guess).
Since there is no CL library for xmlschema, I can go further and try to develop one. It can be used for
stanza validation/generation, and can avoid a manual work for these areas in the future.
- [ ] Add the hostname verification against a SSL certificate ([https://tools.ietf.org/html/rfc6125#section-5](https://tools.ietf.org/html/rfc6125))
- [ ] Implement an utility to generate the stanza id
- [ ] Develop a high-level interface (EPIC)
- [ ] Rewrite the tests using mocks
- [ ] Add more comments and code documentation
- [ ] Think about adding hooks for the basic actions like: connecting, disconnecting, authenticating, etc.
It could be represented as a set of well-defined wrappers over the `xmpp%:handle-stanza` method.
There are some number of approaches to managable, user-defined, flexible hook systems:
global hooks, e.g. `(add-hook 'some-hook #'(lambda () ...))`; per-session hooks.
- [ ] Improve security (SSL, TLS)
    - [ ] Add a support for the modern cryptography mechanisms. There are limitations in cl+ssl library.
- [ ] Develop a small tool for getting shell over xmpp.
- [ ] Develop a simple MUC bot based on 'Markov chains' as an yet another example
