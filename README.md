cl-ngxmpp
================================================================================

[![Build Status](https://travis-ci.org/grouzen/cl-ngxmpp.svg?branch=development)](https://travis-ci.org/grouzen/cl-ngxmpp)

Common Lisp library for XMPP RFCs.

WARNING: This library is under heavy development.  
There is CL-NGXMPP-CLIENT system which is the more abstract, and contains 
methods like: send message, send presence, receive stanza, etc.  
Also inside CL-NGXMPP-CLIENT system there is high level api for roster, 
presence, and so on.  
So, most of the time you should use CL-NGXMPP-CLIENT.

TODO:
================================================================================

- [X] Migrate from cl-async to blackbird library
- [X] Fix Travis-CI build
- [ ] Develop a high-level interface
- [X] Code DSL to have more concise way to define stanzas
- [ ] Debug levels, and generic `print-debug` function
- [ ] Use short package nicknames instead of long names
- [ ] Write more descriptive README
- [ ] Re-think and (it would be better) rewrite the code in `client/xeps/xeps.lisp`
- [ ] Develop a simple bot for conferences based on 'Markov chains' as another example
- [ ] Rewrite tests for client part in such way so that there are no dependencies from external services

Examples:
================================================================================

You can find examples in examples/ directory.  
First you need to load examples system:  
```commonlisp
(ql:quickload :cl-ngxmpp-client.examples)
```  
There is echo-bot.lisp example, to run it type:   
```commonlisp
(cl-ngxmpp-client.examples.echo-bot:run  
    :server-hostname "<your jabber server>"  
    :username        "<your jabber login>"  
    :password        "<your jabber password>"  
    :to              "<opponent's jid>"  
    :body            "<body of your message>")  
```  
    
After that bot will send the message, and will wait for messages in infinite loop.  
