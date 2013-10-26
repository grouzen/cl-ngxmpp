cl-ngxmpp
=========

Common Lisp library for XMPP RFCs.

WARNING: This library is under heavy development.
There is CL-NGXMPP-CLIENT system which is the more abstract, and contains 
methods like: send message, send presence, receive stanza, etc. So, most
of time you must use CL-NGXMPP-CLIENT.

Examples:
=========

You can find examples in examples/ directory.  
First you need to load examples system:  
`(ql:quickload :cl-ngxmpp-client.examples)`.  
There is echo-bot.lisp example, to run it print:   
`(cl-ngxmpp-client.examples.echo-bot:run  
    :server-hostname "<your jabber server>"  
    :username        "<your jabber login>"  
    :password        "<your jabber password>"  
    :to              "<opponent's jid>"  
    :body            "<body of your message>")`  
    
After that bot will send the message, and will wait for messages in infinite loop.  
